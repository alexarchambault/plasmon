package plasmon.integration

import io.github.alexarchambault.testutil.TestOutput.FixedReadBytes
import io.github.alexarchambault.testutil.TestUtil.*
import io.github.alexarchambault.testutil.{OutputFrame, TestOutput}

import java.io.{FileOutputStream, OutputStream, PrintStream}
import java.nio.charset.StandardCharsets
import java.util.concurrent.atomic.AtomicInteger

import scala.concurrent.duration.{DurationInt, FiniteDuration}
import scala.util.control.NonFatal

/** Per-test log files for integration tests.
  *
  * When [[baseDir]] is set (Mill always sets it), each test writes to a temp file under `running/`.
  * The file is deleted on success and moved to `failed/` on failure. CI prints the first few files
  * from `failed/` at the end of the job.
  */
object TestLogs {

  private val counter = new AtomicInteger(0)

  /** Directory passed from Mill as `plasmon.integration.test-logs-dir`. */
  lazy val baseDir: Option[os.Path] =
    sys.props.get("plasmon.integration.test-logs-dir").map(os.Path(_, os.pwd))

  private def runningDir: Option[os.Path] = baseDir.map(_ / "running")
  private def failedDir: Option[os.Path]  = baseDir.map(_ / "failed")

  /** On CI, send System.out/err to the log file so parallel tests don't interleave in the job log.
    */
  def quietConsole: Boolean = System.getenv("CI") != null

  @volatile private var current0: Option[TestLog] = None

  def current: Option[TestLog] = current0

  def currentStream: Option[OutputStream] = current0.map(_.stream)

  def printStream(fallback: PrintStream): PrintStream =
    currentStream match {
      case Some(os) =>
        new PrintStream(new TeeOutputStream(fallback, os), true, StandardCharsets.UTF_8)
      case None =>
        fallback
    }

  def outputStream(fallback: Option[OutputStream]): Option[OutputStream] =
    (fallback, currentStream) match {
      case (Some(a), Some(b)) => Some(new TeeOutputStream(a, b))
      case (a, b)             => a.orElse(b)
    }

  def capturing[T](className: String, testName: String)(body: => T): T = {
    val destDirs = for {
      running <- runningDir
      failed  <- failedDir
    } yield (running, failed)
    destDirs match {
      case None =>
        body
      case Some((running, failed)) =>
        os.makeDir.all(running)
        os.makeDir.all(failed)

        val fileName = logFileName(className, testName)
        val inflight = running / fileName
        val fos      = new FileOutputStream(inflight.toIO, true)
        val ps       = new PrintStream(fos, true, StandardCharsets.UTF_8)
        val log      = TestLog(ps)

        val prev                 = current0
        val (savedOut, savedErr) = (System.out, System.err)
        current0 = Some(log)
        if (quietConsole) {
          System.setOut(ps)
          System.setErr(ps)
        }

        ps.println(s"# test: $className.$testName")
        ps.flush()

        var success = false
        try {
          val res = body
          success = true
          res
        }
        catch {
          case e: Throwable =>
            e.printStackTrace(ps)
            throw e
        }
        finally {
          if (quietConsole) {
            System.setOut(savedOut)
            System.setErr(savedErr)
          }
          current0 = prev
          ps.flush()
          ps.close()

          if (success)
            try os.remove(inflight)
            catch {
              case NonFatal(e) =>
                savedErr.println(s"Ignoring failure deleting successful test log $inflight: $e")
            }
          else {
            val dest = failed / fileName
            try {
              os.move(inflight, dest, replaceExisting = true, createFolders = true)
              savedErr.println(s"Kept failing test log: $dest")
            }
            catch {
              case NonFatal(e) =>
                savedErr.println(s"Failed to keep test log $inflight: $e")
            }
          }
        }
    }
  }

  private def logFileName(className: String, testName: String): String = {
    val shortClass = className.split('.').lastOption.getOrElse(className)
    val raw        = s"$shortClass-$testName"
    val sanitized = raw.map {
      case c if c.isLetterOrDigit || c == '.' || c == '-' || c == '_' => c
      case _                                                          => '-'
    }.replaceAll("-{2,}", "-")
      .take(120)
      .stripPrefix("-")
      .stripSuffix("-")
    val seq   = counter.incrementAndGet()
    val stamp = System.currentTimeMillis()
    val pid =
      try ProcessHandle.current().pid()
      catch {
        case _: Throwable => 0L
      }
    s"$stamp-$pid-$seq-$sanitized.log"
  }

  final case class TestLog(
    stream: PrintStream
  ) {
    def println(msg: String): Unit = {
      stream.println(msg)
      stream.flush()
    }
  }
}

/** Writes to every underlying stream. Does not close them. */
final class TeeOutputStream(streams: OutputStream*) extends OutputStream {
  override def write(b: Int): Unit =
    synchronized {
      streams.foreach(_.write(b))
    }
  override def write(b: Array[Byte], off: Int, len: Int): Unit =
    synchronized {
      streams.foreach(_.write(b, off, len))
    }
  override def flush(): Unit =
    synchronized {
      streams.foreach(_.flush())
    }
}

/** Like `io.github.alexarchambault.testutil.ProcessTest`, but tees process output to
  * [[TestLogs.currentStream]] when a per-test log file is active.
  */
object PlasmonProcessTest {
  def apply[T](
    proc: os.proc,
    timeout: Option[FiniteDuration] = Some(1.minute),
    count: Int = 1,
    env: Map[String, String] = Map.empty,
    runProcIn: os.Path => os.Path = identity,
    enableOutputFrame: Boolean = true,
    enableSilentOutput: Boolean = true,
    printOutputOnError: Boolean = true,
    cleanUp: Boolean = true,
    newOutputFrame: () => OutputFrame = () => new OutputFrame(),
    extraOutput: Option[OutputStream] = TestLogs.currentStream
  )(
    content: (os.SubPath, os.Source)*
  )(f: (os.Path, os.SubProcess, () => Unit, TestOutput, Int) => T): T = {

    val output = new TestOutput(
      enableOutputFrame,
      enableSilentOutput,
      newOutputFrame = newOutputFrame
    )

    val processOutput: os.ProcessOutput =
      (output.outputStreamOpt, extraOutput) match {
        case (Some(a), Some(b)) => FixedReadBytes.pipeTo(new TeeOutputStream(a, b))
        case (Some(a), None)    => FixedReadBytes.pipeTo(a)
        case (None, Some(b))    => FixedReadBytes.pipeTo(b)
        case (None, None)       => os.Inherit
      }

    val errorOutput: PrintStream = TestLogs.printStream(output.printStream)

    var success = false
    output.start()
    try
      os.temp.withContent(content, cleanUp, errorOutput = errorOutput) { tmpDir =>
        os.makeDir.all(tmpDir)

        def run(runCount: Int): T =
          proc.withSubProcess(
            cwd = runProcIn(tmpDir),
            env = env,
            timeout = timeout.map(_ * 2),
            stderr = processOutput,
            errorOutput = errorOutput
          ) { (subProc, ignoreSubprocExit) =>
            runWithTimeout(timeout) {
              f(tmpDir, subProc, ignoreSubprocExit, output, runCount)
            }
          }

        for (i <- 0 until (count - 1))
          run(i)
        val value = run(count - 1)
        success = true
        value
      }
    finally
      // Don't dump to the console when a per-test log file already has the output.
      output.close(success, printOutputOnError && extraOutput.isEmpty)
  }
}
