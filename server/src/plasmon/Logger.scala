package plasmon

import java.io.ByteArrayOutputStream
import java.io.PrintStream
import java.nio.charset.StandardCharsets
import java.time.Instant
import java.time.temporal.ChronoUnit
import java.util.concurrent.atomic.AtomicInteger
import scala.concurrent.duration.Duration
import java.util.concurrent.TimeUnit

trait Logger {
  def channel: Logger.Channel
  def log(line: String): Unit
  final def log(ex: Throwable): Unit = {
    val baos = new ByteArrayOutputStream
    ex.printStackTrace(new PrintStream(baos, true, StandardCharsets.UTF_8))
    log(new String(baos.toByteArray, StandardCharsets.UTF_8))
  }

  def pprint[T](x: sourcecode.Text[T])(implicit
    line: sourcecode.Line,
    fileName: sourcecode.FileName
  ): Unit = {

    val prefix = Seq(
      fansi.Color.Magenta(fileName.value),
      fansi.Str(":"),
      fansi.Color.Green(line.value.toString),
      fansi.Str(" "),
      fansi.Color.Cyan(x.source),
      fansi.Str(": ")
    )

    val str = fansi.Str.join(
      prefix ++
        _root_.pprint.tokenize(
          x.value,
          _root_.pprint.defaultWidth,
          _root_.pprint.defaultHeight,
          0,
          escapeUnicode = _root_.pprint.defaultEscapeUnicode,
          showFieldNames = _root_.pprint.defaultShowFieldNames
        ).toSeq
    )

    log(str.render)
  }

  private val timedCount = new AtomicInteger

  final def timed[T](message: String)(f: => T): T = {
    val indentationSize    = timedCount.getAndIncrement
    val messageIndentation = "  " * indentationSize
    val indentation        = messageIndentation + "  "
    log(messageIndentation + message)
    try {
      val startTime = Instant.now()
      val res =
        try Right(f)
        catch {
          case ex: Throwable =>
            Left(ex)
        }
      val endTime = Instant.now()
      log(s"${indentation}duration: ${Logger.printableDuration(startTime, endTime)}")
      res match {
        case Right(t) => t
        case Left(ex) =>
          log(s"${indentation}failed: $ex")
          log("")
          log(ex)
          log("")
          throw ex
      }
    }
    finally
      timedCount.decrementAndGet()
  }

  final def processOutput =
    os.ProcessOutput.Readlines(log(_))
  final def consumer: java.util.function.Consumer[String] =
    line => log(line)

  final def logCommand(proc: os.proc): proc.type = {
    log("Running")
    log("")
    log(proc.command.flatMap(_.value).mkString("  ", " ", ""))
    log("")
    proc
  }

  final def addPrefix(prefix: String): Logger =
    new Logger.Prefixed(prefix, this)
}

object Logger {
  final case class Channel(id: String, label: String)

  trait Manager {
    def create(id: String, label: String): Logger
  }

  object Manager {

    private final class Impl(doLog: Channel => String => Unit) extends Manager {
      def create(id: String, label: String): Logger = {
        val channel0 = Channel(id, label)
        val f        = doLog(channel0)
        new Logger {
          def channel           = channel0
          def log(line: String) = f(line)
        }
      }
    }

    def create(doLog: Channel => String => Unit): Manager =
      new Impl(doLog)
  }

  private def printableDuration(start: Instant, end: Instant): String = {
    val (unit, duration) =
      Iterator(ChronoUnit.SECONDS, ChronoUnit.MILLIS, ChronoUnit.MICROS, ChronoUnit.NANOS)
        .map(unit => (unit, unit.between(start, end)))
        .filter(_._2 >= 5)
        .find(_ => true)
        .getOrElse {
          (ChronoUnit.NANOS, ChronoUnit.NANOS.between(start, end))
        }
    Duration.apply(duration, TimeUnit.of(unit)).toString
  }

  private class Prefixed(prefix: String, underlying: Logger) extends Logger {
    def channel: Channel = underlying.channel
    def log(line: String): Unit =
      line
        .linesIterator
        .map(prefix + _)
        .foreach(underlying.log(_))
  }
}
