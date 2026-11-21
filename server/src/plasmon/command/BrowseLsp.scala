package plasmon.command

import com.github.plokhotnyuk.jsoniter_scala.core.writeToString
import com.google.gson.JsonParser
import plasmon.ide.Directories
import plasmon.protocol.{Command as ProtocolCommand, CommandClient, PrintData, RemainingArgsJson}
import plasmon.servercommand.*

import java.util.concurrent.{CompletableFuture, ConcurrentHashMap, Executors}
import java.util.concurrent.atomic.AtomicLong

import scala.collection.mutable
import scala.concurrent.{Await, Promise}
import scala.concurrent.duration.Duration
import scala.util.control.NonFatal

/** What `plasmon browse` asks a plasmon server: loading what a file needs, hovers, definitions.
  *
  * It goes through the same commands as the command line (`plasmon lsp … --auto`), run against the
  * server running in this process (see [[Browse.serverOptions]]). Loads run one at a time, and the
  * other requests on a file wait for its load, then run alongside each other. What commands print
  * on stderr (what is being loaded, the logs of the requests) goes to [[status]], that the page
  * shows.
  */
private[command] final class BrowseLsp(root: os.Path, inProcess: Server.InProcess) {
  import BrowseLsp.*

  val status = new Status

  // Whether loading what each file needs went fine, once it's done
  private val loads = new ConcurrentHashMap[os.Path, Promise[Boolean]]
  private val pool = Executors.newCachedThreadPool { r =>
    val t = new Thread(r, "plasmon-browse-load")
    t.setDaemon(true)
    t
  }
  private val loadLock = new Object

  /** Loads a build tool and a module for `file` in the background, unless that was done already. */
  def load(file: os.Path): Unit =
    loadPromise(file)

  private def loadPromise(file: os.Path): Promise[Boolean] = {
    val promise  = Promise[Boolean]()
    val existing = loads.putIfAbsent(file, promise)
    if (existing != null) existing
    else {
      pool.execute { () =>
        val name = display(file)
        val res = loadLock.synchronized {
          run(
            LspCodeLens,
            LspCodeLensOptions(auto = true, json = true),
            Seq(file.toString),
            s"Loading build tool and module for $name",
            {
              case Right((_, lines)) =>
                val module = lines.reverseIterator.collectFirst {
                  case LoadedModule(m) => m
                }
                status.idle(
                  if (lines.exists(_.endsWith(DependencySourceSuffix)))
                    s"$name: dependency source"
                  else
                    module.filter(_ != name)
                      .fold(s"$name: ready")(m => s"$name: ready (module $m)")
                )
              case Left(err) =>
                status.idle(s"$name: $err", error = true)
            }
          )
        }
        promise.success(res.isRight)
      }
      promise
    }
  }

  /** Waits for what `file` needs to be loaded, loading it if needed */
  private def awaitLoad(file: os.Path): Unit =
    Await.ready(loadPromise(file).future, Duration.Inf)

  // Hovers asked while waiting for a load are superseded by the ones that came after them: by the
  // time it's done, users are looking at the latest one
  private val hoverCount = new AtomicLong

  /** The LSP hover at a position, as JSON (`null` if there's none). `None` if a more recent hover
    * superseded it.
    */
  def hover(file: os.Path, line: Int, col: Int): Option[Either[String, ujson.Value]] = {
    val count = hoverCount.incrementAndGet()
    awaitLoad(file)
    Option.when(hoverCount.get() == count) {
      run(
        LspHover,
        LspHoverOptions(line = line, col = col, json = true),
        Seq(file.toString),
        "Hover",
        inStatusBar = false
      ).map(_._1)
    }
  }

  /** The LSP locations of the definitions at a position, as JSON */
  def definition(file: os.Path, line: Int, col: Int): Either[String, ujson.Value] = {
    awaitLoad(file)
    run(
      LspDefinition,
      LspDefinitionOptions(line = line, col = col, json = true),
      Seq(file.toString),
      "Definition",
      inStatusBar = false
    ).map(_._1)
  }

  /** The diagnostics of the presentation compiler for a file, as JSON (LSP diagnostics) */
  def diagnostics(file: os.Path): Either[String, ujson.Value] = {
    awaitLoad(file)
    run(
      LspDiagnostics,
      LspDiagnosticsOptions(json = true),
      Seq(file.toString),
      s"Diagnostics of ${display(file)}"
    ).map(_._1)
  }

  /** Runs a command, returns what it printed on stdout as JSON, and its lines on stderr.
    *
    * `Left` holds what went wrong, for the status bar.
    */
  private def run[T](
    command: ServerCommand[T],
    options: T,
    args: Seq[String],
    description: String,
    after: Either[String, (ujson.Value, Seq[String])] => Unit = _ => (),
    inStatusBar: Boolean = true
  ): Either[String, (ujson.Value, Seq[String])] = {
    if (inStatusBar) status.busy(description)
    else status.line(s"$description…", inStatusBar = false)
    val stdout = new StringBuilder
    val stderr = mutable.ListBuffer.empty[String]
    def log(line: String): Unit = {
      val line0 = shorten(line)
      stderr += line0
      status.line(line0, inStatusBar)
    }
    val client: CommandClient = new CommandClient {
      def print(data: PrintData): CompletableFuture[Unit] = {
        if (data.getLine != null)
          if (data.getIsStderr) log(data.getLine)
          else stdout.append(data.getLine).append('\n')
        CompletableFuture.completedFuture(())
      }
    }
    val request = new ProtocolCommand
    request.setName(command.names.head.toArray)
    request.setOptions(JsonParser.parseString(writeToString(options)(using command.codec)))
    request.setRemainingArgs {
      val remaining = args.toList.zipWithIndex.map {
        case (arg, idx) => RemainingArgsJson.Arg(idx, 1, arg)
      }
      JsonParser.parseString(
        writeToString(RemainingArgsJson(remaining = remaining))(using RemainingArgsJson.codec)
      )
    }
    val res =
      try {
        val exitCode = inProcess.runCommand(request, client)
        if (exitCode == 0)
          Right((ujson.read(stdout.result().trim.nonEmptyOr("null")), stderr.toList))
        else
          Left(stderr.lastOption.getOrElse(s"$description failed (exit code $exitCode)"))
      }
      catch {
        case NonFatal(e) =>
          scribe.warn(s"Error running ${command.names.head.mkString(" ")}", e)
          Left(e.toString)
      }
    after(res)
    if (inStatusBar) status.done()
    res
  }

  // Paths under root relative to it, and the sources of dependencies relative to where they're
  // extracted, in what goes to the status bar
  private val prefixes = {
    val roots = Seq(s"file://$root/", s"$root/")
    roots.map(_ + Directories.dependencies.toString + "/") ++ roots
  }
  private def shorten(line: String): String =
    prefixes.foldLeft(line)(_.replace(_, ""))
  private def display(file: os.Path): String =
    shorten(file.toString)

}

private[command] object BrowseLsp {

  private val LoadedModule = "Loaded module (.*)".r

  // What AutoLoad says of dependency sources
  private val DependencySourceSuffix = " is a dependency source, nothing to load for it"

  extension (s: String) {
    private def nonEmptyOr(other: String): String =
      if (s.isEmpty) other else s
  }

  /** What the page shows in its status bar: what's going on, and the lines that led to it.
    *
    * The page long-polls it, see [[await]].
    */
  final class Status {
    private var seq       = 0L
    private var active    = 0
    private var text      = "Ready"
    private var error     = false
    private var idleText  = "Ready"
    private var idleError = false
    private var firstLine = 0L
    private val lines     = mutable.ArrayDeque.empty[String]
    private val maxLines  = 1000

    private def changed(): Unit = {
      seq += 1
      notifyAll()
    }

    def busy(description: String): Unit = synchronized {
      active += 1
      text = description + "…"
      error = false
      addLine(s"$description…")
      changed()
    }

    /** A line for the log, and for the status bar too with `inStatusBar` */
    def line(line: String, inStatusBar: Boolean = true): Unit = synchronized {
      addLine(line)
      // The logs of the request itself are too chatty for the status bar, they're in the log
      if (inStatusBar && active > 0 && !line.startsWith("["))
        text = line
      changed()
    }

    def done(): Unit = synchronized {
      active = math.max(0, active - 1)
      if (active == 0) {
        text = idleText
        error = idleError
      }
      changed()
    }

    /** What to show when nothing is going on, from now on */
    def idle(description: String, error: Boolean = false): Unit = synchronized {
      idleText = description
      idleError = error
      addLine(description)
      if (active == 0) {
        text = description
        this.error = error
      }
      changed()
    }

    private def addLine(line: String): Unit = {
      lines.append(line)
      if (lines.length > maxLines) {
        lines.removeHead()
        firstLine += 1
      }
    }

    /** The status, once it changed from `since` (or after `timeoutMs`), with the log lines from
      * `fromLine` on.
      */
    def await(since: Long, fromLine: Long, timeoutMs: Long): ujson.Obj = synchronized {
      val deadline = System.currentTimeMillis() + timeoutMs
      while (seq <= since && System.currentTimeMillis() < deadline)
        wait(math.max(1L, deadline - System.currentTimeMillis()))
      val from = math.max(fromLine, firstLine)
      ujson.Obj(
        "seq"      -> seq.toDouble,
        "busy"     -> (active > 0),
        "text"     -> text,
        "error"    -> error,
        "fromLine" -> from.toDouble,
        "lines" -> ujson.Arr.from(lines.iterator.drop((from - firstLine).toInt).map(ujson.Str(_)))
      )
    }
  }
}
