package plasmon.command

import org.eclipse.lsp4j as l
import plasmon.languageclient.{PlasmonLanguageClient, PlasmonNoopLanguageClient}

import java.util.List as JList

import scala.collection.mutable
import scala.jdk.CollectionConverters.*
import scala.util.Try

/** What the page of `plasmon browse` shows of what's going on, in its status bar and log panel.
  *
  * That is
  *   - what `plasmon browse` itself is doing (loading what a file needs, hovers, …, see
  *     [[BrowseLsp]]), with its own log,
  *   - what the plasmon server says through [[client]], the language client it sends what it'd send
  *     an editor: its status items for the focused file, the requests of build tools in progress,
  *     and its logs, one per channel, like the output channels of VS Code,
  *   - the log of the server itself (scribe's, see [[serverLog]]).
  *
  * The page long-polls it, see [[await]].
  */
private[command] final class BrowseStatus(root: os.Path = os.root) {
  import BrowseStatus.*

  private var seq       = 0L
  private var active    = 0
  private var text      = "Ready"
  private var error     = false
  private var idleText  = "Ready"
  private var idleError = false

  // Log channels, in the order they showed up in
  private val channels = mutable.LinkedHashMap(
    BrowseChannel -> new Channel("Browse", maxBrowseLines),
    ServerChannel -> new Channel("Server", maxLines)
  )

  // The status items of the focused file, and that file
  private var statusFile  = Option.empty[String]
  private var statusItems = Seq.empty[PlasmonLanguageClient.StatusUpdate]

  // Build tool requests in progress, by build tool and request ids
  private val tasks = mutable.LinkedHashMap.empty[(String, String), String]

  private def changed(): Unit = {
    seq += 1
    notifyAll()
  }

  def busy(description: String): Unit = synchronized {
    active += 1
    text = description + "…"
    error = false
    addLine(BrowseChannel, s"$description…")
    changed()
  }

  /** A line for the log, and for the status bar too with `inStatusBar` */
  def line(line: String, inStatusBar: Boolean = true): Unit = synchronized {
    addLine(BrowseChannel, line)
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
    addLine(BrowseChannel, description)
    if (active == 0) {
      text = description
      this.error = error
    }
    changed()
  }

  /** Lines of a log channel, that gets created if needed */
  def log(channelId: String, label: String, lines: Seq[String]): Unit = synchronized {
    val channel = channels.getOrElseUpdate(channelId, new Channel(label, maxLines))
    for (line <- lines)
      channel.add(line)
    changed()
  }

  /** A line of the log of the server itself */
  def serverLog(line: String): Unit =
    log(ServerChannel, "Server", Seq(line))

  /** The status items the server sends for its focused file */
  def statusUpdate(uri: String, items: Seq[PlasmonLanguageClient.StatusUpdate]): Unit =
    synchronized {
      statusFile =
        if (uri.isEmpty) None
        else Some(relativePath(uri))
      statusItems = items
      changed()
    }

  def progress(details: PlasmonLanguageClient.ProgressDetails): Unit = synchronized {
    val key = (details.buildToolId, details.requestId)
    if (details.done == java.lang.Boolean.TRUE) tasks.remove(key)
    else if (!tasks.contains(key)) tasks(key) = s"${details.buildToolName}: ${details.request}"
    changed()
  }

  /** The language client the plasmon server sends what it'd send an editor */
  val client: PlasmonLanguageClient = new PlasmonNoopLanguageClient {
    override def statusUpdate(uri: String, updates: JList[PlasmonLanguageClient.StatusUpdate])
      : Unit =
      BrowseStatus.this.statusUpdate(uri, Option(updates).fold(Nil)(_.asScala.toSeq))
    override def log(message: PlasmonLanguageClient.LogMessage): Unit =
      BrowseStatus.this.log(
        message.channelId,
        message.channelLabel,
        Option(message.lines).fold(Nil)(_.asScala.toSeq)
      )
    override def progress(details: PlasmonLanguageClient.ProgressDetails): Unit =
      BrowseStatus.this.progress(details)
    override def showMessage(params: l.MessageParams): Unit =
      message(params)
    override def logMessage(params: l.MessageParams): Unit =
      message(params)
    private def message(params: l.MessageParams): Unit = {
      val level = Option(params.getType).fold("info")(_.toString.toLowerCase(java.util.Locale.ROOT))
      BrowseStatus.this.log("messages", "Messages", Seq(s"[$level] ${params.getMessage}"))
    }
    override def toString = "plasmon browse"
  }

  /** The `file://` URI of a file under `root` as a path relative to it, the URI otherwise */
  private def relativePath(uri: String): String =
    Try(os.Path(java.nio.file.Path.of(new java.net.URI(uri))))
      .toOption
      .filter(_.startsWith(root))
      .map(_.relativeTo(root).toString)
      .getOrElse(uri)

  private def addLine(channelId: String, line: String): Unit =
    channels(channelId).add(line)

  /** The status, once it changed from `since` (or after `timeoutMs`), with the lines of the log
    * channel `log` (if any) from `fromLine` on.
    */
  def await(
    since: Long,
    fromLine: Long,
    timeoutMs: Long,
    log: Option[String] = Some(BrowseChannel)
  ): ujson.Obj = synchronized {
    val deadline = System.currentTimeMillis() + timeoutMs
    while (seq <= since && System.currentTimeMillis() < deadline)
      wait(math.max(1L, deadline - System.currentTimeMillis()))
    val res = ujson.Obj(
      "seq"   -> seq.toDouble,
      "busy"  -> (active > 0),
      "text"  -> text,
      "error" -> error,
      "file"  -> statusFile.fold[ujson.Value](ujson.Null)(ujson.Str(_)),
      "items" -> ujson.Arr.from(statusItems.map(itemJson)),
      "tasks" -> ujson.Arr.from(tasks.valuesIterator.map(ujson.Str(_))),
      "channels" -> ujson.Arr.from {
        channels.iterator.map {
          case (id, channel) =>
            ujson.Obj("id" -> id, "label" -> channel.label, "end" -> channel.end.toDouble)
        }
      }
    )
    for (channelId <- log; channel <- channels.get(channelId)) {
      val from = math.max(fromLine, channel.firstLine)
      res("log") = channelId
      res("fromLine") = from.toDouble
      res("lines") = ujson.Arr.from(channel.from(from).map(ujson.Str(_)))
    }
    res
  }
}

private[command] object BrowseStatus {

  /** The log of what `plasmon browse` does */
  val BrowseChannel = "browse"

  /** The log of the server itself */
  val ServerChannel = "server"

  private val maxBrowseLines = 1000
  private val maxLines       = 5000

  /** The last lines of a log, numbered from its first one */
  private final class Channel(val label: String, maxLines: Int) {
    private val lines = mutable.ArrayDeque.empty[String]
    var firstLine     = 0L
    def end: Long     = firstLine + lines.length
    def add(line: String): Unit = {
      lines.append(line)
      if (lines.length > maxLines) {
        lines.removeHead()
        firstLine += 1
      }
    }
    def from(line: Long): Iterator[String] =
      lines.iterator.drop((line - firstLine).toInt)
  }

  /** A status item, as the page shows it: what clicking it does is showing a log, if anything */
  private def itemJson(item: PlasmonLanguageClient.StatusUpdate): ujson.Obj = {
    val obj = ujson.Obj(
      "id"       -> item.id,
      "text"     -> Option(item.text).getOrElse(""),
      "severity" -> Option(item.severity).fold(0)(_.intValue),
      "busy"     -> item.isBusy
    )
    for (detail <- Option(item.detail) if detail.nonEmpty)
      obj("detail") = detail
    for {
      command <- Option(item.command)
      if command.command == "plasmon.show-log"
      args  <- Option(command.arguments)
      logId <- args.asScala.headOption
    }
      obj("log") = logId
    obj
  }
}
