package plasmon.servercommand

import caseapp.core.RemainingArgs
import caseapp.core.app.Command
import caseapp.core.help.Help
import caseapp.core.parser.Parser
import com.github.plokhotnyuk.jsoniter_scala.core.{JsonValueCodec, readFromString}
import com.google.gson.Gson
import plasmon.Logger
import plasmon.protocol.CommandClient
import plasmon.protocol.CommandClient.ops.*

abstract class ServerCommandInstance[T](client: CommandClient)(implicit
  parser: Parser[T],
  help: Help[T],
  codec: JsonValueCodec[T]
) extends Command[T] {
  override def exit(code: Int): Nothing =
    ServerCommandInstance.exit(code)
  override def printLine(line: String, toStderr: Boolean): Unit =
    client.printLine(line, toStderr)

  /** Runs the command on the options the client parsed for it.
    *
    * The command line was already made sense of on the other side, so nothing here parses anything
    *   - this is [[run]] with its arguments read back out of JSON.
    */
  final def runFromJson(optionsJson: String, remainingArgs: RemainingArgs): Unit =
    run(readFromString(optionsJson)(using codec), remainingArgs)

  /** Prints an LSP response as JSON, on stdout.
    *
    * Deliberately a plain [[Gson]], the way an LSP message would be serialized: callers - the
    * integration tests among them - can read the output back into the very same lsp4j classes.
    */
  protected def printJson(value: Object): Unit =
    printLine(new Gson().toJson(value))

  /** A logger for the LSP request a command runs on behalf of its user.
    *
    * Its logs go to stderr, out of the way of the result on stdout (plain text or JSON), and
    * nowhere at all with `quiet`.
    */
  protected def requestLogger(quiet: Boolean): Logger =
    Logger.Manager
      .create { channel => msg =>
        if (!quiet)
          printLine(s"[${channel.label}] $msg", toStderr = true)
      }
      .create("request", "Request")

  /** Exits with an error unless there is something to look at in `file`.
    *
    * Language features answer for a file's content: the one the editor sent (`lsp did-open`), or
    * else the one on disk. With neither - a mistyped path, say - the request handlers have nothing
    * to say, and report that the way an editor expects, with an empty response (and, for a file
    * that belongs to no module, a log line at most). From a terminal, that reads as a success.
    */
  protected def ensureFileContent(server: plasmon.Server, file: os.Path): Unit =
    if (!server.editorState.buffers.contains(file) && !os.isFile(file))
      ServerCommandInstance.usageError(
        if (os.exists(file)) s"$file: not a file"
        else s"$file: no such file"
      )
}

object ServerCommandInstance {
  final class Exit(val code: Int) extends Exception(s"Command exited with code $code")

  /** A command can't run because of how it was called (a missing argument, a file that isn't
    * there…): the command client gets the message alone, no stack trace.
    */
  final class UsageError(message: String) extends Exception(message)
  def usageError(message: String): Nothing =
    throw new UsageError(message)
  def exit(code: Int): Nothing =
    throw new Exit(code)
}
