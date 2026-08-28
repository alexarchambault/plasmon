package plasmon.servercommand

import caseapp.core.app.Command
import caseapp.core.help.Help
import caseapp.core.parser.Parser
import com.google.gson.Gson
import plasmon.protocol.CommandClient
import plasmon.protocol.CommandClient.ops.*

abstract class ServerCommandInstance[T](client: CommandClient)(implicit
  parser: Parser[T],
  help: Help[T]
) extends Command[T] {
  override def exit(code: Int): Nothing =
    ServerCommandInstance.exit(code)
  override def printLine(line: String, toStderr: Boolean): Unit =
    client.printLine(line, toStderr)

  /** Prints an LSP response as JSON, on stdout.
    *
    * Deliberately a plain [[Gson]], the way an LSP message would be serialized: callers - the
    * integration tests among them - can read the output back into the very same lsp4j classes.
    */
  protected def printJson(value: Object): Unit =
    printLine(new Gson().toJson(value))
}

object ServerCommandInstance {
  final class Exit(val code: Int) extends Exception(s"Command exited with code $code")
  def exit(code: Int): Nothing =
    throw new Exit(code)
}
