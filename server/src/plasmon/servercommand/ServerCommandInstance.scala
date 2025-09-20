package plasmon.servercommand

import caseapp.core.app.Command
import caseapp.core.help.Help
import caseapp.core.parser.Parser
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
}

object ServerCommandInstance {
  final class Exit(val code: Int) extends Exception(s"Command exited with code $code")
  def exit(code: Int): Nothing =
    throw new Exit(code)
}
