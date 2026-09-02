package plasmon.command

import caseapp.core.RemainingArgs
import com.github.plokhotnyuk.jsoniter_scala.core.writeToString
import com.google.gson.JsonParser
import plasmon.protocol.{Command as ProtocolCommand, RemainingArgsJson}
import plasmon.servercommand.ServerCommand

/** A server command as the client sees it: parsed here, run there.
  *
  * Everything that can be answered without a server is answered here - `--help`, an unknown option,
  * a missing argument - and only a command that was understood is sent on. What goes over the wire
  * is then the command that was meant rather than the words that were typed: its options as JSON,
  * and whatever arguments case-app didn't take.
  */
final class RemoteCommand[T](
  serverCommand: ServerCommand[T],
  connection: CommandOptions
) extends caseapp.Command[T]()(using serverCommand.parser, serverCommand.help) {

  override def names = serverCommand.names

  def run(options: T, remainingArgs: RemainingArgs): Unit = {
    val request = new ProtocolCommand
    request.setName(names.head.toArray)
    request.setOptions {
      JsonParser.parseString(writeToString(options)(using serverCommand.codec))
    }
    request.setRemainingArgs {
      JsonParser.parseString {
        writeToString(RemainingArgsJson.of(remainingArgs))(using RemainingArgsJson.codec)
      }
    }
    Command.send(connection, request, AutoServer.Auto.of(options))
  }
}
