package plasmon.servercommand

import caseapp.core.help.Help
import caseapp.core.parser.Parser
import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import plasmon.Server
import plasmon.command.ServerCommandThreadPools
import plasmon.index.Indexer
import plasmon.protocol.CommandClient

/** A command that runs in the server.
  *
  * Both sides of the wire are built from this: the client turns it into a
  * [[plasmon.command.RemoteCommand]], which parses `T` out of the command line and sends it as
  * JSON, and the server hands that JSON to [[instance]], which is where the command actually runs.
  * The parser, the help and the codec live here so that the two agree on what a command is called,
  * what it accepts, and how its options are written down.
  */
abstract class ServerCommand[T](implicit
  val parser: Parser[T],
  val help: Help[T],
  val codec: JsonValueCodec[T]
) {
  def names: List[List[String]] =
    List(List(help.progName))
  def instance(
    server: Server,
    indexer: Indexer,
    client: CommandClient,
    lspServer: plasmon.jsonrpc.JsonrpcServer,
    pool: ServerCommandThreadPools
  ): ServerCommandInstance[T]
}
