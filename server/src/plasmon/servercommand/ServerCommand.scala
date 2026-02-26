package plasmon.servercommand

import caseapp.core.help.Help
import plasmon.Server
import plasmon.command.ServerCommandThreadPools
import plasmon.index.Indexer
import plasmon.protocol.CommandClient

abstract class ServerCommand[T](implicit val help: Help[T]) {
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
