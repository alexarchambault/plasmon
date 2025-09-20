package plasmon.servercommand

import caseapp.core.help.Help
import plasmon.command.ServerCommandThreadPools
import plasmon.protocol.CommandClient
import plasmon.index.Indexer
import plasmon.Server

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
