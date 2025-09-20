package plasmon.servercommand

import caseapp.core.RemainingArgs
import plasmon.command.ServerCommandThreadPools
import plasmon.protocol.CommandClient
import plasmon.index.Indexer
import plasmon.Server

final case class Reload(
  server: Server,
  client: CommandClient
) extends ServerCommandInstance[ReloadOptions](client) {
  override def run(options: ReloadOptions, remainingArgs: RemainingArgs): Unit = {
    ???
  }
}

object Reload extends ServerCommand[ReloadOptions] {
  override def instance(
    server: Server,
    indexer: Indexer,
    client: CommandClient,
    lspServer: plasmon.jsonrpc.JsonrpcServer,
    pool: ServerCommandThreadPools
  ): ServerCommandInstance[ReloadOptions] =
    Reload(server, client)
}
