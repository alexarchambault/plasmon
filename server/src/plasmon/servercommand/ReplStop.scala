package plasmon.servercommand

import caseapp.core.RemainingArgs
import plasmon.Server
import plasmon.index.Indexer
import plasmon.protocol.CommandClient

final case class ReplStop(
  server: Server,
  client: CommandClient
) extends ServerCommandInstance[ReplStopOptions](client) {
  def run(options: ReplStopOptions, args: RemainingArgs): Unit = {
    ???
  }
}

object ReplStop extends ServerCommand[ReplStopOptions] {
  def instance(
    server: Server,
    indexer: Indexer,
    client: CommandClient,
    lspServer: plasmon.jsonrpc.JsonrpcServer,
    pool: plasmon.command.ServerCommandThreadPools
  ): ServerCommandInstance[ReplStopOptions] =
    ReplStop(server, client)
}
