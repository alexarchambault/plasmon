package plasmon.servercommand

import caseapp.core.RemainingArgs
import plasmon.protocol.CommandClient
import plasmon.index.Indexer
import plasmon.Server

final case class Template(
  server: Server,
  indexer: Indexer,
  client: CommandClient,
  pool: plasmon.command.ServerCommandThreadPools
) extends ServerCommandInstance[TemplateOptions](client) {
  def run(options: TemplateOptions, args: RemainingArgs): Unit = {
    ???
  }
}

object Template extends ServerCommand[TemplateOptions] {
  def instance(
    server: Server,
    indexer: Indexer,
    client: CommandClient,
    lspServer: plasmon.jsonrpc.JsonrpcServer,
    pool: plasmon.command.ServerCommandThreadPools
  ): ServerCommandInstance[TemplateOptions] =
    Template(server, indexer, client, pool)
}
