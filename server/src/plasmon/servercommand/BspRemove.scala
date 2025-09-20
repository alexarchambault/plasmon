package plasmon.servercommand

import caseapp.core.RemainingArgs
import plasmon.command.ServerCommandThreadPools
import plasmon.protocol.CommandClient
import plasmon.index.Indexer
import plasmon.Server
import scala.concurrent.Await
import scala.concurrent.duration.Duration

final case class BspRemove(
  server: Server,
  indexer: Indexer,
  client: CommandClient,
  pool: ServerCommandThreadPools
) extends ServerCommandInstance[BspRemoveOptions](client) {
  override def names = BspRemove.names
  def run(options: BspRemoveOptions, args: RemainingArgs): Unit = {

    val info0 = BspAdd.info(server.workingDir, options.shared, args.all, server.tools)

    if (indexer.dontReloadBuildTool(info0))
      indexer.persist()
    if (Await.result(server.bspServers.remove(info0), Duration.Inf).nonEmpty)
      server.bspServers.persist()

    if (server.bspData.targetData(info0).isDefined)
      indexer.reIndex()
  }
}

object BspRemove extends ServerCommand[BspRemoveOptions] {
  def instance(
    server: Server,
    indexer: Indexer,
    client: CommandClient,
    lspServer: plasmon.jsonrpc.JsonrpcServer,
    pool: ServerCommandThreadPools
  ): ServerCommandInstance[BspRemoveOptions] =
    BspRemove(server, indexer, client, pool)
  override def names = List(
    List("bsp", "remove"),
    List("bsp-remove")
  )
}
