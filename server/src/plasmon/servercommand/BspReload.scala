package plasmon.servercommand

import caseapp.core.RemainingArgs
import plasmon.Server
import plasmon.index.Indexer
import plasmon.protocol.CommandClient

import scala.concurrent.Await
import scala.concurrent.duration.Duration

final case class BspReload(
  server: Server,
  indexer: Indexer,
  client: CommandClient,
  pool: plasmon.command.ServerCommandThreadPools
) extends ServerCommandInstance[BspReloadOptions](client) {
  def run(options: BspReloadOptions, args: RemainingArgs): Unit = {
    val info0 = BspAdd.info(server.workingDir, options.shared, args.all, server.tools)

    val detailsOpt   = server.bspServers.get(info0)
    val buildToolOpt = Await.result(server.bspServers.remove(info0), Duration.Inf)
    buildToolOpt match {
      case Some(buildTool) =>
        server.bspServers.addOne(
          buildTool,
          info0,
          detailsOpt.fold(info0.`type`)(_.name),
          printLine(_, toStderr = true),
          pool.bspEces,
          () => pool.bloopThreads
        )
        indexer.reIndex()
      case None =>
        printLine("Nothing to reload")
        if (options.strict)
          exit(1)
    }
  }
}

object BspReload extends ServerCommand[BspReloadOptions] {
  def instance(
    server: Server,
    indexer: Indexer,
    client: CommandClient,
    lspServer: plasmon.jsonrpc.JsonrpcServer,
    pool: plasmon.command.ServerCommandThreadPools
  ): ServerCommandInstance[BspReloadOptions] =
    BspReload(server, indexer, client, pool)
}
