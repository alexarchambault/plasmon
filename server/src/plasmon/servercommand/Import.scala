package plasmon.servercommand

import caseapp.core.RemainingArgs
import plasmon.Server
import plasmon.command.ServerCommandThreadPools
import plasmon.index.Indexer
import plasmon.protocol.CommandClient

import java.util.concurrent.ExecutorService

import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.jdk.CollectionConverters.*

final case class Import(
  server: Server,
  indexer: Indexer,
  client: CommandClient,
  bspPool: ExecutorService
) extends ServerCommandInstance[ImportOptions](client) {
  def run(options: ImportOptions, args: RemainingArgs): Unit = {

    val allTargetsByBuildServer = server.bspServers.list.flatMap(_._2).map { buildServer =>
      buildServer -> buildServer
        .conn
        .workspaceBuildTargets
        .get()
        .getTargets
        .asScala
        .toList
    }
    val allTargets = allTargetsByBuildServer.map(_._2).flatten
    printLine(s"  Found ${allTargets.length} build targets:", toStderr = true)
    for (target <- allTargets)
      printLine(target.getId.getUri, toStderr = true)
    printLine("", toStderr = true)

    printLine(
      s"Retained ${allTargetsByBuildServer.map(_._2.length).sum} build targets",
      toStderr = true
    )

    printLine(
      if (options.keep) "Indexing additional build targets"
      else "Indexing build targets",
      toStderr = true
    )
    if (!options.keep)
      indexer.targets = Map.empty
    for ((server, targets) <- allTargetsByBuildServer)
      indexer.addTargets(server.info, targets.map(_.getId))
    try {
      val f = indexer.index(
        // keepTarget = {
        //   val finalTargetsSets = finalTargetsByBuildServer
        //     .map { case (server, targets) => (server.info, targets.toSet) }
        //     .toMap
        //   if (options.keep) {
        //     // val inLatestIndexed = indexer.inLatestIndexed()
        //     (info, target) =>
        //       // inLatestIndexed(info, target) ||
        //         finalTargetsSets.get(info).exists(_.contains(target.getId))
        //   }
        //   else
        //     (info, target) =>
        //       finalTargetsSets.get(info).exists(_.contains(target.getId))
        // },
        toplevelCacheOnly = options.toplevelCacheOnly,
        ignoreToplevelSymbolsErrors = options.ignoreToplevelSymbolsErrors,
        mayReadFromBspCache = false
      )
      Await.result(f, Duration.Inf)
    }
    catch {
      case e: java.util.concurrent.ExecutionException =>
        e.getCause match {
          case e0: org.eclipse.lsp4j.jsonrpc.MessageIssueException =>
            printLine(e0.getRpcMessage.getJsonrpc)
            printLine(s"Error decoding response: ${e0.getMessage}", toStderr = true)
          case _ =>
        }
        throw e
    }
  }
}

object Import extends ServerCommand[ImportOptions] {
  def instance(
    server: Server,
    indexer: Indexer,
    client: CommandClient,
    lspServer: plasmon.jsonrpc.JsonrpcServer,
    pool: ServerCommandThreadPools
  ): ServerCommandInstance[ImportOptions] =
    Import(server, indexer, client, pool.bspEces)
}
