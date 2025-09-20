package plasmon.servercommand

import caseapp.core.RemainingArgs
import plasmon.command.ServerCommandThreadPools
import plasmon.protocol.CommandClient
import plasmon.index.Indexer
import plasmon.Server

import java.util.concurrent.ExecutorService

import scala.jdk.CollectionConverters.*
import scala.concurrent.Await
import scala.concurrent.duration.Duration

final case class Import(
  server: Server,
  indexer: Indexer,
  client: CommandClient,
  bspPool: ExecutorService
) extends ServerCommandInstance[ImportOptions](client) {
  def run(options: ImportOptions, args: RemainingArgs): Unit = {

    val onlyTargets = options.target.map(_.trim).toSet

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

    val targetsByBuildServer = allTargetsByBuildServer.map {
      case (buildServer, allTargets) =>
        val targets =
          if (onlyTargets.isEmpty)
            allTargets
          else
            allTargets
              .filter { target =>
                onlyTargets.contains(target.getId.getUri) ||
                onlyTargets.contains(BspUtil.targetShortId(
                  server.bspData,
                  target.getId
                ))
              }
        (buildServer, targets)
    }

    if (onlyTargets.nonEmpty && targetsByBuildServer.map(_._2.length).sum != onlyTargets.size) {
      val loadedTargets = targetsByBuildServer
        .flatMap {
          case (buildServer, targets) =>
            targets.map(target =>
              BspUtil.targetShortId(server.bspData, target.getId)
            ) ++
              targets.map(_.getId.getUri)
        }
      val missing = onlyTargets -- loadedTargets
      assert(missing.nonEmpty)
      val missing0 = missing.toVector.sorted
      printLine(s"Target(s) not found: ${missing0.mkString(", ")}", toStderr = true)
      printLine(s" Loaded target(s):", toStderr = true)
      for (target <- loadedTargets)
        printLine(target, toStderr = true)
      printLine("", toStderr = true)
      exit(1)
    }

    printLine(
      s"Retained ${targetsByBuildServer.map(_._2.length).sum} build targets",
      toStderr = true
    )

    printLine(
      if (options.keep) "Indexing additional build targets"
      else "Indexing build targets",
      toStderr = true
    )
    if (!options.keep) {
      indexer.targets = Map.empty
      indexer.addAllTargets = Set.empty
    }
    for ((server, targets) <- targetsByBuildServer)
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
        ignoreToplevelSymbolsErrors = options.ignoreToplevelSymbolsErrors
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
