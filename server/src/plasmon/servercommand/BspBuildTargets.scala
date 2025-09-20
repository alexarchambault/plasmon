package plasmon.servercommand

import caseapp.core.RemainingArgs
import plasmon.index.Indexer
import plasmon.protocol.CommandClient
import plasmon.Server

import java.util.concurrent.ExecutorService

import scala.jdk.CollectionConverters.*

final case class BspBuildTargets(
  server: Server,
  client: CommandClient,
  bspPool: ExecutorService
) extends ServerCommandInstance[BspBuildTargetsOptions](client) {
  override def names = BspBuildTargets.names
  def run(options: BspBuildTargetsOptions, args: RemainingArgs): Unit = {

    val path = args.all match {
      case Seq()      => os.pwd
      case Seq(path0) => os.Path(path0, os.pwd)
      case _ =>
        sys.error("Cannot look into several projects for now")
    }

    for (buildServer <- server.bspServers.list.flatMap(_._2)) {

      // TODO Say if each build target is loaded or not

      printLine(s"  Build server: ${buildServer.name}")

      val allTargets = buildServer
        .conn
        .workspaceBuildTargets
        .get()
        .getTargets
        .asScala
        .toList
      printLine(s"  Found ${allTargets.length} build targets", toStderr = true)

      val constraints = options.require
        .map(_.trim)
        .filter(_.nonEmpty)
        .flatMap(BspUtil.TagConstraint.parse)

      val targets = allTargets.filter { target =>
        val tags = BspUtil.tags(target)
        constraints.forall(_.validate(tags))
      }
      printLine(s"  Retaining ${targets.length} build targets", toStderr = true)

      val values  = targets.sortBy(_.getId.getUri)
      val tagsMap = BspUtil.tags(targets)
      for (target <- values) {
        val id =
          if (options.rawIds) target.getId.getUri
          else BspUtil.targetShortId(server.bspData, target.getId)
        def tags = tagsMap
          .getOrElse(target.getId, Set.empty[BspUtil.TargetTag])
          .map(_.tag)
          .toVector
          .sorted
        printLine(
          if (options.details)
            s"$id (${tags.mkString(", ")})"
          else
            id
        )
      }
    }
  }
}

object BspBuildTargets extends ServerCommand[BspBuildTargetsOptions] {
  def instance(
    server: Server,
    indexer: Indexer,
    client: CommandClient,
    lspServer: plasmon.jsonrpc.JsonrpcServer,
    pool: plasmon.command.ServerCommandThreadPools
  ): ServerCommandInstance[BspBuildTargetsOptions] =
    BspBuildTargets(server, client, pool.bspEces)
  override def names = List(
    List("bsp", "build-target"),
    List("bsp", "build-targets"),
    List("bsp", "build", "target"),
    List("bsp", "build", "targets"),
    List("bsp", "target"),
    List("bsp", "targets")
  )
}
