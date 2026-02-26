package plasmon.index

import ch.epfl.scala.bsp4j as b
import plasmon.{HasState, Server}
import plasmon.PlasmonEnrichments.*
import plasmon.bsp.BuildServerInfo

import scala.concurrent.{Future, Promise}

class Indexer(server: Server) extends HasState.Delegate[String] {

  val logger = server.loggerManager.create("indexer", "Indexer")

  val actor = new IndexerActor(server, logger)(using server.pools.indexerActorContext)
  protected def delegateStateTo: HasState[String] = actor

  // loading these targets, plus their transitive dependencies
  var targets = Map.empty[BuildServerInfo, Seq[b.BuildTargetIdentifier]]

  def addTarget(info: BuildServerInfo, target: b.BuildTargetIdentifier): Boolean = {
    val current = targets.getOrElse(info, Nil)
    !current.contains(target) && {
      targets += info -> (current :+ target)
      true
    }
  }
  def addTargets(info: BuildServerInfo, additionalTargets: Seq[b.BuildTargetIdentifier]): Unit = {
    val current = targets.getOrElse(info, Nil)
    targets += info -> (current ++ additionalTargets.filterNot(current.toSet))
  }

  def removeTarget(info: BuildServerInfo, target: b.BuildTargetIdentifier): Boolean = {
    val current = targets.getOrElse(info, Nil)
    current.contains(target) && {
      val updated = current.filter(_ != target)
      if (updated.isEmpty)
        targets -= info
      else
        targets += info -> current.filter(_ != target)
      true
    }
  }

  private def buildTargetsFile = server.workingDir / ".plasmon/build-targets.json"

  def dontReloadBuildTool(info: BuildServerInfo): Boolean = {
    val formerTargets = targets
    targets -= info
    formerTargets.size != targets.size
  }

  def loadFromDisk(
    toplevelCacheOnly: Boolean,
    ignoreToplevelSymbolsErrors: Boolean,
    mayReadFromBspCache: Boolean
  ): Future[Unit] =
    loadFromDisk(
      buildTargetsFile,
      toplevelCacheOnly,
      ignoreToplevelSymbolsErrors,
      mayReadFromBspCache
    )

  def loadFromDisk(
    readFrom: os.Path,
    toplevelCacheOnly: Boolean,
    ignoreToplevelSymbolsErrors: Boolean,
    mayReadFromBspCache: Boolean
  ): Future[Unit] =
    Persist.loadFromDisk(readFrom, server.tools) match {
      case Some(targets0) =>
        val filteredInfoIt = targets0.iterator.filter {
          case (info, _) =>
            server.bspServers.get(info).isEmpty
        }
        for ((info, targets) <- filteredInfoIt) {
          scribe.info(
            s"BSP server not found: $info" +
              server.bspServers.list
                .flatMap(_._2)
                .map(_.info)
                .mkString(" (available build servers: ", ", ", ")")
          )
          scribe.info(s"Not loading targets: ${targets.mkString(", ")}")
        }
        // Also remove details about build servers not persisted on disk?
        targets ++= targets0
        index(toplevelCacheOnly, ignoreToplevelSymbolsErrors, mayReadFromBspCache)
      case None =>
        Future.successful(())
    }

  def persist(): Unit =
    Persist.persistTargets(targets, buildTargetsFile)

  def index(
    toplevelCacheOnly: Boolean,
    ignoreToplevelSymbolsErrors: Boolean,
    mayReadFromBspCache: Boolean
  ): Future[Unit] = {
    val p = Promise[Unit]()
    actor.send(
      IndexerActor.Message.Index(
        targets,
        Some(toplevelCacheOnly),
        Some(ignoreToplevelSymbolsErrors),
        Some(buildTargetsFile),
        onDone = Some(res => p.complete(res)),
        mayReadFromBspCache = mayReadFromBspCache
      )
    )
    p.future
  }

  def cachedIndexed(): Seq[(BuildServerInfo, Seq[b.BuildTargetIdentifier])] =
    actor.latestIndexed

  def cachedToplevelCacheOnly(): Boolean =
    actor.latestToplevelCacheOnly
  def cachedIgnoreToplevelSymbolsErrors(): Boolean =
    actor.latestIgnoreToplevelSymbolsErrors

  def reIndex(): Future[Unit] = {
    val p = Promise[Unit]()
    actor.send(
      IndexerActor.Message.Index(
        targets,
        None,
        None,
        None,
        onDone = Some(res => p.complete(res)),
        mayReadFromBspCache = false
      )
    )
    p.future
  }

  def expireSymbolDocDefinitions(
    source: os.Path,
    target: b.BuildTargetIdentifier,
    data: Seq[TargetData]
  ): Unit =
    actor.send(
      IndexerActor.Message.ExpireSymbolDocDefinitions(os.Path(source.toNIO), target, data)
    )

  def indexWorkspaceSourceSymbols(
    source: os.Path,
    sourceItem: Option[os.Path],
    target: b.BuildTargetIdentifier,
    data: Seq[TargetData]
  ): Unit =
    actor.send(
      IndexerActor.Message.IndexWorkspaceSourceSymbols(
        os.Path(source.toNIO),
        sourceItem.map(path => os.Path(path.toNIO)),
        target,
        data
      )
    )

  def reindexWorkspaceSource(path: os.Path): Unit =
    if (path.isScalaOrJava)
      for (targetId <- server.bspData.inverseSources0(path).merge) {
        expireSymbolDocDefinitions(
          path,
          targetId,
          server.bspData.allWritableData
        )
        indexWorkspaceSourceSymbols(
          path,
          server.bspData.inverseSourceItem(path),
          targetId,
          server.bspData.allWritableData
        )
      }
}
