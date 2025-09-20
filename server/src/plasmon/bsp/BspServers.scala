package plasmon.bsp

import bloop.rifle.BloopThreads
import com.github.plokhotnyuk.jsoniter_scala.core.*

import java.util.concurrent.ExecutorService

import plasmon.HasState
import scala.concurrent.Future
import scala.concurrent.Promise

class BspServers(
  persistTo: Option[os.Path],
  server: BspServersServerLike,
  actorCtx: castor.Context
) extends HasState.Delegate[String] with AutoCloseable {

  val actor                                       = new BspServersActor(server)(actorCtx)
  protected def delegateStateTo: HasState[String] = actor

  override def close(): Unit = {
    scribe.info(s"Closing $this")
    actor.close()
  }

  def get(info: BuildServerInfo): Option[BspConnection] =
    actor.byInfo.get(info)
  def get(workspace: os.Path, name: String): Option[BspConnection] =
    actor.byWorkspaceAndName.get((workspace, name))

  def list: Seq[(BuildTool, Seq[BspConnection])] =
    actor.connections

  def tryAdd(
    buildTool: BuildTool,
    launchers: Seq[BuildServerLauncher],
    log: String => Unit,
    bspPool: ExecutorService,
    bloopThreads: () => BloopThreads
  ): Future[Either[String, Unit]] = {
    val p = Promise[Either[String, Unit]]()
    actor.send(
      BspServersActor.Message.Add(
        buildTool,
        launchers,
        log,
        bspPool,
        bloopThreads,
        onDone = res => p.complete(res)
      )
    )
    p.future
  }

  def add(
    buildTool: BuildTool,
    log: String => Unit,
    bspPool: ExecutorService,
    bloopThreads: () => BloopThreads,
    tools: BuildTool.Tools
  ): Future[Either[String, Unit]] =
    tryAdd(
      buildTool,
      // important: the order matters here for Mill Bloop / Mill BSP
      // Mill BSP sometimes recompiles the Mill build, which can make the Bloop export crash
      buildTool.extraLaunchers :+ buildTool.launcher(tools),
      log,
      bspPool,
      bloopThreads
    )
  def addOne(
    buildTool: BuildTool,
    info: BuildServerInfo,
    displayName: String,
    log: String => Unit,
    bspPool: ExecutorService,
    bloopThreads: () => BloopThreads
  ): Future[Either[String, Unit]] =
    tryAdd(
      buildTool,
      Seq(BuildServerLauncher(info, displayName, None)),
      log,
      bspPool,
      bloopThreads
    )

  def remove(info: BuildServerInfo): Future[Option[BuildTool]] =
    remove(info, hard = false)
  def remove(info: BuildServerInfo, hard: Boolean): Future[Option[BuildTool]] = {
    val p = Promise[Option[BuildTool]]()
    actor.send(
      BspServersActor.Message.Remove(
        info,
        hard,
        onDone = res => p.complete(res)
      )
    )
    p.future
  }

  def json: Array[Byte] = {
    val seq = actor.connections.map(c => BuildTool.BuildToolJson(c._1))
    writeToArray(seq)(BuildTool.BuildToolJson.seqCodec)
  }

  private val persistLock = new Object
  def persist(): Unit =
    for (path <- persistTo)
      persistLock.synchronized {
        scribe.info(s"Persisting BSP server list to $path")
        os.write.over(path, json, createFolders = true)
      }

  def canLoadFromDisk(): Boolean =
    persistTo.exists(os.isFile)

  def loadFromDisk(
    log: String => Unit,
    bspPool: ExecutorService,
    bloopThreads: () => BloopThreads
  ): Future[Unit] =
    persistTo.filter(os.isFile) match {
      case Some(path) =>
        val p = Promise[Unit]()
        actor.send(
          BspServersActor.Message.AddAllFromFile(
            path,
            log,
            bspPool,
            bloopThreads,
            Some("Starting saved BSP servers"),
            res => p.complete(res)
          )
        )
        p.future
      case None =>
        Future.successful(())
    }
}
