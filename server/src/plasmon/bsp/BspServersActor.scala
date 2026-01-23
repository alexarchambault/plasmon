package plasmon.bsp

import plasmon.HasState
import java.util.concurrent.TimeUnit
import bloop.rifle.BloopThreads

import java.util.concurrent.ExecutorService
import plasmon.Logger
import plasmon.servercommand.BspUtil
import scala.util.Try

class BspServersActor(
  server: BspServersServerLike
)(implicit ctx: castor.Context) extends plasmon.PlasmonActor[BspServersActor.Message]
    with HasState[String] with AutoCloseable {

  import BspServersActor.*

  var connections        = Seq.empty[(BuildTool, Seq[BspConnection])]
  var byWorkspaceAndName = Map.empty[(os.Path, String), BspConnection]
  var byInfo             = Map.empty[BuildServerInfo, BspConnection]

  def languageClient         = server.languageClient
  protected def progressId   = "bsp"
  protected def progressName = "BSP"

  override def close(): Unit = {
    scribe.info(s"Closing $this")
    inState("Closing BSP connections", progress = "Closing BSP connections") {
      for (conn <- connections.flatMap(_._2)) {
        try conn.client.close()
        catch {
          case t: Throwable =>
            scribe.error(s"Error closing BSP build client ${conn.info.label}", t)
        }
        try conn.conn.buildShutdown().get(2L, TimeUnit.SECONDS)
        catch {
          case t: Throwable =>
            scribe.error(s"Error waiting for BSP server shutdown for ${conn.info.label}", t)
        }
        try conn.conn.onBuildExit()
        catch {
          case t: Throwable =>
            scribe.error(s"Error asking BSP server to exit for ${conn.info.label}", t)
        }
        try conn.proc.close()
        catch {
          case t: Throwable =>
            scribe.error(s"Error shutting down BSP server process for ${conn.info.label}", t)
        }
      }
    }
  }

  var activeMessages: Seq[Message] = Nil

  override def runBatch(msgs: Seq[Message]): Unit =
    try {
      assert(activeMessages.isEmpty)
      activeMessages = msgs
      super.runBatch(msgs)
      msgs.foreach {
        case add: Message.Add =>
          val res = Try {
            val maybeError = tryAdd(
              add.buildTool,
              add.launchers,
              add.log,
              add.bspPool,
              add.bloopThreads
            )
            maybeError match {
              case Left(err) =>
                scribe.error(s"Error adding build tool ${add.buildTool}: $err")
              case Right(()) =>
            }
            maybeError
          }
          add.onDone(res)
        case addAllFromFile: Message.AddAllFromFile =>
          def proceed(): Try[Unit] =
            Try {
              val buildTools = inState(
                "Reading saved BSP servers from disk",
                progress = "Reading saved BSP servers from disk"
              ) {
                Persist.load(addAllFromFile.path, server.tools)
              }
              for (buildTool <- buildTools) {
                val maybeError = tryAdd(
                  buildTool,
                  buildTool.extraLaunchers :+ buildTool.launcher(server.tools),
                  addAllFromFile.log,
                  addAllFromFile.bspPool,
                  addAllFromFile.bloopThreads
                )
                maybeError match {
                  case Left(err) =>
                    scribe.error(s"Error adding build tool $buildTool: $err")
                  case Right(()) =>
                }
              }
            }
          val res = addAllFromFile.state match {
            case Some(state) =>
              inState(state) {
                proceed()
              }
            case None =>
              proceed()
          }
          addAllFromFile.onDone(res)
        case remove0: Message.Remove =>
          val res = Try(remove(remove0.info, remove0.hard))
          remove0.onDone(res)
      }
    }
    finally {
      activeMessages = Nil
    }

  private def tryAdd(
    buildTool: BuildTool,
    launchers: Seq[BuildServerLauncher],
    log: String => Unit,
    bspPool: ExecutorService,
    bloopThreads: () => BloopThreads
  ): Either[String, Unit] =
    inState(
      s"Adding ${buildTool.description(server.workingDir)}",
      progress = s"Adding ${buildTool.description(server.workingDir)}"
    ) {
      val connections = launchers.map { launcher =>
        val (logger, outputLogger) = createLoggers(
          server.loggerManager,
          server.workingDir,
          launcher.info.workspace,
          launcher.info.id,
          launcher.info.label
        )
        val conn = inState(
          s"Starting ${launcher.preliminaryDisplayName} BSP server",
          Some(logger),
          progress = s"Starting ${launcher.preliminaryDisplayName} BSP server"
        ) {
          BspUtil.bspServerFromInfo(
            launcher,
            log,
            server.createBuildClient,
            server.languageClient,
            launcher.info.id,
            launcher.preliminaryDisplayName,
            bspPool,
            bloopThreads,
            server.javaHome,
            server.bloopJavaHome,
            logger,
            outputLogger,
            server.logJsonrpcInput,
            server.enableBestEffortMode
          )
        }
        scribe.info(
          s"${launcher.preliminaryDisplayName} BSP server initialization result: ${conn.params}"
        )
        conn
      }

      add(buildTool, connections)
    }

  private def add(buildTool: BuildTool, conns: Seq[BspConnection]): Either[String, Unit] = {
    val errorOpt = conns
      .iterator
      .flatMap { conn0 =>
        if (byWorkspaceAndName.contains((conn0.info.workspace, conn0.name)))
          Iterator(s"A connection for '${conn0.name}' in ${conn0.info.workspace} already exists")
        else if (byInfo.contains(conn0.info))
          Iterator(s"A connection for ${conn0.info} already exists")
        else
          Iterator.empty
      }
      .find(_ => true)

    errorOpt.toLeft {
      connections =
        if (connections.exists(_._1 == buildTool))
          connections.map {
            case (`buildTool`, currentConns) => (buildTool, currentConns ++ conns)
            case other                       => other
          }
        else
          connections :+ (buildTool -> conns)
      for (conn0 <- conns) {
        byWorkspaceAndName += (conn0.info.workspace, conn0.name) -> conn0
        byInfo += conn0.info                                     -> conn0
      }
    }
  }

  private def remove(info: BuildServerInfo, hard: Boolean): Option[BuildTool] =
    byInfo.get(info).flatMap { conn =>
      conn.client.close()
      conn.conn.buildShutdown().get()
      if (hard)
        conn.proc.hardClose(conn.logger)
      else
        conn.proc.close()
      byWorkspaceAndName = byWorkspaceAndName - (conn.info.workspace -> conn.name)
      val buildToolOpt = connections.find(_._2.exists(_.info == info)).map(_._1)
      if (buildToolOpt.isEmpty)
        scribe.error(s"No build tool found for $info (should not happen)")
      connections = connections.flatMap {
        case (buildTool, conns) =>
          if (conns.exists(_.info == info)) {
            val remainingConns = conns.filter(_.info != info)
            if (remainingConns.isEmpty) Nil
            else Seq((buildTool, remainingConns))
          }
          else
            Seq((buildTool, conns))
      }
      byInfo = byInfo - conn.info
      buildToolOpt
    }

  // private def health(): Map[BuildServerInfo, Boolean] = {
  //   byInfo.map {
  //     case (info, conn) =>
  //       info -> conn.conn
  //   }
  //   ???
  // }

  def asJson: BspServersActor.AsJson = {
    val state0 = state()
    BspServersActor.AsJson(
      connections = connections.map {
        case (buildTool, conns) =>
          (buildTool, conns.map(_.asJson))
      },
      state = state0._1,
      stateLogger = state0._2.map(_.toString),
      awaitingMessages = awaitingMessages.toSeq.map(_.toString),
      activeMessages = activeMessages.map(_.toString)
    )
  }
}

object BspServersActor {
  sealed abstract class Message extends Product with Serializable
  object Message {
    final case class Add(
      buildTool: BuildTool,
      launchers: Seq[BuildServerLauncher],
      log: String => Unit,
      bspPool: ExecutorService,
      bloopThreads: () => BloopThreads,
      onDone: Try[Either[String, Unit]] => Unit
    ) extends Message
    final case class AddAllFromFile(
      path: os.Path,
      log: String => Unit,
      bspPool: ExecutorService,
      bloopThreads: () => BloopThreads,
      state: Option[String],
      onDone: Try[Unit] => Unit
    ) extends Message
    final case class Remove(
      info: BuildServerInfo,
      hard: Boolean,
      onDone: Try[Option[BuildTool]] => Unit
    ) extends Message
  }

  private def createLoggers(
    loggerManager: Logger.Manager,
    serverWorkingDir: os.Path,
    bspWorkspace: os.Path,
    id: String,
    label: String
  ): (Logger, Logger) = {
    val relPath = bspWorkspace.relativeTo(serverWorkingDir)
    val (pathPart, labelSuffix) = (relPath.ups, relPath.segments) match {
      case (0, Seq())    => ("wd", "")
      case (n, segments) => (("up-" * n) + segments.mkString("-"), s" $relPath")
    }
    val mainLogger = loggerManager.create(s"bsp-$id-$pathPart", label + labelSuffix)
    val outputLogger =
      loggerManager.create(s"bsp-output-$id-$pathPart", label + " (output)" + labelSuffix)
    (mainLogger, outputLogger)
  }

  final case class AsJson(
    connections: Seq[(BuildTool, Seq[BspConnection.AsJson])],
    state: Seq[String],
    stateLogger: Option[String],
    awaitingMessages: Seq[String],
    activeMessages: Seq[String]
  )
}
