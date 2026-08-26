package plasmon.bsp

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import plasmon.Logger
import plasmon.render.JsonCodecs.given

import java.math.BigInteger
import java.nio.charset.StandardCharsets
import java.security.MessageDigest

import scala.util.Properties

sealed abstract class BuildServerInfo extends Product with Serializable {
  def `type`: String
  def workspace: os.Path

  // for logging purposes
  def id: String
  def label: String

  /** Directory name this build server's recorded BSP responses live under.
    *
    * Defaults to [[id]]. Must be unique among the build servers loaded for a given workspace, so
    * infos that can legitimately appear more than once there override it.
    */
  def cacheKey: String = id

  def prepare: Option[(Logger, Boolean) => Unit] = None
}

object BuildServerInfo {

  given JsonValueCodec[BuildServerInfo] =
    JsonCodecMaker.make

  final case class Bsp(
    workspace: os.Path,
    bspFile: Either[os.Path, os.SubPath]
  ) extends BuildServerInfo {
    def `type` = "BSP"
    def id     = "server"
    def label  = "BSP Server"
  }
  object Bsp {
    def apply(workspace: os.Path, bspFile: os.Path): Bsp =
      Bsp(
        workspace,
        if (bspFile.startsWith(workspace)) Right(bspFile.relativeTo(workspace).asSubPath)
        else Left(bspFile)
      )
  }

  final case class Bloop(
    workspace: os.Path
  ) extends BuildServerInfo {
    def `type`                           = "Bloop"

    def id    = "bloop"
    def label = "Bloop"
  }

  final case class Mill(workspace: os.Path)
      extends BuildServerInfo {
    def `type` = "Mill"
    def id     = "mill"
    def label  = "Mill"

    def commandName =
      if (Properties.isWin) "mill.bat"
      else "./mill"

    override def prepare: Option[(Logger, Boolean) => Unit] =
      Some { (logger, force) =>
        // Ideally, I'd like to skip that step, and run the BSP server straightaway.
        // The BSP install -> read .bsp/*.json file -> … feels clunky. If we know the
        // command to generate the BSP file, we might as well run the BSP server
        // straightaway…
        val millBspFileSubPath = os.sub / ".bsp/mill-bsp.json"
        val millBspFile        = workspace / millBspFileSubPath
        val proceed =
          if (force) {
            logger.log("Forced Mill BSP install")
            true
          }
          else if (!os.exists(millBspFile)) {
            logger.log(s"Mill BSP file $millBspFileSubPath not found, running Mill BSP install")
            true
          }
          else {
            logger.log(s"Found Mill BSP file $millBspFileSubPath, NOT running Mill BSP install")
            false
          }
        if (proceed)
          logger.logCommand(os.proc(commandName, "-i", "mill.bsp.BSP/install"))
            .call(
              stdout = logger.processOutput,
              cwd = workspace,
              mergeErrIntoOut = true
            )
      }
  }

  final case class Sbt(workspace: os.Path)
      extends BuildServerInfo {
    def `type` = "sbt"
    def id     = "sbt"
    def label  = "sbt"
  }

  final case class ScalaCli(
    workspace: os.Path,
    paths: Seq[os.Path],
    scalaCliCommand: Seq[String]
  ) extends BuildServerInfo {
    def `type`                           = "Scala CLI"
    def id                               = "scala-cli"
    def label                            = "Scala CLI"

    // Several Scala CLI build servers can be loaded in one workspace, one per source set
    override def cacheKey =
      if (paths.isEmpty) id
      else {
        val pathList = paths
          .map(p => if (p.startsWith(workspace)) p.relativeTo(workspace).toString else p.toString)
          .mkString(System.lineSeparator())
        val md = MessageDigest.getInstance("SHA-1")
        md.update(pathList.getBytes(StandardCharsets.UTF_8))
        s"$id-${new BigInteger(1, md.digest()).toString(16)}"
      }
  }

  /** Replays BSP responses recorded from a real build tool, without running one.
    *
    * Lets the presentation compiler be brought into the state a given build would put it in, at a
    * fraction of the cost - no build tool JVM, no compilation, no BSP handshake. Dependencies still
    * get fetched through the coursier cache when the recording refers to them.
    *
    * @param dataDir
    *   directory holding the recorded responses, in the layout [[plasmon.index.IndexerActor]]
    *   writes
    */
  final case class Replay(
    workspace: os.Path,
    dataDir: os.Path
  ) extends BuildServerInfo {
    def `type` = "Replay"
    def id     = "replay"
    def label  = "Replay"
  }
}
