package plasmon.bsp

import scala.util.Properties
import plasmon.Logger

sealed abstract class BuildServerInfo extends Product with Serializable {
  def `type`: String
  def workspace: os.Path
  def onlyTargets: Option[Set[String]]

  // for logging purposes
  def id: String
  def label: String

  def prepare: Option[(Logger, Boolean) => Unit] = None
}

object BuildServerInfo {
  final case class Bsp(
    workspace: os.Path,
    bspFile: Either[os.Path, os.SubPath],
    onlyTargets: Option[Set[String]]
  ) extends BuildServerInfo {
    def `type` = "BSP"
    def id     = "server"
    def label  = "BSP Server"
  }
  object Bsp {
    def apply(workspace: os.Path, bspFile: os.Path, onlyTargets: Option[Set[String]]): Bsp =
      Bsp(
        workspace,
        if (bspFile.startsWith(workspace)) Right(bspFile.relativeTo(workspace).asSubPath)
        else Left(bspFile),
        onlyTargets
      )
  }

  final case class Bloop(
    workspace: os.Path
  ) extends BuildServerInfo {
    def `type`                           = "Bloop"
    def onlyTargets: Option[Set[String]] = None

    def id    = "bloop"
    def label = "Bloop"
  }

  final case class Mill(workspace: os.Path, onlyTargets: Option[Set[String]])
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

  final case class Sbt(workspace: os.Path, onlyTargets: Option[Set[String]])
      extends BuildServerInfo {
    def `type` = "Sbt"
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
    def onlyTargets: Option[Set[String]] = None
  }
}
