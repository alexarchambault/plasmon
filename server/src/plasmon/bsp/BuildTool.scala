package plasmon.bsp

import com.github.plokhotnyuk.jsoniter_scala.core._
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import java.util.{Properties => JProperties}
import plasmon.Logger
import plasmon.internal.Constants
import java.security.MessageDigest
import java.nio.charset.StandardCharsets
import java.math.BigInteger
import scala.util.Properties

sealed abstract class BuildTool extends Product with Serializable {
  def launcher(tools: BuildTool.Tools): BuildServerLauncher

  /** Unique id to identify this build tool
    *
    * Should be unique for a given value of workspace.
    *
    * That is, two BuildTool instances with different workspaces can have the same id, but two
    * different BuildTool instances having the same workspace must have different ids.
    */
  def id: String

  def description(serverWorkspace: os.Path): String
  def extraLaunchers: Seq[BuildServerLauncher] = Nil

  def workspace: os.Path
}

object BuildTool {

  final case class Mill(workspace: os.Path) extends BuildTool {
    def id = Mill.id
    def description(serverWorkspace: os.Path) = {
      val relPath = workspace.relativeTo(serverWorkspace)
      if (relPath == os.rel) "Mill in this workspace"
      else s"Mill in $relPath"
    }

    def launcher(tools: Tools) =
      BuildServerLauncher(
        BuildServerInfo.Mill(workspace, None),
        "Mill",
        None
      )
  }

  object Mill {
    def id = "mill"
  }

  final case class MillViaBloop(workspace: os.Path) extends BuildTool {
    def id = MillViaBloop.id
    def description(serverWorkspace: os.Path) = {
      val relPath = workspace.relativeTo(serverWorkspace)
      if (relPath == os.rel) "Mill via Bloop in this workspace"
      else s"Mill via Bloop in $relPath"
    }

    def launcher(tools: Tools) =
      BuildServerLauncher(
        BuildServerInfo.Mill(
          workspace,
          Some(Set((workspace / "mill-build").toNIO.toUri.toASCIIString))
        ),
        "Mill via Bloop",
        None
      )

    override def extraLaunchers = Seq(
      BuildServerLauncher(
        BuildServerInfo.Bloop(
          workspace
        ),
        "Bloop",
        Some { (logger, force) =>
          Bloop.prepare(
            os.proc(
              workspace / "mill",
              "-i",
              "--import",
              "ivy:com.lihaoyi::mill-contrib-bloop:",
              "mill.contrib.bloop.Bloop/install"
            ),
            workspace,
            logger,
            force = force
          )
        }
      )
    )
  }

  object MillViaBloop {
    def id = "mill-via-bloop"
  }

  final case class SbtViaBloop(workspace: os.Path) extends BuildTool {
    def id = SbtViaBloop.id
    def description(serverWorkspace: os.Path) = {
      val relPath = workspace.relativeTo(serverWorkspace)
      if (relPath == os.rel) "Sbt via Bloop in this workspace"
      else s"Sbt via Bloop in $relPath"
    }

    def commandName: os.Shellable = SbtDist.sbtLauncher
    def launcher(tools: Tools) =
      BuildServerLauncher(
        BuildServerInfo.Bloop(workspace),
        "Sbt via Bloop",
        Some { (logger, force) =>
          val version = {
            val propFileSubPath = os.sub / "project/build.properties"
            val propFile        = workspace / propFileSubPath
            def defaultVersion  = Constants.defaultSbtVersion
            if (os.exists(propFile)) {
              val props = new JProperties
              val is    = os.read.inputStream(propFile)
              try props.load(is)
              finally is.close()
              val versionOpt = Option(props.getProperty("sbt.version"))
              versionOpt match {
                case Some(version) =>
                  logger.log(s"Found sbt version $version in $propFileSubPath")
                  version
                case None =>
                  logger.log(
                    s"No sbt version found in $propFile, using default version $defaultVersion"
                  )
                  defaultVersion
              }
            }
            else {
              logger.log(
                s"Property file $propFile not found, using default sbt version $defaultVersion"
              )
              defaultVersion
            }
          }

          def pathAsUserFriendlyString(path: os.Path): String =
            if (path.startsWith(workspace)) path.relativeTo(workspace).toString
            else path.toString

          val (metalsSbtFile, metalsSbtRemoved) = SbtBuildTool.removeLegacyGlobalPlugin(version)
          if (metalsSbtRemoved)
            logger.log(
              s"Removed existing ${pathAsUserFriendlyString(os.Path(metalsSbtFile.toNIO))}"
            )
          val written0 =
            SbtBuildTool.writeBloopPlugin(workspace, Constants.sbtBloopVersion, version)
          for ((pluginFile, written) <- written0)
            logger.log {
              if (written) s"Wrote ${pathAsUserFriendlyString(os.Path(pluginFile.toNIO))}"
              else s"No need to update ${pathAsUserFriendlyString(os.Path(pluginFile.toNIO))}"
            }
          Bloop.prepare(
            os.proc(commandName, "-Dbloop.export-jar-classifiers=sources", "bloopInstall"),
            workspace,
            logger,
            force = force
          )
        }
      )
  }

  object SbtViaBloop {
    def id = "sbt-via-bloop"
  }

  final case class Sbt(workspace: os.Path) extends BuildTool {
    def id = Sbt.id
    def description(serverWorkspace: os.Path) = {
      val relPath = workspace.relativeTo(serverWorkspace)
      if (relPath == os.rel) "Sbt in this workspace"
      else s"Sbt in $relPath"
    }

    def commandName: os.Shellable = SbtDist.sbtLauncher
    def launcher(tools: Tools) =
      BuildServerLauncher(
        BuildServerInfo.Sbt(workspace, None),
        "Sbt",
        Some { (logger, force) =>
          // Ideally, I'd like to skip that step, and run the BSP server straightaway.
          // The BSP install -> read .bsp/*.json file -> … feels clunky. If we know the
          // command to generate the BSP file, we might as well run the BSP server
          // straightaway…
          val sbtBspFileSubPath = os.sub / ".bsp/sbt.json"
          val sbtBspFile        = workspace / sbtBspFileSubPath
          val proceed =
            if (force) {
              logger.log("Forced sbt BSP install")
              true
            }
            else if (!os.exists(sbtBspFile)) {
              logger.log(s"sbt BSP file $sbtBspFileSubPath not found, running sbt BSP install")
              true
            }
            else {
              logger.log(s"Found sbt BSP file $sbtBspFileSubPath, NOT running sbt BSP install")
              false
            }
          if (proceed)
            logger.logCommand(os.proc(commandName, "bspConfig"))
              .call(
                stdout = logger.processOutput,
                cwd = workspace,
                mergeErrIntoOut = true
              )
          }
      )
  }

  object Sbt {
    def id = "sbt"
  }

  final case class Bloop(workspace: os.Path) extends BuildTool {
    def id = Bloop.id
    def description(serverWorkspace: os.Path) = {
      val relPath = workspace.relativeTo(serverWorkspace)
      if (relPath == os.rel) "Bloop in this workspace"
      else s"Bloop in $relPath"
    }

    def launcher(tools: Tools) =
      BuildServerLauncher(
        BuildServerInfo.Bloop(workspace),
        "Bloop",
        None
      )
  }

  object Bloop {
    def id = "bloop"

    def prepare(
      runBeforeIfNoDotBloop: os.proc,
      workspace: os.Path,
      logger: Logger,
      force: Boolean
    ): Unit = {
      val bloopDir = workspace / ".bloop"
      val proceed =
        if (force) {
          logger.log("Forced project export to Bloop")
          true
        }
        else if (!os.isDir(bloopDir)) {
          logger.log(s"${bloopDir.relativeTo(workspace)} not found, exporting project to Bloop")
          true
        }
        else if (os.list(bloopDir).isEmpty) {
          logger.log(
            s"Bloop directory ${bloopDir.relativeTo(workspace)} is empty, exporting project to Bloop"
          )
          true
        }
        else {
          logger.log(
            s"Found non-empty Bloop directory ${bloopDir.relativeTo(workspace)}, NOT exporting project to Bloop"
          )
          false
        }
      if (proceed)
        logger.logCommand(runBeforeIfNoDotBloop).call(
          cwd = workspace,
          mergeErrIntoOut = true,
          stdout = logger.processOutput
        )
    }
  }

  final case class ScalaCli(
    workspace: os.Path,
    sources: Seq[os.Path]
  ) extends BuildTool {
    lazy val id =
      if (sources.isEmpty) ScalaCli.id
      else {
        val sourcesList = sources
          .map { path =>
            if (path.startsWith(workspace)) path.relativeTo(workspace).toString
            else path.toString
          }
          .mkString(System.lineSeparator())
        val md = MessageDigest.getInstance("SHA-1")
        md.update(sourcesList.getBytes(StandardCharsets.UTF_8))
        val sum = md.digest()
        ScalaCli.id + "-" + new BigInteger(1, sum).toString(16)
      }
    def description(serverWorkspace: os.Path) =
      sources match {
        case Seq() =>
          val relPath = workspace.relativeTo(serverWorkspace)
          if (relPath == os.rel) "Scala CLI in this workspace"
          else s"Scala CLI in $relPath"
        case Seq(first) =>
          val relPath = first.relativeTo(serverWorkspace)
          s"Scala CLI for $relPath"
        case Seq(first, other @ _*) =>
          val relPath = first.relativeTo(serverWorkspace)
          s"Scala CLI for $relPath and ${other.length} other source(s)"
      }

    def launcher(tools: Tools) = {
      val scalaCliCommand =
        tools.tools.getOrElse("scala-cli", sys.error("scala-cli not found in tools"))
      BuildServerLauncher(
        BuildServerInfo.ScalaCli(workspace, sources, scalaCliCommand),
        "Scala CLI",
        Some { (logger, force) =>
          val scalaCliBspFile = workspace / ".bsp/scala-cli.json"
          if (force || !os.exists(scalaCliBspFile)) {
            val sourceArgs =
              if (sources.isEmpty) Seq(workspace) else sources
            logger.logCommand(os.proc(scalaCliCommand, "setup-ide", sourceArgs))
              .call(cwd = workspace, mergeErrIntoOut = true, stdout = logger.processOutput)
          }
        }
      )
    }
  }

  object ScalaCli {
    def id = "scala-cli"
  }

  final case class Bsp(info: BuildServerInfo) extends BuildTool {
    lazy val id = {
      val sum = {
        val md = MessageDigest.getInstance("SHA-1")
        md.digest(writeToArray(ConnectionInfoJson(info)))
        new BigInteger(1, md.digest()).toString(16)
      }
      s"${Bsp.id}-${info.id}-$sum"
    }
    def workspace = info.workspace
    def description(serverWorkspace: os.Path) = {
      val relPath = workspace.relativeTo(serverWorkspace)
      if (relPath == os.rel) "BSP connection in this workspace"
      else s"BSP connection in $relPath"
    }
    def launcher(tools: Tools) =
      BuildServerLauncher(info, "BSP", None)
  }

  object Bsp {
    def id = "bsp"
  }

  sealed abstract class BuildToolJson extends Product with Serializable {
    def toBuildTool(tools: BuildTool.Tools): BuildTool
  }

  object BuildToolJson {
    final case class Mill(workspace: String) extends BuildToolJson {
      def toBuildTool(tools: BuildTool.Tools): BuildTool = BuildTool.Mill(os.Path(workspace))
    }
    final case class MillViaBloop(workspace: String) extends BuildToolJson {
      def toBuildTool(tools: BuildTool.Tools): BuildTool =
        BuildTool.MillViaBloop(os.Path(workspace))
    }
    final case class Sbt(workspace: String) extends BuildToolJson {
      def toBuildTool(tools: BuildTool.Tools): BuildTool = BuildTool.Sbt(os.Path(workspace))
    }
    final case class SbtViaBloop(workspace: String) extends BuildToolJson {
      def toBuildTool(tools: BuildTool.Tools): BuildTool = BuildTool.SbtViaBloop(os.Path(workspace))
    }
    final case class Bloop(workspace: String) extends BuildToolJson {
      def toBuildTool(tools: BuildTool.Tools): BuildTool = BuildTool.Bloop(os.Path(workspace))
    }
    final case class ScalaCli(workspace: String, sources: Seq[String]) extends BuildToolJson {
      def toBuildTool(tools: BuildTool.Tools): BuildTool =
        BuildTool.ScalaCli(os.Path(workspace), sources.map(os.Path(_)))
    }
    final case class Bsp(info: ConnectionInfoJson) extends BuildToolJson {
      def toBuildTool(tools: BuildTool.Tools): BuildTool =
        BuildTool.Bsp(info.toConnectionInfo(tools))
    }

    implicit lazy val codec: JsonValueCodec[BuildToolJson] = JsonCodecMaker.make
    lazy val seqCodec: JsonValueCodec[Seq[BuildToolJson]]  = JsonCodecMaker.make

    def apply(buildTool: BuildTool): BuildToolJson =
      buildTool match {
        case t: BuildTool.Mill         => Mill(t.workspace.toString)
        case t: BuildTool.MillViaBloop => MillViaBloop(t.workspace.toString)
        case t: BuildTool.Sbt          => Sbt(t.workspace.toString)
        case t: BuildTool.SbtViaBloop  => SbtViaBloop(t.workspace.toString)
        case t: BuildTool.Bloop        => Bloop(t.workspace.toString)
        case t: BuildTool.ScalaCli     => ScalaCli(t.workspace.toString, t.sources.map(_.toString))
        case t: BuildTool.Bsp          => Bsp(ConnectionInfoJson(t.info))
      }
  }

  final case class Tools(tools: Map[String, Seq[String]])
}
