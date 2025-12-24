package plasmon.integration

import com.virtuslab.using_directives.UsingDirectivesProcessor

import dependency.*
import dependency.parser.DependencyParser
import scala.jdk.CollectionConverters.*
import com.virtuslab.using_directives.custom.model.Path
import com.virtuslab.using_directives.custom.model.StringValue
import java.io.OutputStream
import scala.concurrent.duration.{DurationInt, FiniteDuration, IntMult}

abstract class SingleModuleBuildTool extends Product with Serializable {
  def id: String
  def displayName: String
  def singleFile(path: os.SubPath, source: String): (os.SubPath, Seq[(os.SubPath, String)])
  def singleModule(
    moduleName: String,
    files: Map[os.SubPath, String]
  ): (Map[os.SubPath, os.SubPath], Seq[(os.SubPath, String)])
  def setup(
    workspace: os.Path,
    osOpt: Option[OutputStream],
    readOnlyToplevelSymbolsCache: Boolean = false,
    compiles: Boolean = true
  ): Unit
  def compile(
    workspace: os.Path,
    osOpt: Option[OutputStream]
  ): Unit

  def needsStandaloneCompiler: Boolean = false
  def isSlow: Boolean                  = false

  final def defaultTimeout: FiniteDuration =
    (if (isSlow) 2 else 1) * TestUtil.baseTimeout
}

object SingleModuleBuildTool {

  private def scalaVersion(source: String): Option[String] = {
    val processor  = new UsingDirectivesProcessor
    val directives = processor.extract(source.toCharArray).asScala
    directives
      .iterator
      .flatMap(_.getFlattenedMap.asScala.get(new Path("scala")).iterator)
      .flatMap(_.asScala.iterator)
      .collectFirst {
        case s: StringValue => s.get()
      }
  }

  private def dependencies(source: String): Seq[String] = {
    val processor  = new UsingDirectivesProcessor
    val directives = processor.extract(source.toCharArray).asScala
    directives
      .iterator
      .flatMap { dir =>
        Iterator("lib", "dep").flatMap { name =>
          dir.getFlattenedMap.asScala.get(new Path(name)).iterator
        }
      }
      .flatMap(_.asScala.iterator)
      .collect {
        case s: StringValue => s.get()
      }
      .toVector
  }

  case object ScalaCli extends SingleModuleBuildTool {
    def id          = "scala-cli"
    def displayName = "Scala CLI"
    def singleFile(path: os.SubPath, source: String): (os.SubPath, Seq[(os.SubPath, String)]) =
      (path, Seq(path -> source))
    def singleModule(
      moduleName: String,
      files: Map[os.SubPath, String]
    ): (Map[os.SubPath, os.SubPath], Seq[(os.SubPath, String)]) = {
      // Ignoring moduleName (Scala CLI is basically single module anyway)
      def pathFor(basePath: os.SubPath) = os.sub / basePath
      val map = files.map {
        case (path, _) =>
          (path, pathFor(path))
      }
      val files0 = files.toVector.sortBy(_._1).map {
        case (path, content) =>
          (pathFor(path), content)
      }
      (map, files0)
    }
    def setup(
      workspace: os.Path,
      osOpt: Option[OutputStream],
      readOnlyToplevelSymbolsCache: Boolean,
      compiles: Boolean
    ): Unit = {
      if (compiles)
        TestUtil.runCommand(workspace, osOpt)(TestUtil.scalaCli, "compile", ".")
      else
        TestUtil.runCommand(workspace, osOpt)(TestUtil.scalaCli, "setup-ide", ".")
      TestUtil.runServerCommand(workspace, osOpt)("build-tool", "add", ".", "--scala-cli")
      TestUtil.runServerCommand(workspace, osOpt)(
        "import",
        ".",
        "--ignore-toplevel-symbols-errors=false",
        if (readOnlyToplevelSymbolsCache) Seq("--toplevel-cache-only") else Nil
      )
      if (compiles)
        TestUtil.runServerCommand(workspace, osOpt)("bsp", "compile")
    }
    def compile(
      workspace: os.Path,
      osOpt: Option[OutputStream]
    ): Unit =
      TestUtil.runServerCommand(workspace, osOpt)("bsp", "compile")
  }

  case object Mill extends SingleModuleBuildTool {
    def id          = "mill"
    def displayName = "Mill"
    private def scalaBuildMill(moduleName: String, sources: Seq[String]): String = {
      val sv = sources.iterator
        .flatMap(source => scalaVersion(source).iterator)
        .find(_ => true)
        .getOrElse {
          sys.error("No Scala version found in test source")
        }
      val deps = sources.flatMap(dependencies(_))
      val q    = "\""
      val nl   = System.lineSeparator()
      val depsPart =
        if (deps.isEmpty) ""
        else
          s"""def mvnDeps = Seq(
             |${deps.map(dep => s"  mvn$q$dep$q,$nl").mkString})
             |""".stripMargin
      s"""import mill._
         |import mill.scalalib._
         |
         |object `$moduleName` extends ScalaModule {
         |  def jvmId = ""
         |  def scalaVersion = "$sv"
         |  $depsPart
         |}
         |
         |""".stripMargin
    }
    private def javaBuildMill(moduleName: String, sources: Seq[String]): String = {
      val deps = sources.flatMap(dependencies(_))
      val q    = "\""
      val nl   = System.lineSeparator()
      val depsPart =
        if (deps.isEmpty) ""
        else
          s"""def mvnDeps = Seq(
             |${deps.map(dep => s"  mvn$q$dep$q,$nl").mkString})
             |""".stripMargin
      s"""import mill._
         |import mill.scalalib._
         |
         |object `$moduleName` extends JavaModule {
         |  $depsPart
         |}
         |
         |""".stripMargin
    }
    def singleFile(path: os.SubPath, source: String): (os.SubPath, Seq[(os.SubPath, String)]) = {
      val buildMill0 =
        if (path.last.endsWith(".java")) javaBuildMill("foo", Seq(source))
        else scalaBuildMill("foo", Seq(source))
      val path0 = os.sub / "foo/src" / path
      (path0, Seq(path0 -> source, (os.sub / "build.mill") -> buildMill0))
    }
    def singleModule(
      moduleName: String,
      files: Map[os.SubPath, String]
    ): (Map[os.SubPath, os.SubPath], Seq[(os.SubPath, String)]) = {
      def pathFor(basePath: os.SubPath) = os.sub / moduleName / "src" / basePath
      val map = files.map {
        case (path, _) =>
          (path, pathFor(path))
      }
      val sortedFiles = files.toVector.sortBy(_._1)
      val files0 = sortedFiles.map {
        case (path, content) =>
          (pathFor(path), content)
      }
      val buildMill0 =
        if (files.keysIterator.forall(_.last.endsWith(".java")))
          javaBuildMill(moduleName, sortedFiles.map(_._2))
        else scalaBuildMill(moduleName, sortedFiles.map(_._2))
      (map, files0 ++ Seq((os.sub / "build.mill", buildMill0)))
    }

    private lazy val millwPath = {
      val f = coursierapi.Cache.create()
        .get(coursierapi.Artifact.of(
          s"https://github.com/com-lihaoyi/mill/raw/${IntegrationConstants.millwCommit}/mill",
          true,
          false
        ))
      os.Path(f, os.pwd)
    }
    private lazy val millwBatPath = {
      val f = coursierapi.Cache.create()
        .get(coursierapi.Artifact.of(
          s"https://github.com/com-lihaoyi/mill/raw/${IntegrationConstants.millwCommit}/mill.bat",
          true,
          false
        ))
      os.Path(f, os.pwd)
    }
    def setup(
      workspace: os.Path,
      osOpt: Option[OutputStream],
      readOnlyToplevelSymbolsCache: Boolean,
      compiles: Boolean
    ): Unit =
      millSetup(
        workspace,
        osOpt,
        readOnlyToplevelSymbolsCache
      )
    def millSetup(
      workspace: os.Path,
      osOpt: Option[OutputStream],
      readOnlyToplevelSymbolsCache: Boolean
    ): Unit = {
      os.copy(millwPath, workspace / "mill")
      os.copy(millwBatPath, workspace / "mill.bat")
      os.write(workspace / ".mill-version", IntegrationConstants.millVersion)
      (workspace / "mill").toIO.setExecutable(true)
      TestUtil.runServerCommand(workspace, osOpt)(
        "build-tool",
        "add",
        ".",
        "--mill"
      )
      TestUtil.runServerCommand(workspace, osOpt)(
        "import",
        "--ignore-toplevel-symbols-errors=false",
        if (readOnlyToplevelSymbolsCache) Seq("--toplevel-cache-only") else Nil
      )
      TestUtil.runServerCommand(workspace, osOpt)("bsp", "compile")
    }
    def compile(
      workspace: os.Path,
      osOpt: Option[OutputStream]
    ): Unit =
      TestUtil.runServerCommand(workspace, osOpt)("bsp", "compile")
    override def isSlow: Boolean = true
  }

  case object Sbt extends SingleModuleBuildTool {
    def id          = "sbt"
    def displayName = "sbt"
    def scalaBuildSbt(moduleName: String, sources: Seq[String]) = {
      val sv = sources.iterator
        .flatMap(source => scalaVersion(source).iterator)
        .find(_ => true)
        .getOrElse {
          sys.error("No Scala version found in test source")
        }
      val deps = sources
        .flatMap(dependencies(_))
        .map { depStr =>
          val dep = DependencyParser.parse(depStr)
            .left.map(err => sys.error(s"Error parsing dependency '$depStr': $err"))
            .merge
          val sep = dep.nameAttributes match {
            case s: ScalaNameAttributes =>
              assert(!s.fullCrossVersion.contains(true), "TODO")
              assert(!s.platform.contains(true), "TODO")
              "%%"
            case NoAttributes => "%"
          }
          s""""${dep.organization}" $sep "${dep.name}" % "${dep.version}"
             |""".stripMargin
        }
        .mkString
      s"""
         |lazy val `$moduleName` = project.settings(
         |  scalaVersion := "$sv",
         |  libraryDependencies ++= Seq(
         |  $deps)
         |)
         |
         |""".stripMargin
    }
    def javaBuildSbt(moduleName: String, sources: Seq[String]) = {
      val deps = sources
        .flatMap(dependencies(_))
        .map { depStr =>
          val dep = DependencyParser.parse(depStr)
            .left.map(err => sys.error(s"Error parsing dependency '$depStr': $err"))
            .merge
          dep.nameAttributes match {
            case _: ScalaNameAttributes =>
              sys.error(s"Invalid Scala dependency in Java project: '$depStr'")
            case NoAttributes =>
          }
          s""""${dep.organization}" % "${dep.name}" % "${dep.version}"
             |""".stripMargin
        }
        .mkString
      s"""
         |lazy val `$moduleName` = project.settings(
         |  crossPaths := false,
         |  autoScalaLibrary := false,
         |  libraryDependencies ++= Seq(
         |  $deps)
         |)
         |
         |""".stripMargin
    }
    def singleFile(path: os.SubPath, source: String): (os.SubPath, Seq[(os.SubPath, String)]) = {
      val buildSbtContent =
        if (path.last.endsWith(".java")) javaBuildSbt("foo", Seq(source))
        else scalaBuildSbt("foo", Seq(source))
      val path0 = os.sub / "foo/src/main/scala" / path
      (path0, Seq(path0 -> source, (os.sub / "build.sbt") -> buildSbtContent))
    }
    def singleModule(
      moduleName: String,
      files: Map[os.SubPath, String]
    ): (Map[os.SubPath, os.SubPath], Seq[(os.SubPath, String)]) = {
      def pathFor(basePath: os.SubPath) = {
        val langDir =
          if (basePath.last.endsWith(".java")) "java"
          else "scala"
        os.sub / moduleName / "src/main" / langDir / basePath
      }
      val map = files.map {
        case (path, _) =>
          (path, pathFor(path))
      }
      val sortedFiles = files.toVector.sortBy(_._1)
      val files0 = sortedFiles.map {
        case (path, content) =>
          (pathFor(path), content)
      }
      val buildSbt =
        if (sortedFiles.nonEmpty && sortedFiles.forall(_._1.last.endsWith(".java")))
          javaBuildSbt(moduleName, sortedFiles.map(_._2))
        else
          scalaBuildSbt(moduleName, sortedFiles.map(_._2))
      (map, files0 ++ Seq((os.sub / "build.sbt", buildSbt)))
    }

    private lazy val sbtExtrasPath = {
      val f = coursierapi.Cache.create()
        .get(coursierapi.Artifact.of(
          s"https://github.com/dwijnand/sbt-extras/raw/${IntegrationConstants.sbtExtrasCommit}/sbt",
          true,
          false
        ))
      os.Path(f, os.pwd)
    }
    def setup(
      workspace: os.Path,
      osOpt: Option[OutputStream],
      readOnlyToplevelSymbolsCache: Boolean,
      compiles: Boolean
    ): Unit = {
      os.copy(sbtExtrasPath, workspace / "sbt")
      os.write(
        workspace / "project/build.properties",
        s"sbt.version=${IntegrationConstants.sbtVersion}",
        createFolders = true
      )
      (workspace / "sbt").toIO.setExecutable(true)
      TestUtil.runServerCommand(workspace, osOpt)("build-tool", "add", ".", "--sbt")
      TestUtil.runServerCommand(workspace, osOpt)(
        "import",
        ".",
        "--ignore-toplevel-symbols-errors=false",
        if (readOnlyToplevelSymbolsCache) Seq("--toplevel-cache-only") else Nil
      )
      TestUtil.runServerCommand(workspace, osOpt)("bsp", "compile", "--dumb-build-tool-hacks")
    }
    def compile(
      workspace: os.Path,
      osOpt: Option[OutputStream]
    ): Unit =
      TestUtil.runServerCommand(workspace, osOpt)("bsp", "compile", "--dumb-build-tool-hacks")
    override def isSlow: Boolean = true
  }

}
