// Originally based on https://github.com/scalameta/metals/blob/0ad0bc184f82dbd178d01f76913ea6bdfa98db14/metals/src/main/scala/scala/meta/internal/metals/JavaInteractiveSemanticdb.scala or an earlier version of that file

package plasmon.semdb

import java.io.File
import java.io.PrintWriter
import java.io.StringWriter

import scala.util.Failure
import scala.util.Properties
import scala.util.Success
import scala.util.Try

import scala.meta.internal.mtags.MD5
import scala.meta.internal.mtags.SourcePath
import scala.meta.internal.pc.JavaMetalsGlobal
import scala.meta.internal.{semanticdb => s}
import ch.epfl.scala.{bsp4j => b}
import javax.tools.JavaFileManager
import plasmon.ide.Directories
import scala.meta.internal.metals.JdkSources
import plasmon.index.BspData
import plasmon.internal.Constants

import plasmon.PlasmonEnrichments._
import scala.jdk.CollectionConverters._

trait JavaInteractiveSemanticdb {
  def textDocument(
    source: os.Path,
    text: String,
    targetId: Option[b.BuildTargetIdentifier] = None
  ): s.TextDocument
}

object JavaInteractiveSemanticdb {

  private class NoopJavaInteractiveSemanticdb extends JavaInteractiveSemanticdb {
    override def textDocument(
      source: os.Path,
      text: String,
      targetId: Option[b.BuildTargetIdentifier] = None
    ): s.TextDocument =
      s.TextDocument()
  }

  private class DownloadedJavaInteractiveSemanticdb(
    javaFileManager: () => JavaFileManager,
    pluginJars: List[os.Path],
    workspace: os.Path,
    workingDir: os.Path,
    bspData: BspData
  ) extends JavaInteractiveSemanticdb {

    private val readonly = workingDir / Directories.readonly

    def textDocument(
      source: os.Path,
      text: String,
      targetId: Option[b.BuildTargetIdentifier] = None
    ): s.TextDocument = {
      val workDir    = os.temp.dir(prefix = "plasmon-javac-semanticdb").dealias
      val targetRoot = workDir / "target"
      os.makeDir(targetRoot)

      val localSource =
        if (source.isSameFileSystem(workspace))
          source
        else {
          val sourceRoot = workDir / "source"
          os.makeDir(sourceRoot)
          val localSource = sourceRoot / source.last
          os.write(localSource, text)
          localSource
        }

      val sourceRoot = localSource / os.up
      val targetClasspath = targetId match {
        case None =>
          bspData
            .inferBuildTarget(SourcePath.Standard(source.toNIO))
            .flatMap(bspData.targetJarClasspath)
            .getOrElse(Nil)
            .map(_.toString)
        case Some(targetId0) =>
          bspData
            .targetJarClasspath(targetId0)
            .getOrElse {
              sys.error(s"Build target ${targetId0.getUri} not loaded")
            }
            .map(_.toString)
      }

      val extraOptions =
        Option(System.getenv("PLASMON_JAVAC_EXTRA_OPTIONS")).toList
          .flatMap(_.split(',').toList)
      val jigsawOptions = patchModuleFlags(localSource, sourceRoot, source)
      val mainOptions =
        List(
          "-cp",
          (pluginJars ++ targetClasspath).mkString(File.pathSeparator),
          "-d",
          targetRoot.toString
          // "-verbose"
        )
      val pluginOption =
        s"-Xplugin:semanticdb -sourceroot:$sourceRoot -targetroot:$targetRoot"
      val allOptions =
        mainOptions ::: jigsawOptions ::: extraOptions ::: pluginOption :: Nil

      val writer      = new StringWriter()
      val printWriter = new PrintWriter(writer)
      val successOpt =
        try {
          // JavacFileManager#getLocationForModule specifically tests that JavaFileObject is instanceof PathFileObject when using Patch-Module
          // so can't use Metals SourceJavaFileObject
          val javaFileObject = JavaMetalsGlobal.makeFileObject(localSource.toIO)

          val javacTask = JavaMetalsGlobal.classpathCompilationTask(
            javaFileObject,
            Some(printWriter),
            allOptions,
            javaFileManager()
          )

          Some(javacTask.call())
        }
        catch {
          case e: Throwable =>
            scribe.error(
              s"Can't run javac on $localSource with options: [${allOptions.mkString("\n")}]",
              e
            )
            None
        }
      for (success <- successOpt if !success) {
        printWriter.flush()
        val log = writer.getBuffer
        scribe.error(
          s"Error running javac on $localSource with options: " +
            s"[${allOptions.mkString("\n")}]: $log"
        )
      }

      val semanticdbFile =
        targetRoot / "META-INF/semanticdb" / s"${localSource.last}.semanticdb"

      val doc =
        if (os.exists(semanticdbFile))
          readAllDocuments(semanticdbFile).headOption.getOrElse(s.TextDocument())
        else {
          printWriter.flush()
          val log         = writer.getBuffer
          val targetRoot0 = os.Path(targetRoot.toNIO, os.pwd)
          val files       = os.walk(targetRoot0).toVector.map(_.relativeTo(targetRoot0))
          scribe.warn(
            s"Running javac-semanticdb failed for ${source.toNIO.toUri.toASCIIString}, " +
              s"options: [${allOptions.mkString("\n")}]. " +
              s"Output:\n$log\nFiles:\n${files.map(_.toString + "\n")}"
          )
          s.TextDocument()
        }

      val documentSource = Try(workspace.toNIO.relativize(source.toNIO)).toOption
        .map { relativeUri =>
          val relativeString =
            if (Properties.isWin) relativeUri.toString().replace("\\", "/")
            else relativeUri.toString()
          relativeString
        }
        .getOrElse(source.toString())

      val out = doc.copy(
        uri = documentSource,
        text = text,
        md5 = MD5.compute(text)
      )

      os.remove.all(workDir)
      out
    }

    private def readAllDocuments(path: os.Path): Seq[s.TextDocument] = {
      val stream = os.read.inputStream(path)
      try s.TextDocuments.parseFrom(stream).documents
      finally stream.close()
    }

    private def patchModuleFlags(
      source: os.Path,
      sourceRoot: os.Path,
      originalSource: os.Path
    ): List[String] =
      // Jigsaw doesn't allow compiling source with package
      // that is declared in some existing module.
      // It fails with: `error: package exists in another module: $packageName`
      // but it might be fixed by passing `--patch-module $moduleName=$sourceRoot` option.
      //
      // Currently there is no infrastucture to detect if package belong to jigsaw module or not
      // so this case is covered only for JDK sources.
      if (source.startsWith(readonly))
        source.relativeTo(readonly).segments.toList match {
          case Directories.dependenciesName :: JdkSources.zipFileName :: moduleName :: _ =>
            List("--patch-module", s"$moduleName=$sourceRoot")
          case _ =>
            Nil
        }
      else if (originalSource.jarPath.exists(_.last == JdkSources.zipFileName))
        originalSource
          .segments
          .find(_ => true)
          .map { moduleName =>
            List("--patch-module", s"$moduleName=$sourceRoot")
          }
          .getOrElse(Nil)
      else
        Nil
  }

  def create(
    javaFileManager: () => JavaFileManager,
    workspace: os.Path,
    workingDir: os.Path,
    bspData: BspData,
    logger: java.util.function.Consumer[String]
  ): JavaInteractiveSemanticdb =
    Try(downloadSemanticdbJavac) match {
      case Failure(exception) =>
        scribe.error(
          "Failed to download semanticdb-javac, Java semanticdb for standalone files will not be available",
          exception
        )
        new NoopJavaInteractiveSemanticdb
      case Success(pluginJars) =>
        if (pluginJars.isEmpty)
          logger.accept("No plugin JARs")
        else {
          logger.accept(
            s"${pluginJars.length} plugin ${if (pluginJars.length == 1) "JAR"
              else "JARs"}"
          )
          for (jar <- pluginJars)
            logger.accept(s"  $jar")
          logger.accept("")
        }
        new DownloadedJavaInteractiveSemanticdb(
          javaFileManager,
          pluginJars,
          workspace,
          workingDir,
          bspData
        )
    }

  private def downloadSemanticdbJavac: List[os.Path] =
    coursierapi.Fetch.create()
      .withDependencies(
        coursierapi.Dependency.of(
          "com.sourcegraph",
          "semanticdb-javac",
          Constants.semanticdbJavaVersion
        )
      )
      .fetch()
      .asScala
      .toList
      .map(os.Path(_))
}
