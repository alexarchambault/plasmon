// Originally based on https://github.com/scalameta/metals/blob/0ad0bc184f82dbd178d01f76913ea6bdfa98db14/metals/src/main/scala/scala/meta/internal/metals/JavaInteractiveSemanticdb.scala or an earlier version of that file

package plasmon.semdb

import ch.epfl.scala.bsp4j as b
import plasmon.PlasmonEnrichments.*
import plasmon.ide.Directories
import plasmon.index.BspData
import plasmon.internal.Constants

import java.io.{File, PrintWriter, StringWriter}
import java.util.zip.{ZipException, ZipFile}
import javax.tools.{Diagnostic, DiagnosticListener, JavaFileManager, JavaFileObject}

import scala.jdk.CollectionConverters.*
import scala.meta.internal.semanticdb as s
import scala.meta.internal.metals.JdkSources
import scala.meta.internal.mtags.{MD5, SourcePath}
import scala.meta.internal.pc.JavaMetalsGlobal
import scala.util.{Failure, Success, Try, Using}

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
    ): s.TextDocument =
      if (source.startsWith(workspace)) {
        val workDir    = os.temp.dir(prefix = "plasmon-javac-semanticdb").dealias
        val targetRoot = workDir / "target"
        os.makeDir(targetRoot)

        val targetClasspath = targetId match {
          case None =>
            bspData
              .inferBuildTarget(SourcePath.Standard(source.toNIO))
              .flatMap(bspData.targetJarClasspath)
              .getOrElse(Nil)
          case Some(targetId0) =>
            bspData
              .targetJarClasspath(targetId0)
              .getOrElse {
                sys.error(s"Build target ${targetId0.getUri} not loaded")
              }
        }

        val filteredTargetClassPath = targetClasspath.filter { elem =>
          !os.isFile(elem) || {
            try
              Using.resource(new ZipFile(elem.toIO)) { zf =>
                // Keep this JAR only if it doesn't look like another java semdb plugin
                zf.getEntry("com/sourcegraph/semanticdb_javac/SemanticdbPlugin.class") == null
              }
            catch {
              case _: ZipException =>
                true
            }
          }
        }

        val extraOptions = Option(System.getenv("PLASMON_JAVAC_EXTRA_OPTIONS"))
          .toList
          .flatMap(_.split(',').toList)
        val jigsawOptions = patchModuleFlags(source, workspace, source)
        val mainOptions =
          List(
            "-cp",
            (pluginJars ++ filteredTargetClassPath).mkString(File.pathSeparator),
            "-d",
            targetRoot.toString
            // "-verbose"
          )
        val pluginOption =
          s"-Xplugin:semanticdb -sourceroot:$workspace -targetroot:$targetRoot"
        val allOptions =
          mainOptions ::: jigsawOptions ::: extraOptions ::: pluginOption :: Nil

        val writer      = new StringWriter
        val printWriter = new PrintWriter(writer)
        val successOpt =
          try {
            // JavacFileManager#getLocationForModule specifically tests that JavaFileObject is instanceof PathFileObject when using Patch-Module
            // so can't use Metals SourceJavaFileObject
            val javaFileObject = JavaMetalsGlobal.makeFileObject(source.toIO)

            // We call COMPILER.getTask directly (rather than via classpathCompilationTask)
            // so we can pass a capturing DiagnosticListener. classpathCompilationTask hardcodes
            // a noopDiagnosticListener that silently drops all errors; the out Writer only
            // receives "additional notes" from javac, not diagnostic messages.
            val diagnosticListener: DiagnosticListener[JavaFileObject] =
              (d: Diagnostic[? <: JavaFileObject]) =>
                printWriter.println(
                  s"${d.getKind}: ${d.getMessage(null)}" +
                    Option(d.getSource)
                      .map(src => s" (${src.getName}:${d.getLineNumber})")
                      .getOrElse("")
                )
            val javacTask = JavaMetalsGlobal.COMPILER
              .getTask(
                printWriter,
                javaFileManager(),
                diagnosticListener,
                allOptions.asJava,
                null,
                java.util.List.of(javaFileObject)
              )
              .asInstanceOf[com.sun.source.util.JavacTask]

            Some(javacTask.call())
          }
          catch {
            case e: Throwable =>
              scribe.error(
                s"Can't run javac on $source with options: [${allOptions.mkString("\n")}]",
                e
              )
              None
          }
        for (success <- successOpt if !success) {
          printWriter.flush()
          val log = writer.getBuffer
          scribe.error(
            s"Error running javac on $source with options: " +
              s"[${allOptions.mkString("\n")}]: $log"
          )
        }

        val semanticdbFile = {
          val subPath = source.subRelativeTo(workspace)
          targetRoot / "META-INF/semanticdb" / (subPath / os.up) / s"${subPath.last}.semanticdb"
        }

        val doc =
          if (os.exists(semanticdbFile))
            readAllDocuments(semanticdbFile).headOption.getOrElse(s.TextDocument())
          else {
            printWriter.flush()
            val log   = writer.getBuffer
            val files = os.walk(targetRoot).toVector.map(_.subRelativeTo(targetRoot))
            scribe.warn(
              s"Running javac-semanticdb failed for ${source.toNIO.toUri.toASCIIString}, " +
                s"options: [${allOptions.mkString("\n")}]. " +
                s"Output:\n$log\nFiles:\n${files.map(_.toString + "\n")}"
            )
            s.TextDocument()
          }

        val out = doc.copy(
          uri = source.subRelativeTo(workspace).toString,
          text = text,
          md5 = MD5.compute(text)
        )

        os.remove.all(workDir)
        out
      }
      else {
        scribe.error(s"Cannot get Java interactive semanticdb for $source: not in $workspace")
        s.TextDocument()
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
          logger.accept {
            val jars = if (pluginJars.length == 1) "JAR" else "JARs"
            s"${pluginJars.length} Java semanticdb plugin $jars"
          }
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
