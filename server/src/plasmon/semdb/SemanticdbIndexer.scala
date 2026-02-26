// Originally based on https://github.com/scalameta/metals/blob/0ad0bc184f82dbd178d01f76913ea6bdfa98db14/metals/src/main/scala/scala/meta/internal/metals/SemanticdbIndexer.scala or an earlier version of that file

package plasmon.semdb

import com.google.protobuf.InvalidProtocolBufferException
import plasmon.PlasmonEnrichments.*
import plasmon.ide.Directories
import plasmon.index.BspData

import java.nio.file.FileSystemException

import scala.meta.internal.mtags.GlobalSymbolIndex
import scala.meta.internal.semanticdb.{TextDocument, TextDocuments}
import scala.util.control.NonFatal

class SemanticdbIndexer(
  providers: List[SemanticdbIndexer.FeatureProvider],
  bspData: BspData,
  workspace: os.Path,
  logger: java.util.function.Consumer[String]
) {

  def onTargetRoots: Unit = {
    logger.accept("Looking for semanticdb file changes")
    for (targetRoot <- bspData.allTargetRoots)
      onChangeDirectory(targetRoot / Directories.semanticdb)
  }

  def reset(): Unit =
    providers.foreach(_.reset())

  def onDelete(semanticdbFile: os.Path): Unit =
    SemanticdbClasspath.toScala(workspace, semanticdbFile).foreach {
      sourceFile =>
        providers.foreach(_.onDelete(sourceFile))
    }

  /** Handle EventType.OVERFLOW
    *
    * The overflow events comes either with a non-null or null path.
    *
    * In case the path is not null, we walk up the file tree to the parent `META-INF/semanticdb`
    * parent directory and re-index all of its `*.semanticdb` children.
    *
    * In case of a null path, we re-index `META-INF/semanticdb` for all targets
    */
  def onOverflow(path: os.Path): Unit =
    for (dir <- SemanticdbClasspath.semanticdbRoot(path))
      onChangeDirectory(dir)

  private def semanticdbFiles(dir: os.Path): Option[Seq[os.Path]] =
    Option.when(os.isDir(dir)) {
      os.walk(dir).filter(_.isSemanticdb)
    }

  private def onChangeDirectory(dir: os.Path): Unit =
    if (os.isDir(dir)) {
      def javaTargets  = bspData.allJava.filter(target => dir.startsWith(target.classDirectory))
      def scalaTargets = bspData.allScala.filter(target => dir.startsWith(target.classDirectory))

      def moduleClassDirs =
        javaTargets.map(target =>
          target.info.getId.module -> target.classDirectory
        ) ++
          scalaTargets.map(target =>
            target.info.getId.module -> target.classDirectory
          )

      for ((module, classDir) <- moduleClassDirs if os.isDir(classDir)) {
        val semdbs = os.walk(classDir)
          .filter(_.isSemanticdb)
          .filter(os.isFile)
        logger.accept {
          if (semdbs.isEmpty)
            s"Found no semanticdb files in $dir"
          else
            s"Found ${semdbs.length} semanticdb " +
              s"${if (semdbs.length == 1) "file" else "files"} in $dir"
        }
        for (file <- semdbs)
          onChange(module, file)
      }
    }
    else if (os.exists(dir))
      logger.accept(s"Cannot look for semanticdb files in $dir (not a directory)")
    else
      logger.accept(s"Cannot look for semanticdb files in $dir (not found)")

  def onChange(module: GlobalSymbolIndex.Module, path: os.Path, textDocument: TextDocument): Unit =
    onChange(module, path, TextDocuments(Seq(textDocument)))

  private def onChange(module: GlobalSymbolIndex.Module, path: os.Path, docs: TextDocuments): Unit =
    for (provider <- providers)
      provider.onChange(module, path, docs)

  def onChange(module: GlobalSymbolIndex.Module, file: os.Path): Unit =
    if (!os.isDir(file))
      try
        SemanticdbClasspath.toScala(workspace, file).foreach { sourceFile =>
          val docs = TextDocuments.parseFrom(os.read.bytes(file))
          onChange(module, sourceFile, docs)
        }
      catch {
        /* @note in some cases file might be created or modified, but not actually finished
         * being written. In that case, exception here is expected and a new event will
         * follow after it was finished.
         */
        case e: InvalidProtocolBufferException =>
          scribe.debug(s"$file is not yet ready", e)
        /* @note FileSystemException is thrown on Windows instead of InvalidProtocolBufferException */
        case e: FileSystemException =>
          scribe.debug(s"$file is not yet ready", e)
        case NonFatal(e) =>
          scribe.warn(s"unexpected error processing the file $file", e)
      }
}

object SemanticdbIndexer {
  trait FeatureProvider {
    def onChange(module: GlobalSymbolIndex.Module, path: os.Path, docs: TextDocuments): Unit
    def onDelete(path: os.Path): Unit
    def reset(): Unit
  }
}
