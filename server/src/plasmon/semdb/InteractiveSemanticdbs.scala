// Originally based on https://github.com/scalameta/metals/blob/0ad0bc184f82dbd178d01f76913ea6bdfa98db14/metals/src/main/scala/scala/meta/internal/metals/InteractiveSemanticdbs.scala or an earlier version of that file

package plasmon.semdb

import java.util.Collections

import scala.util.Failure
import scala.util.Success
import scala.util.Try

import scala.meta.internal.mtags.MD5
import plasmon.semdb.TextDocumentLookup
import scala.meta.internal.{semanticdb => s}

import ch.epfl.scala.{bsp4j => b}
import scala.meta.internal.mtags.SourcePath
import plasmon.pc.PresentationCompilers

import plasmon.PlasmonEnrichments._
import plasmon.ide.Buffers
import plasmon.semdb.Semanticdbs
import scala.meta.internal.mtags.GlobalSymbolIndex

/** Produces SemanticDBs on-demand by using the presentation compiler.
  *
  * Only used to provide navigation inside external library sources, not used to compile workspace
  * sources.
  *
  * Uses persistent storage to keep track of what external source file is associated with what build
  * target (to determine classpath and compiler options).
  */
final class InteractiveSemanticdbs(
  workspace: os.Path,
  buffers: Buffers,
  onNewSemanticdb: (GlobalSymbolIndex.Module, os.Path, s.TextDocument) => Unit,
  computeInteractiveSemanticdb: (os.Path, String, b.BuildTargetIdentifier) => s.TextDocument
) extends AutoCloseable
    with Semanticdbs {

  private val textDocumentCache = Collections.synchronizedMap(
    new java.util.HashMap[os.Path, s.TextDocument]
  )

  def reset(): Unit =
    textDocumentCache.clear()

  override def close(): Unit =
    reset()

  override def textDocument(
    source: SourcePath,
    module: GlobalSymbolIndex.Module
  ): Either[String, TextDocumentLookup] = textDocument(source, unsavedContents = None, module)

  def onClose(path: os.Path): Unit =
    textDocumentCache.remove(path)

  def textDocument(
    sourcePath: SourcePath,
    unsavedContents: Option[String],
    module: GlobalSymbolIndex.Module
  ): Either[String, TextDocumentLookup] =
    sourcePath match {
      case _: SourcePath.ZipEntry =>
        // FIXME We should actually proceed here
        Left("No interactive semanticdbs for zip entries")
      case p: SourcePath.Standard =>
        val source = os.Path(p.path)
        lazy val sourceText =
          buffers.get(source).orElse {
            if (os.exists(source)) Some(os.read(source))
            else None
          }
        def shouldTryCalculateInteractiveSemanticdb =
          unsavedContents.isDefined ||
          source.isInReadonlyDirectory(workspace) || // dependencies
          source.isSbt ||                            // sbt files
          source.isMill ||                           // mill files
          // starts with shebang
          sourceText.exists(_.startsWith(Shebang.shebang))

        // anything aside from `*.scala`, `*.sbt`, `*.mill`, `*.sc`, `*.java` file
        def isExcludedFile = !source.isScalaFilename && !source.isJavaFilename

        if (isExcludedFile)
          Left("No interactive semanticdb, not a Scala or Java file")
        else if (!source.isSameFileSystem(workspace))
          Left("Not in the main file system")
        else if (!shouldTryCalculateInteractiveSemanticdb)
          Left("Not a file we should compute interactive semanticdbs for")
        else
          unsavedContents.orElse(sourceText) match {
            case None => Left(s"No content for $source")
            case Some(text) =>
              val adjustedText =
                if (text.startsWith(Shebang.shebang))
                  "//" + text.drop(2)
                else text
              def sha            = MD5.compute(adjustedText)
              val existingDocOpt = Option(textDocumentCache.get(source)).filter(_.md5 == sha)
              existingDocOpt match {
                case None =>
                  Try(computeInteractiveSemanticdb(
                    source,
                    adjustedText,
                    new b.BuildTargetIdentifier(module.targetId)
                  )) match {
                    case Success(doc) =>
                      if (doc == null)
                        scribe.warn(s"Got null semantic DB when compiling $source")
                      else {
                        textDocumentCache.put(source, doc)
                        if (!source.isDependencySource(workspace))
                          onNewSemanticdb(module, source, doc)
                      }

                      Right(TextDocumentLookup.Success(doc, source))
                    case Failure(ex) =>
                      Right(TextDocumentLookup.Error(ex, source.toNIO.toUri.toASCIIString))
                  }
                case Some(existingDoc) =>
                  Right(TextDocumentLookup.Success(existingDoc, source))
              }
          }
    }
}
