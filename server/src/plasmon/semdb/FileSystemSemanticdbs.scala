// Originally based on https://github.com/scalameta/metals/blob/0ad0bc184f82dbd178d01f76913ea6bdfa98db14/metals/src/main/scala/scala/meta/internal/metals/FileSystemSemanticdbs.scala or an earlier version of that file

package plasmon.semdb

import plasmon.PlasmonEnrichments._

import scala.meta.internal.mtags.Md5Fingerprints
import plasmon.semdb.Semanticdbs.FoundSemanticDbPath
import plasmon.semdb.TextDocumentLookup
import scala.meta.io.RelativePath
import ch.epfl.scala.{bsp4j => b}
import scala.meta.internal.mtags.SourcePath
import plasmon.index.BspData
import scala.meta.internal.mtags.GlobalSymbolIndex
import java.nio.charset.StandardCharsets
import plasmon.semdb.Semanticdbs

/** Reads SemanticDBs from disk that are produces by the semanticdb-scalac compiler plugin.
  */
final class FileSystemSemanticdbs(
  bspData: BspData,
  fingerprints: Md5Fingerprints
) extends Semanticdbs {

  override def textDocument(
    source: SourcePath,
    module: GlobalSymbolIndex.Module
  ): Option[TextDocumentLookup] =
    source match {
      case z: SourcePath.ZipEntry => None
      case p: SourcePath.Standard =>
        val file = os.Path(p.path)
        bspData.inverseSources(file).flatMap(textDocument0(file, _).toOption)
    }

  def textDocument0(
    file: os.Path,
    targetId: b.BuildTargetIdentifier
  ): Either[String, TextDocumentLookup] =
    if (!file.isScala && !file.isJava)
      Left("Not a Scala or Java source")
    else {
      scribe.info("targetId.getUri=" + pprint.apply(targetId.getUri))
      val paths = for {
        workspace <- bspData
          .workspaceDirectory(targetId)
          .toRight(s"Workspace not found for target ${targetId.getUri}")
        _ = scribe.info("workspace=" + pprint.apply(workspace))
        targetroot <- {
          val javaRoot =
            if (file.toLanguage.isJava) bspData.javaTargetRoot(targetId)
            else None
          javaRoot.orElse(bspData.scalaTargetRoot(targetId)).toRight {
            val prefix =
              if (file.toLanguage.isJava) "Java or Scala"
              else "Scala"
            s"$prefix target root not found for target ${targetId.getUri}"
          }
        }
        _ = scribe.info("targetroot.toNIO=" + pprint.apply(targetroot.toNIO))
      } yield {
        if (!os.exists(targetroot))
          scribe.warn(s"Target root $targetroot does not exist")
        val optScalaVersion =
          if (file.toLanguage.isJava) None
          else bspData.scalaTarget(targetId).map(_.scalaVersion)

        (workspace, targetroot, optScalaVersion)
      }

      paths.flatMap {
        case (ws, targetroot, optScalaVersion) =>
          Semanticdbs.loadTextDocument(
            SourcePath.Standard(file.toNIO),
            ws,
            optScalaVersion,
            StandardCharsets.UTF_8,
            fingerprints,
            semanticdbRelativePath =>
              findSemanticDb(os.SubPath(semanticdbRelativePath.toNIO), targetroot, file, ws),
            (warn: String) => scribe.warn(warn)
          )
      }
    }

  private def findSemanticDb(
    semanticdbRelativePath: os.SubPath,
    targetroot: os.Path,
    file: os.Path,
    workspace: os.Path
  ): Either[String, FoundSemanticDbPath] = {
    val semanticdbpath = targetroot / semanticdbRelativePath
    scribe.info("semanticdbpath=" + pprint.apply(semanticdbpath))
    scribe.info("semanticdbpath.isFile=" + pprint.apply(os.isFile(semanticdbpath)))
    if (os.isFile(semanticdbpath)) Right(FoundSemanticDbPath(semanticdbpath, None))
    else {
      // needed in case sources are symlinked,
      val result = for {
        sourceRoot <- bspData.originalInverseSourceItem(file).toRight(
          s"$semanticdbpath not found"
        )
        relativeSourceRoot = sourceRoot.relativeTo(workspace)
        relativeFile       = file.relativeTo(sourceRoot.dealias)
        fullRelativePath   = (relativeSourceRoot / relativeFile).asSubPath
        alternativeRelativePath =
          SemanticdbClasspath.fromScalaOrJava(fullRelativePath)
        alternativeSemanticdbPath = targetroot / alternativeRelativePath
        _ <- {
          if (os.isFile(alternativeSemanticdbPath))
            Right(())
          else
            Left(s"Found neither $semanticdbpath nor $alternativeSemanticdbPath")
        }
      } yield FoundSemanticDbPath(
        alternativeSemanticdbPath,
        Some(fullRelativePath)
      )
      if (result.isLeft)
        scribe.debug(
          s"No text document found at for $file expected at $semanticdbpath"
        )
      result
    }
  }
}
