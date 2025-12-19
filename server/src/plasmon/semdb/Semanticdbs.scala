// Originally based on https://github.com/scalameta/metals/blob/0ad0bc184f82dbd178d01f76913ea6bdfa98db14/mtags/src/main/scala/scala/meta/internal/mtags/Semanticdbs.scala or an earlier version of that file

package plasmon.semdb

import java.nio.charset.Charset

import scala.meta.inputs.Input
import scala.meta.inputs.Position
import scala.meta.internal.io.FileIO
import scala.meta.internal.mtags.{GlobalSymbolIndex, MD5, Md5Fingerprints, SourcePath}
import scala.meta.internal.mtags.ScalametaCommonEnrichments._
import scala.meta.internal.mtags.SymbolOccurrenceOrdering._
import scala.meta.internal.semanticdb.Scala._
import scala.meta.internal.semanticdb.XtensionSemanticdbSymbolInformation
import scala.meta.internal.{semanticdb => s}

import plasmon.PlasmonEnrichments._

trait Semanticdbs {
  def textDocument(
    path: SourcePath,
    module: GlobalSymbolIndex.Module
  ): Either[String, TextDocumentLookup]
  final def textDocument(
    path: os.Path,
    module: GlobalSymbolIndex.Module
  ): Either[String, TextDocumentLookup] =
    textDocument(SourcePath.Standard(path.toNIO), module)
}
object Semanticdbs {
  private def loadTextDocuments(path: os.Path): s.TextDocuments = {
    val in = os.read.inputStream(path)
    try s.TextDocuments.parseFrom(in)
    finally in.close()
  }

  def loadTextDocument(
    scalaOrJavaPath: SourcePath,
    sourceroot: os.Path,
    optScalaVersion: Option[String],
    charset: Charset,
    fingerprints: Md5Fingerprints,
    loader: os.SubPath => Either[String, FoundSemanticDbPath],
    log: String => Unit = _ => ()
  ): Either[String, TextDocumentLookup] =
    scalaOrJavaPath match {
      case s: SourcePath.Standard =>
        val scalaRelativePath =
          os.Path(s.path).relativeTo(sourceroot.dealias).asSubPath
        val semanticdbRelativePath =
          SemanticdbClasspath.fromScalaOrJava(scalaRelativePath)
        loader(semanticdbRelativePath).flatMap { semanticdbPath =>
          loadResolvedTextDocument(
            os.Path(s.path),
            semanticdbPath.nonDefaultRelPath.getOrElse(scalaRelativePath),
            semanticdbPath.path,
            optScalaVersion,
            charset,
            fingerprints,
            log
          )
        }
      case _ =>
        Left(s"${scalaOrJavaPath.uri} is not a standard file")
    }

  private def loadResolvedTextDocument(
    scalaPath: os.Path,
    scalaRelativePath: os.SubPath,
    semanticdbPath: os.Path,
    optScalaVersion: Option[String],
    charset: Charset,
    fingerprints: Md5Fingerprints,
    log: String => Unit
  ): Either[String, TextDocumentLookup] = {
    val reluri = scalaRelativePath.toUri(isDirectory = false).toString
    val sdocs  = loadTextDocuments(semanticdbPath)
    val docOpt =
      sdocs.documents.find(_.uri.replace("\\", "/") == reluri).orElse {
        if (scalaPath.last.endsWith(".java"))
          sdocs.documents.find(_.uri.replace("\\", "/").endsWith("/" + reluri))
        else
          None
      }
    docOpt match {
      case None =>
        Right(
          TextDocumentLookup.NoMatchingUri(
            scalaPath,
            semanticdbPath,
            sdocs,
            reluri,
            sdocs.documents.map(_.uri.replace("\\", "/"))
          )
        )
      case Some(sdoc) if os.exists(scalaPath) =>
        val text = os.read(scalaPath, charset)
        if (text.startsWith(Shebang.shebang))
          if (optScalaVersion.exists(_.startsWith("3")))
            Right {
              addIfStaleInfo(
                scalaPath,
                semanticdbPath,
                sdoc,
                Shebang.adjustContent(text),
                fingerprints,
                log
              )
            }
          else
            Left(
              s"$scalaPath starts with a shebang but the Scala version isn't Scala 3 or there's no Scala version"
            )
        else
          Right {
            addIfStaleInfo(
              scalaPath,
              semanticdbPath,
              sdoc,
              text,
              fingerprints,
              log
            )
          }
      case _ => Left(s"$scalaPath not found")
    }
  }

  private def addIfStaleInfo(
    scalaPath: os.Path,
    semanticdbPath: os.Path,
    sdoc: s.TextDocument,
    currentText: String,
    fingerprints: Md5Fingerprints,
    log: String => Unit
  ) = {
    val md5     = MD5.compute(currentText)
    val sdocMd5 = sdoc.md5.toUpperCase()
    if (sdocMd5 == md5)
      TextDocumentLookup.Success(sdoc.withText(currentText), semanticdbPath)
    else
      TextDocumentLookup.Stale(
        scalaPath,
        md5,
        fingerprints.lookupText(scalaPath.toAbsPath, sdocMd5) match {
          case Some(oldText) =>
            sdoc.withText(oldText)
          case None =>
            log(s"Could not load snapshot text for $scalaPath")
            sdoc
        }
      )
  }

  case class FoundSemanticDbPath(
    path: os.Path,
    nonDefaultRelPath: Option[os.SubPath]
  )
}
