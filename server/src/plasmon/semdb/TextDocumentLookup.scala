// Originally based on https://github.com/scalameta/metals/blob/e071d44324016086e32bfafde3d2da448c9e244c/mtags/src/main/scala/scala/meta/internal/mtags/TextDocumentLookup.scala or an earlier version of that file

package plasmon.semdb

import scala.meta.internal.{semanticdb => s}

sealed abstract class TextDocumentLookup {
  case class MissingSemanticdb(uri: String)
      extends Exception(s"missing SemanticDB: $uri")
  case class StaleSemanticdb(file: os.Path)
      extends Exception(s"stale SemanticDB: $file")
  final def isNotFound: Boolean =
    this.isInstanceOf[TextDocumentLookup.NotFound]
  final def isSuccess: Boolean =
    this.isInstanceOf[TextDocumentLookup.Success]
  final def documentIncludingStale: Option[s.TextDocument] =
    this match {
      case TextDocumentLookup.Success(doc, _)  => Some(doc)
      case TextDocumentLookup.Stale(_, _, doc) => Some(doc)
      case TextDocumentLookup.Aggregate(results) =>
        results.flatMap(_.documentIncludingStale).headOption
      case _ => None
    }
  final def documentIncludingStaleE: Either[String, s.TextDocument] =
    this match {
      case TextDocumentLookup.Success(doc, _)  => Right(doc)
      case TextDocumentLookup.Stale(_, _, doc) => Right(doc)
      case TextDocumentLookup.Aggregate(results) =>
        val results0 = results.map(_.documentIncludingStaleE)
        results0
          .collectFirst {
            case Right(doc) => doc
          }
          .toRight {
            val errors = results0.flatMap(_.left.toSeq)
            s"Multiple errors: ${errors.mkString(", ")}"
          }
      case err: TextDocumentLookup.Error =>
        Left(s"${err.uri}: ${err.e}")
      case err: TextDocumentLookup.NotFound =>
        Left(s"Not found: ${err.uri}")
      case err: TextDocumentLookup.NoMatchingUri =>
        Left(s"No matching URI for ${err.file}")
    }
  final def toOption: Option[s.TextDocument] =
    this match {
      case TextDocumentLookup.Success(document, _) =>
        Some(document)
      case _ => None
    }
  final def get: s.TextDocument =
    getE match {
      case Left(e)  => throw e
      case Right(v) => v
    }
  final def getE: Either[Throwable, s.TextDocument] =
    this match {
      case TextDocumentLookup.Success(document, _) =>
        Right(document)
      case TextDocumentLookup.NotFound(file) =>
        Left(MissingSemanticdb(file))
      case n: TextDocumentLookup.NoMatchingUri =>
        Left(MissingSemanticdb(n.file.toNIO.toUri.toASCIIString))
      case TextDocumentLookup.Stale(file, _, _) =>
        Left(StaleSemanticdb(file))
      case TextDocumentLookup.Error(e, _) =>
        Left(e)
      case TextDocumentLookup.Aggregate(errors) =>
        val e = new Exception("Errors loading SemanticDB")
        errors.foreach { error =>
          error.getE.left.foreach(exception => e.addSuppressed(exception))
        }
        Left(e)
    }
}
object TextDocumentLookup {
  def fromOption(
    path: os.Path,
    doc: Option[s.TextDocument]
  ): Option[TextDocumentLookup] =
    doc.map(Success(_, path))
  case class Success(document: s.TextDocument, path: os.Path)
      extends TextDocumentLookup
  case class Aggregate(errors: List[TextDocumentLookup])
      extends TextDocumentLookup
  case class Error(e: Throwable, uri: String) extends TextDocumentLookup
  case class NotFound(uri: String)            extends TextDocumentLookup
  case class NoMatchingUri(
    file: os.Path,
    semdbPath: os.Path,
    documents: s.TextDocuments,
    expectedRelUri: String,
    foundRelUris: Seq[String]
  ) extends TextDocumentLookup
  case class Stale(
    file: os.Path,
    expectedMd5: String,
    document: s.TextDocument
  ) extends TextDocumentLookup
}
