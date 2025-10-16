// Originally based on https://github.com/scalameta/metals/blob/0ad0bc184f82dbd178d01f76913ea6bdfa98db14/metals/src/main/scala/scala/meta/internal/metals/AggregateSemanticdbs.scala

package plasmon.semdb

import plasmon.semdb.{Semanticdbs, TextDocumentLookup}

import scala.meta.internal.mtags.SourcePath
import scala.meta.internal.mtags.GlobalSymbolIndex
import scala.util.control.NonFatal

/** Implements `TextDocuments` trait with a list of underlying implementations.
  */
final case class AggregateSemanticdbs(underlying: List[Semanticdbs])
    extends Semanticdbs {
  override def textDocument(
    path: SourcePath,
    module: GlobalSymbolIndex.Module
  ): Either[String, TextDocumentLookup] = {
    def loop(
      xs: List[Semanticdbs],
      errors: List[TextDocumentLookup]
    ): Either[String, TextDocumentLookup] =
      xs match {
        case Nil =>
          errors match {
            case Nil =>
              Right(TextDocumentLookup.NotFound(path.uri))
            case head :: Nil =>
              Right(head)
            case errors =>
              Right(TextDocumentLookup.Aggregate(errors))
          }
        case head :: tail =>
          head.textDocument(path, module) match {
            case Right(result) =>
              if (result.isSuccess) Right(result)
              else
                loop(tail, result :: errors)
            case Left(err) =>
              loop(tail, TextDocumentLookup.Error(new Exception(err), path.uri) :: errors)
          }
      }
    try
      loop(underlying, Nil)
    catch {
      case NonFatal(e) =>
        scribe.error(s"text document: ${path.uri}", e)
        Right(TextDocumentLookup.Error(e, path.uri))
    }
  }
}
