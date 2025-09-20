// Originally based on https://github.com/scalameta/metals/blob/2a6f8a437a1ce7c44140673edfe34bb74dfd33be/metals/src/main/scala/scala/meta/internal/metals/CodeLensProvider.scala or an earlier version of that file

package plasmon.handlers.codelens

import scala.concurrent.Future

import scala.meta.internal.mtags.GlobalSymbolIndex
import scala.meta.internal.semanticdb.TextDocument
import scala.meta.internal.mtags.SourcePath

import org.eclipse.{lsp4j => l}

trait CodeLensProvider {
  def isEnabled: Boolean

  def codeLenses(
    module: GlobalSymbolIndex.Module,
    path: SourcePath,
    textDocument: TextDocument
  ): Future[Seq[l.CodeLens]] =
    Future.successful(Seq.empty)
}
