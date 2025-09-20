// Originally based on https://github.com/scalameta/metals/blob/2a6f8a437a1ce7c44140673edfe34bb74dfd33be/metals/src/main/scala/scala/meta/internal/metals/codelenses/SuperMethodCodeLens.scala or an earlier version of that file

package plasmon.handlers.codelens

import scala.concurrent.ExecutionContext
import scala.concurrent.Future

import plasmon.ide.ServerCommands
import scala.meta.internal.mtags.GlobalSymbolIndex
import scala.meta.internal.semanticdb.Scala._
import scala.meta.internal.semanticdb.SymbolInformation
import scala.meta.internal.semanticdb.SymbolOccurrence

import org.eclipse.{lsp4j => l}

import plasmon.PlasmonEnrichments._
import plasmon.ide.{Buffers, Trees}
import scala.meta.internal.semanticdb.TextDocument
import scala.meta.internal.mtags.SourcePath

private final class SuperMethodCodeLens(
  buffers: Buffers,
  trees: Trees
)(implicit val ec: ExecutionContext)
    extends CodeLensProvider {

  override def isEnabled: Boolean = true

  override def codeLenses(
    module: GlobalSymbolIndex.Module,
    path: SourcePath,
    textDocument: TextDocument
  ): Future[Seq[l.CodeLens]] = Future {
    def search(query: String) = textDocument.symbols.find(_.symbol == query)

    val distance = SourcePath.withContext { implicit ctx =>
      buffers.tokenEditDistance(module, path, textDocument.text, trees)
    }

    for {
      occurrence <- textDocument.occurrences
      if occurrence.role.isDefinition
      symbol = occurrence.symbol
      gotoSuperMethod <- createSuperMethodCommand(
        module,
        symbol,
        search,
        textDocument,
        path
      ).toSeq
      range <-
        occurrence.range
          .flatMap(r => distance.toRevisedStrict(r).map(_.toLsp))
          .toList
    } yield new l.CodeLens(range, gotoSuperMethod, null)
  }

  private def createSuperMethodCommand(
    module: GlobalSymbolIndex.Module,
    symbol: String,
    findSymbol: String => Option[SymbolInformation],
    textDocument: TextDocument,
    path: SourcePath
  ): Option[l.Command] =
    for {
      symbolInformation <- findSymbol(symbol)
      gotoParentSymbol <- SuperMethodProvider.findSuperForMethodOrField(
        symbolInformation
      )
      command <- convertToSuperMethodCommand(
        module,
        gotoParentSymbol,
        symbolInformation.displayName,
        textDocument,
        path
      )
    } yield command

  private def convertToSuperMethodCommand(
    module: GlobalSymbolIndex.Module,
    symbol: String,
    name: String,
    textDocument: TextDocument,
    path: SourcePath
  ): Option[l.Command] =
    if (symbol.isLocal)
      textDocument.occurrences.collectFirst {
        case SymbolOccurrence(
              Some(range),
              `symbol`,
              SymbolOccurrence.Role.DEFINITION
            ) =>
          val location = new l.Location(path.uri, range.toLsp)
          val command  = ServerCommands.GotoPosition.toLsp(location)
          command.setTitle(s"$$(arrow-up) $name")
          command
      }
    else
      Some {
        val command = ServerCommands.GotoSymbol.toLsp(
          ServerCommands.GotoSymbolParams(symbol, module.asString)
        )
        command.setTitle(s"$$(arrow-up) $name")
        command
      }

}
