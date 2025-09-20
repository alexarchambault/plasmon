// Originally based on https://github.com/scalameta/metals/blob/0ad0bc184f82dbd178d01f76913ea6bdfa98db14/metals/src/main/scala/scala/meta/internal/metals/MetalsSymbolSearch.scala or earlier versions of that file

package plasmon.index

import java.net.URI
import java.util.{Collections, List => JList, Optional}

import scala.collection.concurrent.TrieMap

import scala.meta.internal.mtags.Mtags
import scala.meta.pc.ContentType
import scala.meta.pc.ParentSymbols
import scala.meta.pc.SymbolDocumentation
import scala.meta.pc.SymbolSearch
import scala.meta.pc.SymbolSearchVisitor

import org.eclipse.{lsp4j => l}
import scala.meta.internal.mtags.Symbol
import scala.meta.internal.mtags.SourcePath
import scala.meta.internal.mtags.GlobalSymbolIndex
import scala.meta.internal.metals.Docstrings
import scala.meta.internal.metals.WorkspaceSymbolQuery

import scala.jdk.CollectionConverters._

import plasmon.index.SymbolSearchIndex
import plasmon.ide.PatchedSymbolIndex
import plasmon.PlasmonEnrichments.XtensionOsPath
import plasmon.PlasmonEnrichments.StringThingExtensions

class SymbolSearchImpl(
  docs: Docstrings,
  wsp: SymbolSearchIndex,
  patchedSymbolIndex: PatchedSymbolIndex,
  index: GlobalSymbolIndex
) extends SymbolSearch {
  // A cache for definitionSourceToplevels.
  // The key is an absolute path to the dependency source file, and
  // the value is the list of symbols that the file contains.
  private val dependencySourceCache = new TrieMap[os.Path, JList[String]]

  def reset(): Unit = {
    dependencySourceCache.clear()
  }

  override def documentation(
    moduleString: String,
    symbol: String,
    parents: ParentSymbols,
    logger: java.util.function.Consumer[String]
  ): Optional[SymbolDocumentation] =
    documentation(moduleString, symbol, parents, ContentType.MARKDOWN, logger)

  override def documentation(
    moduleString: String,
    symbol: String,
    parents: ParentSymbols,
    contentType: ContentType,
    logger: java.util.function.Consumer[String]
  ): Optional[SymbolDocumentation] =
    SourcePath.withContext { implicit ctx =>
      docs.documentation(
        GlobalSymbolIndex.Module.fromString(moduleString),
        symbol,
        parents,
        contentType,
        logger
      )
    }

  def definition(
    moduleString: String,
    symbol: String,
    source: URI
  ): JList[l.Location] =
    SourcePath.withContext { implicit ctx =>
      patchedSymbolIndex.fromSymbol(
        GlobalSymbolIndex.Module.fromString(moduleString),
        symbol
      )
    }

  /** Returns a list of semanticdb symbols in a source file that contains the definition of the
    * given symbol.
    */
  override def definitionSourceToplevels(
    moduleString: String,
    symbol: String,
    source: URI
  ): JList[String] =
    SourcePath
      .withContext { implicit ctx =>
        val module = GlobalSymbolIndex.Module.fromString(moduleString)
        index
          .definitions(module, Symbol(symbol))
          .find(_.path.exists())
          // FIXME path is a SourcePath, might be a zip
          .map(_.path.uri.osPathFromUri)
      }
      .map { path =>
        dependencySourceCache.getOrElseUpdate(
          path,
          Mtags.topLevelSymbols(
            path.toAbsPath // FIXME Dialect
          ).asJava
        )
      }
      .getOrElse(Collections.emptyList())

  override def search(
    query: String,
    moduleString: String,
    visitor: SymbolSearchVisitor,
    ctx: scala.meta.pc.SourcePathContext
  ): SymbolSearch.Result = {
    implicit val ctx0: SourcePath.Context = SourcePath.Context.from(ctx)
    def search(query: WorkspaceSymbolQuery) =
      wsp.search(
        query,
        visitor,
        GlobalSymbolIndex.Module.fromString(moduleString)
      )

    val wQuery          = WorkspaceSymbolQuery.exact(query)
    val (result, count) = search(wQuery)
    if (wQuery.isShortQuery && count == 0)
      search(WorkspaceSymbolQuery.exact(query, isShortQueryRetry = true))._1
    else result
  }

  override def searchMethods(
    query: String,
    moduleString: String,
    visitor: SymbolSearchVisitor
  ): SymbolSearch.Result =
    wsp.searchMethods(
      query,
      visitor,
      GlobalSymbolIndex.Module.fromString(moduleString)
    )
}
