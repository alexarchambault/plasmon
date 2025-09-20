// Based on https://github.com/scalameta/metals/blob/0ad0bc184f82dbd178d01f76913ea6bdfa98db14/metals/src/main/scala/scala/meta/internal/metals/DefinitionProvider.scala

package plasmon.ide

import java.util.{List => JList}

import scala.jdk.CollectionConverters.*
import scala.meta.internal.mtags.GlobalSymbolIndex
import scala.meta.internal.mtags.Mtags
import scala.meta.internal.mtags.SourcePath
import scala.meta.internal.mtags.Symbol
import scala.meta.internal.mtags.SymbolDefinition
import scala.meta.internal.semanticdb.TextDocument

import org.eclipse.{lsp4j => l}
import plasmon.PlasmonEnrichments._
import plasmon.ide.*
import plasmon.index.BspData
import plasmon.pc.NopReportContext
import plasmon.semdb.Semanticdbs

/** Implements goto definition that works even in code that doesn't parse.
  *
  * Uses token edit-distance to align identifiers in the current open buffer with symbol occurrences
  * from the latest SemanticDB snapshot.
  *
  * The implementation logic for converting positions between the latest SemanticDB snapshot and
  * current open buffer is quite hairy. We need to convert positions in both the "source" (where
  * definition request is made) and the "destination" (location of the symbol definition). This
  * requires using token edit distance twice:
  *
  *   - source: dirty buffer -> snapshot
  *   - destination: snapshot -> dirty buffer
  */
final class PatchedSymbolIndex(
  index: GlobalSymbolIndex,
  workspace: os.Path,
  buffers: Buffers,
  semanticdbs: () => Semanticdbs,
  trees: Trees,
  saveDefFileToDisk: Boolean,
  bspData: BspData
) {

  // stateless
  private val sourceMapper = SourceMapper(
    bspData,
    buffers
  )

  private def bestTextDocument(
    module: GlobalSymbolIndex.Module,
    symbolDefinition: SymbolDefinition
  )(implicit ctx: SourcePath.Context): TextDocument = {
    // Read text file from disk instead of editor buffers because the file
    // on disk is more likely to parse.
    lazy val parsed =
      new Mtags()(NopReportContext).index(
        symbolDefinition.path.toLanguage,
        symbolDefinition.path.toInput,
        symbolDefinition.dialectOpt
      )

    val path = symbolDefinition.path
    if (path.isScalaScript || parsed.occurrences.isEmpty) {
      // Fall back to SemanticDB on disk, if any

      def fromSemanticdbs(p: SourcePath): Option[TextDocument] =
        semanticdbs().textDocument(p, module).flatMap(_.documentIncludingStale)

      fromSemanticdbs(path)
        .orElse(
          sourceMapper.mappedTo(path).flatMap(fromSemanticdbs)
        )
        .getOrElse(parsed)

    }
    else
      parsed
  }

  def fromSymbol(
    module: GlobalSymbolIndex.Module,
    symbol: String
  )(implicit ctx: SourcePath.Context): JList[l.Location] =
    index
      .definitions(module, Symbol(symbol))
      .filter(_.path.exists())
      .flatMap { defn =>
        val destinationPath =
          if (saveDefFileToDisk) defn.path.toFileOnDisk(workspace)
          else defn.path

        defn.range match {
          // read only source - no need to adjust positions
          case Some(range) if defn.path.filePath.isEmpty =>
            Seq(range.toLocation(destinationPath.uri))
          case _ =>
            val destinationDoc = bestTextDocument(module, defn)
            for {
              location <-
                destinationDoc.toLocation(destinationPath.uri, defn.definitionSymbol.value)
              result <- buffers
                .tokenEditDistance(
                  module,
                  destinationPath,
                  destinationDoc.text,
                  trees
                )
                .toRevised(
                  location.getRange.getStart.getLine,
                  location.getRange.getStart.getCharacter
                )
                .toLocation(location)
            } yield result
        }
      }
      .asJava
}
