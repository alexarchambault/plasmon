// Based on https://github.com/scalameta/metals/blob/0ad0bc184f82dbd178d01f76913ea6bdfa98db14/metals/src/main/scala/scala/meta/internal/metals/DefinitionProvider.scala

package plasmon.ide

import ch.epfl.scala.bsp4j as b
import org.eclipse.lsp4j as l
import plasmon.PlasmonEnrichments.*
import plasmon.ide.*
import plasmon.index.BspData
import plasmon.pc.NopReportContext
import plasmon.semdb.{SemanticdbProcessor, Semanticdbs}

import java.util.List as JList

import scala.jdk.CollectionConverters.*
import scala.meta.internal.mtags.{
  GlobalSymbolIndex,
  Mtags,
  SourcePath,
  Symbol,
  SymbolDefinition
}
import scala.meta.internal.semanticdb.TextDocument

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

    val mappedToOpt = sourceMapper.mappedTo(
      new b.BuildTargetIdentifier(module.targetId),
      symbolDefinition.path
    )

    // Read text file from disk instead of editor buffers because the file
    // on disk is more likely to parse.
    val path = mappedToOpt
      .map(_.compilerPath.toNIO)
      .map(SourcePath.Standard(_))
      .getOrElse(symbolDefinition.path)
    val parsed = {
      val semdb = new Mtags()(using NopReportContext).index(
        path.toLanguage,
        path.toInput,
        symbolDefinition.dialectOpt
      )
      mappedToOpt match {
        case Some(mappedTo) =>
          val path0 = symbolDefinition.path match {
            case s: SourcePath.Standard => os.Path(s.path)
            case _                      => sys.error("Cannot happen")
          }
          SemanticdbProcessor.postProcess(
            originalCode = os.read(path0),
            // FIXME workspace might not be the right source root, does it matter?
            originalPath = path0.relativeTo(workspace),
            adjust = mappedTo.toUserLine,
            doc = semdb
          )
        case None =>
          semdb
      }
    }

    def fromSemanticdbs(p: SourcePath): Option[TextDocument] =
      semanticdbs().textDocument(p, module).toOption.flatMap(_.documentIncludingStale)

    if (parsed.occurrences.isEmpty)
      // Fall back to SemanticDB on disk, if any
      // FIXME Might need post-processing if mappedToOpt is non-empty
      fromSemanticdbs(path)
        .orElse {
          if (path == symbolDefinition.path) None
          else fromSemanticdbs(symbolDefinition.path)
        }
        .getOrElse(parsed)
    else
      parsed
  }

  def fromSymbol(
    module: GlobalSymbolIndex.Module,
    symbol: String
  )(implicit ctx: SourcePath.Context): JList[l.Location] = {

    // Transform things like
    //     build_/package_.thing.
    //     build_/thing/package_.other.
    // to
    //     build_/thing/package_#
    //     build_/thing/other/package_#
    val symbol0 = PatchedSymbolIndex.millBuildPackagePattern.findFirstMatchIn(symbol) match {
      case Some(match0) =>
        "build_/" + match0.group(1) + match0.group(2) + "/package_#"
      case None =>
        symbol
    }

    index
      .definitions(module, Symbol(symbol0))
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
}

object PatchedSymbolIndex {
  // Written with the help of Claude Sonnet 4.6
  val millBuildPackagePattern = {
    val segment    = """[a-z][a-zA-Z0-9_]*"""
    val quoted     = """`[^`]+`"""
    val either     = s"(?:$quoted|$segment)"
    val middlePart = s"((?:(?:$either)/)*?)"
    val lastPart   = s"($either)"

    s"""^build_/${middlePart}package_\\.$lastPart\\.""".r
  }
}
