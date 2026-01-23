// Originally based on https://github.com/scalameta/metals/blob/0ad0bc184f82dbd178d01f76913ea6bdfa98db14/metals/src/main/scala/scala/meta/internal/metals/ReferenceProvider.scala#L53 or earlier versions of that file

package plasmon.index

import scala.meta.Dialect
import scala.meta.inputs.Input
import scala.meta.internal.tokenizers.LegacyScanner
import scala.meta.internal.tokenizers.LegacyToken._

import java.nio.charset.StandardCharsets
import java.util.concurrent.atomic.AtomicReference

import scala.collection.concurrent.TrieMap
import scala.collection.mutable
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.concurrent.Promise
import scala.util.control.NonFatal

import scala.meta.Importee
import scala.meta.internal.mtags.DefinitionAlternatives.GlobalSymbol
import scala.meta.internal.mtags.GlobalSymbolIndex
import scala.meta.internal.mtags.Symbol
import scala.meta.internal.semanticdb.Scala._
import scala.meta.internal.semanticdb.XtensionSemanticdbSymbolInformation
import scala.meta.internal.{semanticdb => s}
import scala.meta.pc.CompletionItemPriority

import ch.epfl.scala.{bsp4j => b}
import com.google.common.hash.BloomFilter
import com.google.common.hash.Funnels
import org.eclipse.{lsp4j => l}
import scala.meta.internal.mtags.SourcePath
import plasmon.ide.ReferencesResult
import scala.meta.internal.metals.EmptyCancelToken
import plasmon.index.BspData
import plasmon.pc.PresentationCompilers
import plasmon.semdb.{SemanticdbIndexer, Semanticdbs}

import plasmon.PlasmonEnrichments._
import scala.jdk.CollectionConverters._
import plasmon.ide.Buffers
import plasmon.ide.PatchedSymbolIndex
import plasmon.ide.Trees
import plasmon.ide.AdjustRange
import plasmon.ide.TokenEditDistance
import scala.meta.internal.mtags.Mtags
import plasmon.semdb.TextDocumentLookup
import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker

final class ReferenceIndex(
  workspace: os.Path,
  semanticdbs: () => Semanticdbs,
  buffers: Buffers,
  symbolIndex: GlobalSymbolIndex,
  trees: Trees,
  bspData: BspData,
  presentationCompilers: PresentationCompilers
)(implicit ec: ExecutionContext) extends SemanticdbIndexer.FeatureProvider {

  import ReferenceIndex._

  private val patchedSymbolIndex = new PatchedSymbolIndex(
    symbolIndex,
    workspace,
    buffers,
    semanticdbs,
    trees,
    saveDefFileToDisk = true,
    bspData
  )

  private val workspaceMemberPriorityMap: TrieMap[String, Int] = TrieMap.empty

  val completionItemPriority: CompletionItemPriority =
    (symbol: String) =>
      -workspaceMemberPriorityMap.getOrElseUpdate(
        symbol, {
          val visited = scala.collection.mutable.Set.empty[os.Path]
          index.iterator.count {
            case (path, entry) =>
              entry.bloom.mightContain(symbol) &&
              os.exists(path) &&
              visited.add(path)
          }
        }
      )

  private val index: TrieMap[os.Path, MaybeStaleIndexEntry] =
    TrieMap.empty

  override def reset(): Unit = {
    index.clear()
    workspaceMemberPriorityMap.clear()
  }

  private def updateWorkspaceMemberPriority(symbol: String, change: Int): Unit =
    if (change != 0)
      workspaceMemberPriorityMap.updateWith(symbol) { priority =>
        Some(priority.getOrElse(0) + change)
      }

  override def onDelete(file: os.Path): Unit =
    for {
      ent         <- index.remove(file)
      (symbol, _) <- workspaceMemberPriorityMap
      if ent.bloom.mightContain(symbol)
    } updateWorkspaceMemberPriority(symbol, -1)

  override def onChange(
    module: GlobalSymbolIndex.Module,
    file: os.Path,
    docs: s.TextDocuments
  ): Unit = {
    scribe.info(s"ReferenceIndex.onChange($file)")
    val count           = docs.documents.iterator.map(_.occurrences.length).sum
    val syntheticsCount = docs.documents.iterator.map(_.synthetics.length).sum
    val bloom = BloomFilter.create(
      Funnels.stringFunnel(StandardCharsets.UTF_8),
      Integer.valueOf((count + syntheticsCount) * 2),
      0.01
    )

    val oldEntry = index.get(file)

    index(file) = MaybeStaleIndexEntry(new b.BuildTargetIdentifier(module.targetId), bloom)
    for (d <- docs.documents) {
      for (o <- d.occurrences)
        bloom.put(o.symbol)
      for (synthetic <- d.synthetics)
        Synthetics.foreachSymbol(synthetic) { sym =>
          bloom.put(sym)
          Synthetics.Continue
        }
    }
    for ((symbol, _) <- workspaceMemberPriorityMap) {
      val shift = oldEntry
        .exists(_.bloom.mightContain(symbol))
        .compare(bloom.mightContain(symbol))
      if (shift != 0)
        updateWorkspaceMemberPriority(symbol, -shift)
    }
  }

  /** Find all SymbolOccurrences for the given position. Multiple symbols might be attached, for
    * example extension parameter. see: https://github.com/scalameta/metals/issues/3133
    */
  private def positionOccurrences(
    module: GlobalSymbolIndex.Module,
    source: os.Path,
    dirtyPosition: l.Position,
    snapshot: s.TextDocument
  ): List[ReferenceIndex.ResolvedSymbolOccurrence] = {
    val sourceDistance = buffers.tokenEditDistance(module, source, snapshot.text, trees)
    sourceDistance
      .toOriginal(
        dirtyPosition.getLine,
        dirtyPosition.getCharacter
      )
      .toPosition(dirtyPosition)
      .map { queryPosition =>
        val occs = snapshot.occurrences.filter(_.encloses(queryPosition, true))
        // In case of macros we might need to get the postion from the presentation compiler
        if (occs.isEmpty)
          bspData.getDialect(module, source)
            .map { dialect =>
              Mtags
                .allToplevels(source.toInput, dialect)
                .occurrences
                .find(_.encloses(queryPosition))
                .toList
            }
            .getOrElse(occs)
        else
          occs
      }
      .getOrElse(Nil)
      .map { occ =>
        ReferenceIndex.ResolvedSymbolOccurrence(sourceDistance, Some(occ))
      }
      .toList
  }

  /** Find references for the given params.
    *
    * @return - All found list of references, it is a list of result because
    *           in some cases, multiple symbols are attached to the given position.
    *           (e.g. extension parameter). See: https://github.com/scalameta/scalameta/issues/2443
    */
  def references(
    module: GlobalSymbolIndex.Module,
    params: l.ReferenceParams,
    findRealRange: AdjustRange = AdjustRange.none,
    includeSynthetics: s.Synthetic => Boolean = _ => true
  )(implicit ctx: SourcePath.Context): Future[List[ReferencesResult]] = {
    val source         = params.getTextDocument.getUri.osPathFromUri
    val textDoc        = semanticdbs().textDocument(source, module).toOption
    val supportsPcRefs = bspData.inverseSources(source).nonEmpty
    val textDocOpt: Option[s.TextDocument] =
      if (supportsPcRefs) textDoc.flatMap(_.toOption) else textDoc.flatMap(_.documentIncludingStale)
    textDocOpt match {
      case Some(doc) =>
        val results: List[ReferenceIndex.ResolvedSymbolOccurrence] = {
          val posOccurrences = positionOccurrences(module, source, params.getPosition, doc)
          if (posOccurrences.isEmpty)
            // handling case `import a.{A as @@B}`
            trees
              .findLastEnclosingAt[Importee.Rename](
                module,
                source,
                params.getPosition
              )
              .map { rename =>
                positionOccurrences(
                  module,
                  source,
                  rename.name.pos.toLsp.getStart,
                  doc
                )
              }
              .getOrElse(Nil)
          else
            posOccurrences
        }
        scribe.info(s"Found ${results.length} posOccurrences")
        if (results.isEmpty)
          scribe.debug(
            s"No symbol found at ${params.getPosition} for $source"
          )
        val semanticdbResult = Future.sequence {
          results.map { result =>
            val occurrence = result.occurrence.get
            val distance   = result.distance
            val alternatives =
              referenceAlternatives(module, occurrence.symbol, source, doc)
            references(
              module,
              source,
              params,
              doc,
              distance,
              occurrence,
              alternatives,
              params.getContext.isIncludeDeclaration,
              findRealRange,
              includeSynthetics,
              supportsPcRefs
            ).map { locations =>
              if (locations.isEmpty && params.getContext.isIncludeDeclaration)
                scribe.debug {
                  val fileInIndex =
                    if (index.contains(source)) s"Current file $source is present"
                    else s"Missing current file $source"
                  s"No references found, index size ${index.size}\n" + fileInIndex
                }
              ReferencesResult(occurrence.symbol, locations)
            }

          }
        }

        semanticdbResult.map(
          _.groupBy(_.symbol)
            .collect { case (symbol, refs) =>
              ReferencesResult(symbol, refs.flatMap(_.locations))
            }
            .toList
        )
      case None =>
        if (textDoc.flatMap(_.documentIncludingStale).isEmpty)
          scribe.debug(s"No semanticDB for $source")
        else scribe.debug(s"Stale semanticDB for $source")
        val includeDeclaration = params.getContext.isIncludeDeclaration()
        for {
          foundRefs <- presentationCompilers.references(
            params,
            EmptyCancelToken,
            findRealRange
          )
          symbols = foundRefs.map(_.symbol).filterNot(_.isLocal)
        } yield
          if (symbols.isEmpty) foundRefs
          else {
            val fromWorkspace = workspaceReferences(
              module,
              source,
              symbols.toSet,
              includeDeclaration,
              findRealRange,
              includeSynthetics
            )
            val results =
              ReferencesResult(symbols.head, fromWorkspace) :: foundRefs

            results
              .groupBy(_.symbol)
              .collect {
                case (symbol, refs) =>
                  ReferencesResult(symbol, refs.flatMap(_.locations))
              }
              .toList
          }
    }
  }

  // Returns alternatives symbols for which "goto definition" resolves to the occurrence symbol.
  private def referenceAlternatives(
    module: GlobalSymbolIndex.Module,
    symbol: String,
    fromSource: os.Path,
    referenceDoc: s.TextDocument
  )(implicit ctx: SourcePath.Context): Set[String] = {
    val definitionDoc =
      if (referenceDoc.symbols.exists(_.symbol == symbol))
        Some((fromSource, referenceDoc))
      else
        for {
          location <- patchedSymbolIndex
            .fromSymbol(module, symbol)
            .asScala
            .headOption
          source = location.getUri.osPathFromUri
          definitionDoc <- semanticdbs()
            .textDocument(source, module) // is that the right module?
            .toOption
            .flatMap(_.documentIncludingStale)
        } yield (source, definitionDoc)

    definitionDoc match {
      case Some((defPath, definitionDoc)) =>
        val name         = symbol.desc.name.value
        val alternatives = new SymbolAlternatives(symbol, name)

        def candidates(check: s.SymbolInformation => Boolean) = for {
          info <- definitionDoc.symbols
          if check(info)
        } yield info.symbol

        val isCandidate =
          if (defPath.isJava)
            candidates { info =>
              alternatives.isJavaConstructor(info)
            }.toSet
          else
            candidates { info =>
              alternatives.isVarSetter(info) ||
              alternatives.isCompanionObject(info) ||
              alternatives.isCompanionClass(info) ||
              alternatives.isCopyOrApplyParam(info) ||
              alternatives.isContructorParam(info)
            }.toSet

        val nonSyntheticSymbols = for {
          occ <- definitionDoc.occurrences
          if isCandidate(occ.symbol) || occ.symbol == symbol
          if occ.role.isDefinition
        } yield occ.symbol

        def isSyntheticSymbol = !nonSyntheticSymbols.contains(symbol)

        def additionalAlternativesForSynthetic = for {
          info <- definitionDoc.symbols
          if info.symbol != name
          if {
            alternatives.isCompanionClass(info) ||
            alternatives.isFieldParam(info)
          }
        } yield info.symbol

        if (defPath.isJava)
          isCandidate
        else if (isSyntheticSymbol)
          isCandidate ++ additionalAlternativesForSynthetic
        else
          isCandidate
      case None => Set.empty
    }
  }

  /** Return all paths to files which contain at least one symbol from isSymbol set.
    */
  private def pathsFor(
    buildTargetSet: Set[b.BuildTargetIdentifier],
    isSymbol: Set[String]
  ): Iterator[(os.Path, b.BuildTargetIdentifier)] =
    if (buildTargetSet.isEmpty) Iterator.empty
    else {
      val allowedBuildTargets =
        buildTargetSet.flatMap(bspData.allInverseDependencies)
      val visited = scala.collection.mutable.Set.empty[os.Path]
      index
        .iterator
        .filter {
          case (path, entry) =>
            // if includeStale || !entry.isStale || path.last.isJavaFilename
            allowedBuildTargets(entry.id) &&
            isSymbol.exists(entry.bloom.mightContain) &&
            visited.add(path) &&
            os.exists(path)
        }
        .map {
          case (path, entry) =>
            (path, entry.id)
        }
    }

  private def workspaceReferences(
    module: GlobalSymbolIndex.Module,
    source: os.Path,
    isSymbol: Set[String],
    isIncludeDeclaration: Boolean,
    findRealRange: AdjustRange,
    includeSynthetics: s.Synthetic => Boolean,
    sourceContainsDefinition: Boolean = false
  ): Seq[l.Location] = {
    val definitionPaths: Set[SourcePath] =
      if (sourceContainsDefinition) Set(SourcePath.Standard(source.toNIO))
      else {
        val foundDefinitionLocations = SourcePath.withContext { implicit ctx =>
          // FIXME Context could be broader? (passed via an implicit to workspaceReferences)
          isSymbol
            .map(Symbol(_).toplevel)
            .map(symbolIndex.findFileForToplevel(_))
            .flatMap(_.map(_._1))
        }

        if (foundDefinitionLocations.isEmpty)
          Set(SourcePath.Standard(source.toNIO))
        else
          foundDefinitionLocations
      }

    val definitionBuildTargets = definitionPaths.flatMap(bspData.inverseSourcesAll)

    val result = for {
      (sourcePath, sourcePathTargetId) <- pathsFor(definitionBuildTargets, isSymbol)
      semanticdb <- semanticdbs()
        .textDocument(sourcePath, sourcePathTargetId.module)
        .toOption
        .flatMap(_.documentIncludingStale)
        .iterator
      semanticdbDistance = buffers.tokenEditDistance(module, sourcePath, semanticdb.text, trees)
      uri                = sourcePath.toNIO.toUri.toASCIIString
      reference <-
        try
          referenceLocations(
            semanticdb,
            isSymbol,
            semanticdbDistance,
            uri,
            isIncludeDeclaration,
            findRealRange,
            includeSynthetics,
            sourcePath.isJava
          )
        catch {
          case NonFatal(e) =>
            // Can happen for example if the SemanticDB text is empty for some reason.
            scribe.error(s"reference: $sourcePath", e)
            Nil
        }
    } yield reference
    result.toSeq
  }

  private def references(
    module: GlobalSymbolIndex.Module,
    source: os.Path,
    params: l.ReferenceParams,
    snapshot: s.TextDocument,
    distance: TokenEditDistance,
    occ: s.SymbolOccurrence,
    alternatives: Set[String],
    isIncludeDeclaration: Boolean,
    findRealRange: AdjustRange,
    includeSynthetics: s.Synthetic => Boolean,
    supportsPcRefs: Boolean
  ): Future[Seq[l.Location]] = {
    val isSymbol = alternatives + occ.symbol
    val isLocal  = occ.symbol.isLocal
    if (isLocal && supportsPcRefs)
      presentationCompilers
        .references(params, EmptyCancelToken, findRealRange)
        .map(_.flatMap(_.locations))
    else {
      /* search local in the following cases:
       * - it's a dependency source.
       *   We can't search references inside dependencies so at least show them in a source file.
       * - it's a standalone file that doesn't belong to any build target
       */
      val searchLocal =
        source.isDependencySource(workspace) ||
        bspData.inverseSources(source).isEmpty ||
        isLocal

      val local =
        if (searchLocal)
          referenceLocations(
            snapshot,
            isSymbol,
            distance,
            params.getTextDocument.getUri,
            isIncludeDeclaration,
            findRealRange,
            includeSynthetics,
            source.isJava
          )
        else Seq.empty

      def sourceContainsDefinition =
        occ.role.isDefinition || snapshot.symbols.exists(
          _.symbol == occ.symbol
        )
      val workspaceRefs =
        if (isLocal) Seq.empty
        else
          workspaceReferences(
            module,
            source,
            isSymbol,
            isIncludeDeclaration,
            findRealRange,
            includeSynthetics,
            sourceContainsDefinition
          )
      Future.successful(local ++ workspaceRefs)
    }
  }

  private def referenceLocations(
    snapshot: s.TextDocument,
    isSymbol: Set[String],
    distance: TokenEditDistance,
    uri: String,
    isIncludeDeclaration: Boolean,
    findRealRange: AdjustRange,
    includeSynthetics: s.Synthetic => Boolean,
    isJava: Boolean
  ): Seq[l.Location] = {
    val buf = Set.newBuilder[l.Location]
    def add(range: s.Range): Unit = {
      val revised       = distance.toRevised(range.startLine, range.startCharacter)
      val dirtyLocation = range.toLocation(uri)
      for {
        location <- revised.toLocation(dirtyLocation)
      }
        buf += location
    }

    for {
      reference <- snapshot.occurrences
      if isSymbol(reference.symbol)
      if !reference.role.isDefinition || isIncludeDeclaration
      range <- reference.range.toList
    }

      if (isJava)
        add(range)
      /* Find real range is used when renaming occurrences,
       * where we need to check if the symbol name matches exactly.
       * This was needed for some issues with macro annotations
       * and with renames we must be sure that a proper name is replaced.
       * In case of finding references, where false positives
       * are ok and speed is more important, we just use the default AdjustRange.none.
       */
      else
        findRealRange(range, snapshot.text, reference.symbol).foreach(add)

    for {
      synthetic <- snapshot.synthetics
      if Synthetics.existsSymbol(synthetic)(isSymbol) && includeSynthetics(
        synthetic
      )
      range <- synthetic.range.toList
    } add(range)

    buf.result().toSeq
  }

  def asJson: ReferenceIndex.AsJson =
    ReferenceIndex.AsJson()
}

object ReferenceIndex {

  private class SymbolAlternatives(symbol: String, name: String) {

    // Returns true if `info` is the companion object matching the occurrence class symbol.
    def isCompanionObject(info: s.SymbolInformation): Boolean =
      info.isObject &&
      info.displayName == name &&
      symbol == Symbols.Global(
        info.symbol.owner,
        Descriptor.Type(info.displayName)
      )

    // Returns true if `info` is the java constructor matching the occurrence class symbol.
    def isJavaConstructor(info: s.SymbolInformation): Boolean =
      info.isConstructor &&
      (Symbol(info.symbol) match {
        case GlobalSymbol(clsSymbol, Descriptor.Method("<init>", _)) =>
          symbol == clsSymbol.value
        case _ =>
          false
      })

    // Returns true if `info` is the companion class matching the occurrence object symbol.
    def isCompanionClass(info: s.SymbolInformation): Boolean =
      info.isClass &&
      info.displayName == name &&
      symbol == Symbols.Global(
        info.symbol.owner,
        Descriptor.Term(info.displayName)
      )

    // Returns true if `info` is a named parameter of the primary constructor
    def isContructorParam(info: s.SymbolInformation): Boolean =
      info.isParameter &&
      info.displayName == name &&
      symbol == (Symbol(info.symbol) match {
        case GlobalSymbol(
              // This means it's the primary constructor
              GlobalSymbol(owner, Descriptor.Method("<init>", "()")),
              Descriptor.Parameter(_)
            ) =>
          Symbols.Global(owner.value, Descriptor.Term(name))
        case _ =>
          ""
      })

    // Returns true if `info` is a field that corresponds to named parameter of the primary constructor
    def isFieldParam(info: s.SymbolInformation): Boolean =
      (info.isVal || info.isVar) &&
      info.displayName == name &&
      symbol == (Symbol(info.symbol) match {
        case GlobalSymbol(owner, Descriptor.Term(name)) =>
          Symbols.Global(
            // This means it's the primary constructor
            Symbols.Global(owner.value, Descriptor.Method("<init>", "()")),
            Descriptor.Parameter(name)
          )
        case _ =>
          ""
      })

    // Returns true if `info` is a parameter of a synthetic `copy` or `apply` matching the occurrence field symbol.
    def isCopyOrApplyParam(info: s.SymbolInformation): Boolean =
      info.isParameter &&
      info.displayName == name &&
      symbol == (Symbol(info.symbol) match {
        case GlobalSymbol(
              GlobalSymbol(
                GlobalSymbol(owner, Descriptor.Term(obj)),
                Descriptor.Method("apply", _)
              ),
              _
            ) =>
          Symbols.Global(
            Symbols.Global(owner.value, Descriptor.Type(obj)),
            Descriptor.Term(name)
          )
        case GlobalSymbol(
              GlobalSymbol(
                GlobalSymbol(owner, Descriptor.Type(obj)),
                Descriptor.Method("copy", _)
              ),
              _
            ) =>
          Symbols.Global(
            Symbols.Global(owner.value, Descriptor.Type(obj)),
            Descriptor.Term(name)
          )
        case _ =>
          ""
      })

    // Returns true if `info` is companion var setter method for occ.symbol var getter.
    def isVarSetter(info: s.SymbolInformation): Boolean =
      info.displayName.endsWith("_=") &&
      info.displayName.startsWith(name) &&
      symbol == (Symbol(info.symbol) match {
        case GlobalSymbol(owner, Descriptor.Method(setter, disambiguator)) =>
          Symbols.Global(
            owner.value,
            Descriptor.Method(setter.stripSuffix("_="), disambiguator)
          )
        case _ =>
          ""
      })
  }

  private case class ResolvedSymbolOccurrence(
    distance: TokenEditDistance,
    occurrence: Option[s.SymbolOccurrence]
  )

  private case class MaybeStaleIndexEntry(
    id: b.BuildTargetIdentifier,
    bloom: BloomFilter[CharSequence]
  )

  final case class AsJson()

  given JsonValueCodec[AsJson] =
    JsonCodecMaker.make
}
