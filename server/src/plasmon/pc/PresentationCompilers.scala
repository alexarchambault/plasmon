// Originally based on https://github.com/scalameta/metals/blob/0ad0bc184f82dbd178d01f76913ea6bdfa98db14/metals/src/main/scala/scala/meta/internal/metals/Compilers.scala or earlier versions of that file

package plasmon.pc

import java.nio.file.Paths
import java.util.Collections
import java.util.concurrent.CompletableFuture
import java.util.concurrent.ScheduledExecutorService
import java.util.{List => JList, Map => JMap, Optional}

import scala.annotation.tailrec
import scala.collection.concurrent.TrieMap
import scala.concurrent.ExecutionContextExecutorService
import scala.concurrent.Future
import scala.util.control.NonFatal

import scala.meta.inputs.Input
import scala.meta.inputs.Position
import scala.meta.internal.pc
import plasmon.ide.SbtBuildTool
import scala.meta.internal.metals.CompilerOffsetParamsUtils
import scala.meta.internal.metals.CompilerRangeParamsUtils
import scala.meta.internal.mtags.GlobalSymbolIndex
import scala.meta.internal.mtags.MD5
import scala.meta.internal.pc.JavaPresentationCompiler
import scala.meta.internal.pc.LogMessages
import scala.meta.internal.pc.PcSymbolInformation
import scala.meta.internal.{semanticdb => s}
import scala.meta.pc.AutoImportsResult
import scala.meta.pc.CancelToken
import scala.meta.pc.CodeActionId
import scala.meta.pc.CompletionItemPriority
import scala.meta.pc.HoverSignature
import scala.meta.pc.OffsetParams
import scala.meta.pc.PresentationCompiler
import scala.meta.pc.SyntheticDecorationsParams

import ch.epfl.scala.{bsp4j => b}
import org.eclipse.lsp4j.jsonrpc.messages.{Either => JEither}
import org.eclipse.lsp4j.{debug => d}
import org.eclipse.{lsp4j => l}
import scala.meta.pc.CompileResult
import javax.tools.JavaFileManager
import scala.meta.internal.metals.Testing
import scala.meta.internal.metals.CompilerOffsetParams
import scala.meta.internal.metals.ScalaVersions
import plasmon.ide.HoverExtParams
import plasmon.ide.AdjustedLspData
import scala.meta.internal.metals.CompilerVirtualFileParams
import scala.meta.internal.metals.CompilerInlayHintsParams
import plasmon.ide.ReferencesResult
import scala.meta.internal.metals.EmptyCancelToken
import plasmon.index.{BspData, SymbolSearchImpl, SymbolSearchIndex}

import plasmon.PlasmonEnrichments._
import scala.jdk.CollectionConverters._
import scala.meta.internal.metals.Docstrings

import plasmon.ide.{AdjustLspData, AdjustRange, Buffers, PatchedSymbolIndex, SourceMapper}
import plasmon.servercommand.BspUtil
import scala.meta.internal.pc.ScalaPresentationCompiler as Scala2PresentationCompiler
import dotty.tools.pc.ScalaPresentationCompiler as Scala3PresentationCompiler
import java.util.concurrent.ConcurrentHashMap
import scala.meta.internal.pc.InlayHints
import scala.concurrent.Await
import scala.concurrent.duration.Duration
import java.util.concurrent.TimeoutException
import scala.util.Success
import plasmon.ide.ScalaTarget
import scala.meta.pc.SymbolSearch
import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent.ExecutorService
import plasmon.ide.Directories
import scala.meta.pc.PresentationCompilerConfig
import plasmon.languageclient.PlasmonLanguageClient
import java.util.UUID
import com.github.plokhotnyuk.jsoniter_scala.core.{JsonValueCodec, writeToArrayReentrant}
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import coursier.parse.RawJson
import plasmon.render.JsonCodecs.given
import scala.util.Failure
import scala.util.Properties
import java.net.URI
import dotty.tools.dotc.reporting.Diagnostic
import plasmon.Logger

/** Manages lifecycle for presentation compilers in all build targets.
  *
  * We need a custom presentation compiler for each build target since build targets can have
  * different classpaths and compiler settings.
  */
class PresentationCompilers(
  workspace: os.Path,
  javaHome: os.Path,
  javaFileManager: () => JavaFileManager,
  config0: PresentationCompilerConfig,
  bspData: BspData,
  buffers: Buffers,
  symbolDocs: Docstrings,
  symbolSearchIndex: SymbolSearchIndex,
  patchedSymbolIndex: PatchedSymbolIndex,
  index: GlobalSymbolIndex,
  sh: ScheduledExecutorService,
  completionItemPriority: () => CompletionItemPriority,
  javaUserLoggerMaker: java.util.function.Function[
    String,
    java.util.function.Consumer[String]
  ],
  loggerManager: plasmon.Logger.Manager,
  refreshStatus: () => Unit,
  languageClient: PlasmonLanguageClient,
  scala2Compat: Boolean,
  emitDiagnostics: (
    Scala3PresentationCompiler,
    GlobalSymbolIndex.Module,
    URI,
    Seq[l.Diagnostic],
    Logger
  ) => Unit
)(implicit ec: ExecutionContextExecutorService)
    extends AutoCloseable {

  import PresentationCompilers.*

  val interactiveCompilersStatuses = new ConcurrentHashMap[
    scala.meta.pc.PresentationCompiler & scala.meta.internal.pc.HasCompilerAccess,
    ::[(String, String)]
  ]

  private var debug: Boolean = Option(System.getenv("PLASMON_DEBUG")).contains("true")

  /** Enable or disable debugging
    *
    * @param debug
    * @return
    *   Whether the debugging value was changed
    */
  def setDebug(debug: Boolean): Boolean =
    (debug != this.debug) && {
      restartAll()
      this.debug = debug
      true
    }

  // stateless
  private val sourceMapper = SourceMapper(
    bspData,
    buffers
  )

  // stateful
  private val symbolSearch = new SymbolSearchImpl(
    symbolDocs,
    symbolSearchIndex,
    patchedSymbolIndex,
    index
  )

  def reset(): Unit =
    symbolSearch.reset()

  private val plugins = new CompilerPlugins

  private val outlineFilesProvider =
    new OutlineFilesProvider(bspData, buffers)

  // Not a TrieMap because we want to avoid loading duplicate compilers for the same build target.
  // Not a `j.u.c.ConcurrentHashMap` because it can deadlock in `computeIfAbsent` when the absent
  // function is expensive, which is the case here.
  val jcache: JMap[PresentationCompilerKey, MtagsPresentationCompiler] =
    Collections.synchronizedMap(
      new java.util.HashMap[PresentationCompilerKey, MtagsPresentationCompiler]
    )
  val jCompletionCache
    : JMap[PresentationCompilerKey, MtagsPresentationCompiler] =
    Collections.synchronizedMap(
      new java.util.HashMap[PresentationCompilerKey, MtagsPresentationCompiler]
    )

  def reset(targetId: b.BuildTargetIdentifier): Unit =
    for {
      key <- Seq(
        PresentationCompilerKey.ScalaBuildTarget(targetId),
        PresentationCompilerKey.JavaBuildTarget(targetId)
      )
      cache <- Seq(jcache, jCompletionCache)
      elem  <- Option(cache.remove(key))
    } elem.shutdown()

  private val cache           = jcache.asScala
  private val completionCache = jCompletionCache.asScala
  private def buildTargetPCFromCache(
    id: b.BuildTargetIdentifier
  ): Seq[PresentationCompiler] = {
    val seq = cache.get(PresentationCompilerKey.ScalaBuildTarget(id)).toSeq ++
      completionCache.get(PresentationCompilerKey.ScalaBuildTarget(id)).toSeq
    seq
      .collect {
        case lazyPc if lazyPc != null => lazyPc.await.toSeq
      }
      .flatten
  }
  private def buildTargetCompletionPCFromCache(id: b.BuildTargetIdentifier)
    : Option[PresentationCompiler] =
    completionCache
      .get(PresentationCompilerKey.ScalaBuildTarget(id))
      .collect {
        case lazyPc if lazyPc != null => lazyPc.await
      }
      .flatten

  def loadedPresentationCompilerCount(): Int =
    cache.values.count(_.await.exists(_.isLoaded())) +
      completionCache.values.count(_.await.exists(_.isLoaded()))

  override def close(): Unit = {
    for ((_, v) <- cache)
      v.shutdown()
    for ((_, v) <- completionCache)
      v.shutdown()
    cache.clear()
    completionCache.clear()
    outlineFilesProvider.clear()
  }

  def restartAll(): Unit = {
    val count = cache.size + completionCache.size
    close()
    scribe.info(
      s"restarted $count presentation compiler${LogMessages.plural(count)}"
    )
  }

  def load(paths: Seq[os.Path]): Future[Unit] =
    if (Testing.isEnabled) Future.successful(())
    else
      Future {
        val targets = paths
          .filter(_.isScalaFilename)
          .flatMap(path => bspData.inverseSources(path).toList)
          .distinct
        targets.foreach { target =>
          loadCompiler(target).foreach { pc =>
            pc
              .hover(
                CompilerOffsetParams(
                  Paths.get("Main.scala").toUri(),
                  "object Ma\n",
                  "object Ma".length()
                )
              )
              .thenApply(_.map(_.toLsp()))
          }
        }
      }

  def didClose(path: os.Path): Unit = {
    loadCompiler(path).foreach(_.didClose(path.toNIO.toUri()))
  }

  def didChange(path: os.Path): Future[List[l.Diagnostic]] =
    loadCompiler(path)
      .map { pc =>
        val (input, adjust) = (path.toInputFromBuffers(buffers), AdjustedLspData.default)
        outlineFilesProvider.didChange(pc.buildTargetId(), path)
        pc
          .didChange(CompilerVirtualFileParams(path.toNIO.toUri(), input.value))
          .asScala
          .map(_.asScala.map(adjust.adjustDiagnostic).toList)
      }
      .getOrElse(Future.successful(Nil))

  def didCompile(report: b.CompileReport): Unit = {
    val isSuccessful = report.getErrors == 0
    val isBestEffortCompilation =
      bspData
        .scalaTarget(report.getTarget)
        .map(_.isBestEffort)
        .getOrElse(false)
    for (pc <- buildTargetPCFromCache(report.getTarget))
      if (
        outlineFilesProvider.shouldRestartPc(
          report.getTarget,
          OutlineFilesProvider.DidCompile(isSuccessful)
        )
      )
        pc.restart()

    outlineFilesProvider.onDidCompile(report.getTarget, isSuccessful)

    if (isSuccessful || isBestEffortCompilation)
      // Restart PC for all build targets that depend on this target since the classfiles
      // may have changed.
      for {
        target <- bspData.allInverseDependencies(report.getTarget)
        if target != report.getTarget
        if outlineFilesProvider.shouldRestartPc(target, OutlineFilesProvider.InverseDependency)
        compiler <- buildTargetPCFromCache(target)
      }
        compiler.restart()
  }

  def completionItemResolve(item: l.CompletionItem): Future[l.CompletionItem] = {
    val maybeItemFuture = for {
      data     <- item.data
      compiler <- buildTargetCompletionPCFromCache(new b.BuildTargetIdentifier(data.target))
    } yield compiler.completionItemResolve(item, data.symbol).asScala
    maybeItemFuture.getOrElse(Future.successful(item))
  }

  /** Calculates completions for a expression evaluator at breakpointPosition
    *
    * @param path
    *   path to file containing ht ebreakpoint
    * @param breakpointPosition
    *   actual breakpoint position
    * @param token
    *   cancel token for the compiler
    * @param expression
    *   expression that is currently being types
    * @param isZeroBased
    *   whether the client supports starting at 0 or 1 index
    * @return
    */
  def debugCompletions(
    path: os.Path,
    breakpointPosition: l.Position,
    token: CancelToken,
    expression: d.CompletionsArguments,
    isZeroBased: Boolean
  ): Future[Seq[d.CompletionItem]] = {

    /** Find the offset of the cursor inside the modified expression. We need it to give the
      * compiler the right offset.
      *
      * @param modified
      *   modified expression with indentation inserted
      * @param indentation
      *   indentation of the current breakpoint
      * @return
      *   offset where the compiler should insert completions
      */
    def expressionOffset(modified: String, indentation: String) = {
      val line   = expression.getLine
      val column = expression.getColumn
      modified.split("\n").zipWithIndex.take(line).foldLeft(0) {
        case (offset, (lineText, index)) =>
          if (index + 1 == line)
            if (line > 1)
              offset + column + indentation.size - 1
            else
              offset + column - 1
          else offset + lineText.size + 1
      }
    }

    loadCompiler(path)
      .map { compiler =>
        val input = path.toInputFromBuffers(buffers)
        breakpointPosition.toMeta(input) match {
          case Some(metaPos) =>
            val oldText = metaPos.input.text
            val lineStart = oldText.indexWhere(
              c => c != ' ' && c != '\t',
              metaPos.start + 1
            )

            val indentationSize = lineStart - metaPos.start
            val indentationChar =
              if (oldText.lift(lineStart - 1).exists(_ == '\t')) '\t' else ' '
            val indentation = indentationChar.toString * indentationSize

            val expressionText =
              expression.getText.replace("\n", s"\n$indentation")

            val prev = oldText.substring(0, lineStart)
            val succ = oldText.substring(lineStart)
            // insert expression at the start of breakpoint's line and move the lines one down
            val modified = s"$prev;$expressionText\n$indentation$succ"

            val rangeEnd =
              lineStart + expressionOffset(expressionText, indentation) + 1

            /** Calculate the start if insertText is used for item, which does not declare an exact
              * start.
              */
            def insertStart = {
              var i = rangeEnd - 1
              while (modified.charAt(i).isLetterOrDigit) i -= 1
              if (isZeroBased) i else i + 1
            }

            val offsetParams =
              CompilerOffsetParams(
                path.toNIO.toUri,
                modified,
                rangeEnd,
                token,
                outlineFilesProvider.getOutlineFiles(compilerTargetId(compiler))
              )

            val previousLines = expression
              .getText
              .split("\n")

            // we need to adjust start to point at the start of the replacement in the expression
            val adjustStart =
              /* For multiple line we need to insert at the correct offset and
               * then adjust column by indentation that was added to the expression
               */
              if (previousLines.size > 1)
                previousLines
                  .take(expression.getLine - 1)
                  .map(_.size + 1)
                  .sum - indentationSize
              // for one line we only need to adjust column with indentation + ; that was added to the expression
              else -(1 + indentationSize)

            compiler
              .complete(offsetParams)
              .asScala
              .map(list =>
                list.getItems.asScala.toSeq
                  .map(
                    toDebugCompletionItem(
                      _,
                      adjustStart,
                      Position.Range(
                        input.copy(value = modified),
                        insertStart,
                        rangeEnd
                      )
                    )
                  )
              )
          case None =>
            scribe.debug(s"$breakpointPosition was not found in $path ")
            Future.successful(Nil)
        }
      }
      .getOrElse(Future.successful(Nil))
  }

  def semanticTokens(
    params: l.SemanticTokensParams,
    token: CancelToken
  ): Future[l.SemanticTokens] = {
    val emptyTokens = Collections.emptyList[Integer]()

    val path = params.getTextDocument.getUri.osPathFromUri
    loadCompiler(path)
      .map { compiler =>
        val (input, _, adjust) = sourceAdjustments(
          new b.BuildTargetIdentifier(compilerTargetId(compiler)),
          params.getTextDocument.getUri
        )

        /** Find the start that is actually contained in the file and not in the added parts such as
          * imports in sbt.
          *
          * @param line
          *   line within the adjusted source
          * @param character
          *   line within the adjusted source
          * @param remaining
          *   the rest of the tokens to analyze
          * @return
          *   the first found that should be contained with the rest
          */
        @tailrec
        def findCorrectStart(
          line: Integer,
          character: Integer,
          remaining: List[Integer]
        ): List[Integer] =
          remaining match {
            case lineDelta :: charDelta :: next =>
              val newCharacter: Integer =
                // only increase character delta if the same line
                if (lineDelta == 0) character + charDelta
                else charDelta

              val adjustedTokenPos = adjust.adjustPos(
                new l.Position(line + lineDelta, newCharacter),
                adjustToZero = false
              )
              if (
                adjustedTokenPos.getLine >= 0 &&
                adjustedTokenPos.getCharacter >= 0
              )
                (adjustedTokenPos.getLine: Integer) ::
                  (adjustedTokenPos.getCharacter: Integer) :: next
              else
                findCorrectStart(
                  line + lineDelta,
                  newCharacter,
                  next.drop(3)
                )
            case _ => Nil
          }
        val vFile =
          CompilerVirtualFileParams(
            path.toNIO.toUri(),
            input.text,
            token,
            outlineFilesProvider.getOutlineFiles(compilerTargetId(compiler))
          )
        val isScala3 = ScalaVersions.isScala3Version(compiler.scalaVersion())

        compiler
          .semanticTokens(vFile)
          .asScala
          .map { nodes =>
            val plist =
              try
                SemanticTokensProvider.provide(
                  nodes.asScala.toList,
                  vFile,
                  isScala3
                )
              catch {
                case NonFatal(e) =>
                  scribe.error(
                    s"Failed to tokenize input for semantic tokens for $path",
                    e
                  )
                  Nil
              }

            val tokens =
              findCorrectStart(0, 0, plist.toList)
            new l.SemanticTokens(tokens.asJava)
          }
      }
      .getOrElse(Future.successful(new l.SemanticTokens(emptyTokens)))
  }

  def inlayHints(
    params: l.InlayHintParams,
    token: CancelToken
  ): Future[JList[l.InlayHint]] =
    withPCAndAdjustLsp(params) { (pc0, pos, adjust) =>
      def inlayHintsFallback(
        params: SyntheticDecorationsParams
      ): Future[JList[l.InlayHint]] =
        pc0.syntheticDecorations(params)
          .asScala
          .map { list =>
            list
              .asScala
              .map { d =>
                val hint = new l.InlayHint
                hint.setPosition(d.range().getStart)
                hint.setLabel(d.label())
                val kind =
                  if (d.kind() <= 2) l.InlayHintKind.Type
                  else l.InlayHintKind.Parameter
                hint.setKind(kind)
                hint.setData(
                  pc.InlayHints
                    .toData(params.uri().toString(), List(Left("")))
                )
                hint
              }
              .asJava
          }

      def adjustInlayHints(
        inlayHints: JList[l.InlayHint]
      ): JList[l.InlayHint] =
        inlayHints.asScala
          .dropWhile { hint =>
            val adjusted =
              adjust.adjustPos(hint.getPosition, adjustToZero = false)
            adjusted.getLine < 0 || adjusted.getCharacter < 0
          }
          .map { hint =>
            hint.setPosition(adjust.adjustPos(hint.getPosition))
            InlayHintCompat.maybeFixInlayHintData(
              hint,
              params.getTextDocument.getUri
            )
            hint
          }
          .asJava

      val rangeParams =
        CompilerRangeParamsUtils.fromPos(
          pos,
          token,
          outlineFilesProvider.getOutlineFiles(pc0.buildTargetId())
        )
      val options = InlayHintsOptions(Map.empty)
      val pcParams = CompilerInlayHintsParams(
        rangeParams,
        inferredTypes = options.inferredType,
        typeParameters = options.typeParameters,
        implicitParameters = options.implicitArguments,
        hintsXRayMode = options.hintsXRayMode,
        byNameParameters = options.byNameParameters,
        implicitConversions = options.implicitConversions,
        namedParameters = options.namedParameters,
        hintsInPatternMatch = options.hintsInPatternMatch,
        closingLabels = options.closingLabels
      )

      pc0
        .inlayHints(pcParams)
        .asScala
        .flatMap { hints =>
          if (hints.isEmpty)
            inlayHintsFallback(pcParams.toSyntheticDecorationsParams)
              .map(adjustInlayHints)
          else
            Future.successful(adjustInlayHints(hints))
        }
    }
      .getOrElse(Future.successful(Nil.asJava))

  def completions(
    params: l.CompletionParams,
    token: CancelToken
  ): Future[l.CompletionList] =
    withPCAndAdjustLsp(params, isCompletion = true) { (pc, pos, adjust) =>
      val outlineFiles =
        outlineFilesProvider.getOutlineFiles(pc.buildTargetId())
      val offsetParams =
        CompilerOffsetParamsUtils.fromPos(pos, token, outlineFiles)
      pc.complete(offsetParams)
        .asScala
        .map { list =>
          adjust.adjustCompletionListInPlace(list)
          list
        }
    }.getOrElse(Future.successful(new l.CompletionList(Nil.asJava)))

  def completions(
    id: b.BuildTargetIdentifier,
    offsetParams: CompilerOffsetParams
  ): Future[l.CompletionList] =
    loadCompiler(id)
      .map { pc =>
        pc.complete(offsetParams).asScala
      }
      .getOrElse(Future.successful(new l.CompletionList(Nil.asJava)))

  def autoImports(
    params: l.TextDocumentPositionParams,
    name: String,
    findExtensionMethods: Boolean,
    token: CancelToken
  ): Future[JList[AutoImportsResult]] =
    withPCAndAdjustLsp(params) { (pc, pos, adjust) =>
      pc.autoImports(
        name,
        CompilerOffsetParamsUtils.fromPos(
          pos,
          token,
          outlineFilesProvider.getOutlineFiles(pc.buildTargetId())
        ),
        findExtensionMethods
      ).asScala
        .map { list =>
          list.asScala.foreach(adjust.adjustImportResult)
          list
        }
    }
      .getOrElse(Future.successful(Nil.asJava))

  def insertInferredType(
    params: l.TextDocumentPositionParams,
    token: CancelToken
  ): Future[JList[l.TextEdit]] =
    withPCAndAdjustLsp(params) { (pc, pos, adjust) =>
      val offset =
        CompilerOffsetParamsUtils.fromPos(
          pos,
          token,
          outlineFilesProvider.getOutlineFiles(pc.buildTargetId())
        )
      val result =
        if (
          pc.supportedCodeActions()
            .contains(CodeActionId.InsertInferredType)
        )
          pc.codeAction(
            offset,
            CodeActionId.InsertInferredType,
            Optional.empty()
          )
        else
          pc.insertInferredType(offset)

      result.asScala
        .map { edits =>
          adjust.adjustTextEdits(edits)
        }
    }
      .getOrElse(Future.successful(Nil.asJava))

  def inlineEdits(
    params: l.TextDocumentPositionParams,
    token: CancelToken
  ): Future[JList[l.TextEdit]] =
    withPCAndAdjustLsp(params) { (pc, pos, adjust) =>
      val offsetParams = CompilerOffsetParamsUtils.fromPos(
        pos,
        token,
        outlineFilesProvider.getOutlineFiles(pc.buildTargetId())
      )
      val result =
        if (
          pc.supportedCodeActions()
            .contains(CodeActionId.InlineValue)
        )
          pc.codeAction(
            offsetParams,
            CodeActionId.InlineValue,
            Optional.empty()
          )
        else
          pc.inlineValue(offsetParams)

      result.asScala
        .map { edits =>
          adjust.adjustTextEdits(edits)
        }
    }.getOrElse(Future.successful(Nil.asJava))

  def documentHighlight(
    params: l.TextDocumentPositionParams,
    token: CancelToken
  ): Future[JList[l.DocumentHighlight]] =
    withPCAndAdjustLsp(params) { (pc, pos, adjust) =>
      pc.documentHighlight(
        CompilerOffsetParamsUtils.fromPos(
          pos,
          token,
          outlineFilesProvider.getOutlineFiles(pc.buildTargetId())
        )
      ).asScala
        .map { highlights =>
          adjust.adjustDocumentHighlight(highlights)
        }
    }
      .getOrElse(Future.successful(Nil.asJava))

  def references(
    params: l.ReferenceParams,
    token: CancelToken,
    additionalAdjust: AdjustRange
  ): Future[List[ReferencesResult]] =
    withPCAndAdjustLsp(params) { case (pc0, pos, adjust) =>
      val requestParams = new pc.PcReferencesRequest(
        CompilerOffsetParamsUtils.fromPos(pos, token),
        params.getContext.isIncludeDeclaration(),
        JEither.forLeft(pos.start)
      )
      pc0.references(requestParams)
        .asScala
        .map(
          _.asScala
            .map(
              adjust.adjustReferencesResult(
                _,
                additionalAdjust,
                requestParams.file.text()
              )
            )
            .toList
        )
    }
      .getOrElse(Future.successful(Nil))

  def references(
    id: b.BuildTargetIdentifier,
    searchFiles: List[os.Path],
    includeDefinition: Boolean,
    symbols: List[String],
    additionalAdjust: AdjustRange,
    isCancelled: () => Boolean
  ): Future[List[ReferencesResult]] = {
    // we filter only Scala files, since `references` for Java are not implemented
    val filteredFiles = searchFiles.filter(_.isScala)
    val results =
      if (symbols.isEmpty || filteredFiles.isEmpty) Nil
      else
        withUncachedCompiler(id, debug) { compiler =>
          for {
            searchFile <- filteredFiles
            if !isCancelled()
          } yield {
            val uri                = searchFile.toNIO.toUri
            val (input, _, adjust) = sourceAdjustments(id, uri.toASCIIString)
            val requestParams = new pc.PcReferencesRequest(
              CompilerVirtualFileParams(uri, input.text),
              includeDefinition,
              JEither.forRight(symbols.head),
              symbols.tail.asJava
            )
            compiler
              .references(requestParams)
              .asScala
              .map(
                _.asScala
                  .map(
                    adjust
                      .adjustReferencesResult(_, additionalAdjust, input.text)
                  )
                  .toList
              )
          }
        }
          .getOrElse(Nil)

    Future.sequence(results).map(_.flatten)
  }

  def extractMethod(
    doc: l.TextDocumentIdentifier,
    range: l.Range,
    extractionPos: l.Position,
    token: CancelToken
  ): Future[JList[l.TextEdit]] =
    withPCAndAdjustLsp(doc.getUri, range, extractionPos) {
      (pc, metaRange, metaExtractionPos, adjust) =>
        val rangeParams = CompilerRangeParamsUtils.fromPos(
          metaRange,
          token,
          outlineFilesProvider.getOutlineFiles(pc.buildTargetId())
        )
        val extractionOffsetParams =
          CompilerOffsetParamsUtils.fromPos(metaExtractionPos, token)
        val result =
          if (
            pc.supportedCodeActions()
              .contains(CodeActionId.ImplementAbstractMembers)
          )
            pc.codeAction(
              rangeParams,
              CodeActionId.ExtractMethod,
              Optional.of(extractionOffsetParams)
            )
          else
            pc.extractMethod(
              rangeParams,
              extractionOffsetParams
            )

        result.asScala
          .map { edits =>
            adjust.adjustTextEdits(edits)
          }
    }
      .getOrElse(Future.successful(Nil.asJava))

  def convertToNamedArguments(
    position: l.TextDocumentPositionParams,
    argIndices: JList[Integer],
    token: CancelToken
  ): Future[JList[l.TextEdit]] =
    withPCAndAdjustLsp(position) { (pc, pos, adjust) =>
      val offset =
        CompilerOffsetParamsUtils.fromPos(
          pos,
          token,
          outlineFilesProvider.getOutlineFiles(pc.buildTargetId())
        )
      val result =
        if (
          pc.supportedCodeActions()
            .contains(CodeActionId.ConvertToNamedArguments)
        )
          pc.codeAction(
            offset,
            CodeActionId.ConvertToNamedArguments,
            Optional.of(argIndices)
          )
        else
          pc.convertToNamedArguments(offset, argIndices)

      result.asScala
        .map { edits =>
          adjust.adjustTextEdits(edits)
        }
    }
      .getOrElse(Future.successful(Nil.asJava))

  def implementAbstractMembers(
    params: l.TextDocumentPositionParams,
    token: CancelToken
  ): Future[JList[l.TextEdit]] =
    withPCAndAdjustLsp(params) { (pc, pos, adjust) =>
      val offsetParams = CompilerOffsetParamsUtils.fromPos(
        pos,
        token,
        outlineFilesProvider.getOutlineFiles(pc.buildTargetId())
      )

      val result =
        if (
          pc.supportedCodeActions()
            .contains(CodeActionId.ImplementAbstractMembers)
        )
          pc.codeAction(
            offsetParams,
            CodeActionId.ImplementAbstractMembers,
            None.asJava
          )
        else
          pc.implementAbstractMembers(offsetParams)

      result.asScala
        .map { edits =>
          adjust.adjustTextEdits(edits)
        }
    }
      .getOrElse(Future.successful(Nil.asJava))

  def codeAction(
    params: l.TextDocumentPositionParams,
    token: CancelToken,
    codeActionId: String,
    codeActionPayload: Option[Object]
  ): Future[JList[l.TextEdit]] =
    withPCAndAdjustLsp(params) { (pc, pos, adjust) =>
      pc.codeAction(
        CompilerOffsetParamsUtils.fromPos(
          pos,
          token,
          outlineFilesProvider.getOutlineFiles(pc.buildTargetId())
        ),
        codeActionId,
        codeActionPayload.asJava
      ).asScala
        .map { edits =>
          adjust.adjustTextEdits(edits)
        }
    }
      .getOrElse(Future.successful(Nil.asJava))

  def supportedCodeActions(path: os.Path): JList[String] =
    loadCompiler(path).map { pc =>
      pc.supportedCodeActions()
    }
      .getOrElse(Nil.asJava)

  def hover(
    params: HoverExtParams,
    token: CancelToken
  ): Future[Option[HoverSignature]] =
    withPCAndAdjustLsp(params) { (pc, pos, adjust) =>
      val params = CompilerRangeParamsUtils.offsetOrRange(
        pos,
        token,
        outlineFilesProvider.getOutlineFiles(pc.buildTargetId())
      )
      pc.hover(params).asScala
        .map(_.asScala.map(hover => adjust.adjustHoverResp(hover)))
    }
      .getOrElse(Future.successful(None))

  def compile(
    params: l.TextDocumentIdentifier,
    targetId: b.BuildTargetIdentifier,
    token: CancelToken
  ): Option[Future[CompileResult]] =
    withPCAndAdjustLsp(params, targetId) { (input, pc, adjust) =>
      val path = params.getUri.osPathFromUri
      val params0 = CompilerVirtualFileParams(
        new URI(input.path),
        input.value,
        token
      )
      /* FIXME Adjust positions in diagnostics with 'adjust' */
      pc.compile(params0).asScala
    }

  def prepareRename(
    params: l.TextDocumentPositionParams,
    token: CancelToken
  ): Future[Optional[l.Range]] =
    withPCAndAdjustLsp(params) { (pc, pos, adjust) =>
      pc.prepareRename(
        CompilerRangeParamsUtils.offsetOrRange(
          pos,
          token,
          outlineFilesProvider.getOutlineFiles(pc.buildTargetId())
        )
      ).asScala
        .map { range =>
          range.map(adjust.adjustRange(_))
        }
    }
      .getOrElse(Future.successful(None.asJava))

  def rename(
    params: l.RenameParams,
    token: CancelToken
  ): Future[JList[l.TextEdit]] =
    withPCAndAdjustLsp(params) { (pc, pos, adjust) =>
      pc.rename(
        CompilerRangeParamsUtils.offsetOrRange(
          pos,
          token,
          outlineFilesProvider.getOutlineFiles(pc.buildTargetId())
        ),
        params.getNewName
      ).asScala
        .map { edits =>
          adjust.adjustTextEdits(edits)
        }
    }
      .getOrElse(Future.successful(Nil.asJava))

  def definition(
    params: l.TextDocumentPositionParams,
    token: CancelToken
  ): Future[Seq[l.Location]] =
    definition(params = params, token = token, findTypeDef = false)

  def typeDefinition(
    params: l.TextDocumentPositionParams,
    token: CancelToken
  ): Future[Seq[l.Location]] =
    definition(params = params, token = token, findTypeDef = true)

  def info(
    path: os.Path,
    symbol: String
  ): Future[Option[PcSymbolInformation]] =
    loadCompiler(path, forceScala = true)
      .map(
        _.info(symbol).asScala
          .map(_.asScala.map(PcSymbolInformation.from))
      )
      .getOrElse(Future(None))

  def info(
    id: b.BuildTargetIdentifier,
    symbol: String
  ): Future[Option[PcSymbolInformation]] =
    loadCompiler(id)
      .map(
        _.info(symbol).asScala
          .map(_.asScala.map(PcSymbolInformation.from))
      )
      .getOrElse(Future(None))

  private def definition(
    params: l.TextDocumentPositionParams,
    token: CancelToken,
    findTypeDef: Boolean
  ): Future[Seq[l.Location]] =
    withPCAndAdjustLsp(params) { (pc, pos, adjust) =>
      val params0 = CompilerOffsetParamsUtils.fromPos(
        pos,
        token,
        outlineFilesProvider.getOutlineFiles(pc.buildTargetId())
      )
      val defResult =
        if (findTypeDef)
          pc.typeDefinition(params0)
        else
          pc.definition(CompilerOffsetParamsUtils.fromPos(pos, token))
      defResult.asScala.map { c =>
        for {
          (appliesTo, userPath) <- adjust.paths
          loc                   <- c.locations.asScala
          if loc.getUri.osPathFromUri == appliesTo
        }
          adjust.adjustLocations(c.locations())
        c.locations().asScala.toSeq
      }
    }.getOrElse(Future.successful(Nil))

  def signatureHelp(
    params: l.TextDocumentPositionParams,
    token: CancelToken
  ): Future[l.SignatureHelp] =
    withPCAndAdjustLsp(params) { (pc, pos, _) =>
      pc.signatureHelp(
        CompilerOffsetParamsUtils.fromPos(
          pos,
          token,
          outlineFilesProvider.getOutlineFiles(pc.buildTargetId())
        )
      ).asScala
    }.getOrElse(Future.successful(new l.SignatureHelp))

  def signatureHelp(
    id: b.BuildTargetIdentifier,
    offsetParams: CompilerOffsetParams
  ): Future[l.SignatureHelp] =
    loadCompiler(id)
      .map { pc =>
        pc.signatureHelp(offsetParams).asScala
      }
      .getOrElse(Future.successful(new l.SignatureHelp))

  def selectionRange(
    params: l.SelectionRangeParams,
    token: CancelToken
  ): Future[JList[l.SelectionRange]] =
    withPCAndAdjustLsp(params) { (pc, positions) =>
      val offsetPositions: JList[OffsetParams] =
        positions
          .asScala
          .map[OffsetParams] { pos =>
            CompilerOffsetParamsUtils.fromPos(
              pos,
              token,
              outlineFilesProvider.getOutlineFiles(pc.buildTargetId())
            )
          }
          .asJava
      pc.selectionRange(offsetPositions).asScala
    }.getOrElse(Future.successful(Nil.asJava))

  def getTasty(
    buildTargetId: b.BuildTargetIdentifier,
    path: os.Path
  ): Option[Future[String]] =
    loadCompiler(buildTargetId).map(
      _.getTasty(
        path.toNIO.toUri,
        false /* isHttpEnabled */
      ).asScala
    )

  /** Gets presentation compiler for a file.
    * @param path
    *   for which presentation compiler should be loaded, resolves build target based on this file
    * @param forceScala
    *   if should use Scala pc for `.java` files that are in a Scala build target, useful when Scala
    *   pc can handle Java files and Java pc implementation of a feature is missing
    */
  def loadCompiler(
    path: os.Path,
    forceScala: Boolean = false,
    isCompletion: Boolean = false
  ): Option[PresentationCompiler] =
    if (path.isScalaFilename || path.isJavaFilename)
      bspData.inverseSources(path).flatMap { target =>
        if (path.isScalaFilename)
          loadCompiler(target, isCompletion = isCompletion)
        else if (path.isJavaFilename && forceScala)
          loadCompiler(target, isCompletion = isCompletion)
            .orElse {
              loadJavaCompiler(target.module, javaFileManager, isCompletion)
            }
        else if (path.isJavaFilename)
          loadJavaCompiler(target.module, javaFileManager, isCompletion)
        else
          None
      }
    else
      None

  private def loadJavaCompiler(
    module: GlobalSymbolIndex.Module,
    javaFileManager: () => JavaFileManager,
    isCompletion: Boolean
  ): Option[PresentationCompiler] =
    (if (isCompletion) jCompletionCache else jcache)
      .computeIfAbsent(
        PresentationCompilerKey.JavaBuildTarget(
          new b.BuildTargetIdentifier(module.targetId)
        ),
        _ => {
          scribe.info(
            "Loading presentation compiler for Java" +
              (if (isCompletion) " (completions)" else "")
          )
          JavaLazyCompiler(
            javaFileManager,
            module,
            symbolSearch,
            completionItemPriority(),
            javaUserLoggerMaker,
            workspace,
            ec,
            sh,
            config0,
            bspData,
            languageClient
          )
        }
      )
      .await

  def loadCompiler(targetId: b.BuildTargetIdentifier): Option[PresentationCompiler] =
    loadCompiler(targetId, false)

  def loadCompiler(
    targetId: b.BuildTargetIdentifier,
    isCompletion: Boolean
  ): Option[PresentationCompiler] =
    withKeyAndDefault(targetId, isCompletion, debug) {
      case (key, getCompiler) =>
        (if (isCompletion) jCompletionCache else jcache)
          .computeIfAbsent(key, _ => getCompiler())
          .await
    }

  def unloadCompilerForTarget(
    targetId: b.BuildTargetIdentifier,
    isCompletion: Boolean = false
  ): Boolean = {
    val cache = if (isCompletion) jCompletionCache else jcache
    val previousValue =
      cache.remove(PresentationCompilerKey.ScalaBuildTarget(targetId))
    previousValue != null
  }

  private lazy val scala3Library = {
    val files = coursierapi.Fetch.create()
      .addDependencies(
        coursierapi.Dependency
          .of(
            "org.scala-lang",
            "scala-library",
            Properties.versionNumberString
          )
          .withTransitive(false)
      )
      .fetch()
      .asScala
    assert(files.length == 1)
    os.Path(files.head, os.pwd)
  }

  def loggerIdName(
    targetId: b.BuildTargetIdentifier,
    scalaVersion: String
  ): (String, String) = {
    val targetShortId = BspUtil.targetShortId(bspData, targetId)
    val id            = s"interactive-$scalaVersion-$targetShortId"
    val label         = s"Interactive $targetShortId"
    (id, label)
  }

  private def resolve(
    targetId: b.BuildTargetIdentifier,
    scalaVersion: String
  ): (MakeCompiler, Seq[os.Path]) = {
    val (id, label) = loggerIdName(targetId, scalaVersion)
    def logger()    = loggerManager.create(id, label)
    def setupPc(pc0: PresentationCompiler & pc.HasCompilerAccess): Unit = {
      pc0.compilerAccess.beforeAccess { (reqId, name, uri) =>
        interactiveCompilersStatuses.compute(
          pc0,
          (_, previousValueOrNull) =>
            ::((name, uri), Option(previousValueOrNull).getOrElse(Nil))
        )
        refreshStatus()
        languageClient.progress(
          PlasmonLanguageClient.ProgressDetails(id, label, reqId, name, done = false)
        )
      }
      pc0.compilerAccess.afterAccess { (reqId, name, uri) =>
        interactiveCompilersStatuses.compute(
          pc0,
          (_, previousValueOrNull) =>
            if (previousValueOrNull != null && previousValueOrNull.head == (name, uri))
              previousValueOrNull.tail match {
                case h :: t => ::(h, t)
                case Nil    => null
              }
            else
              previousValueOrNull
        )
        refreshStatus()
        languageClient.progress(
          PlasmonLanguageClient.ProgressDetails(id, label, reqId, name, done = true)
        )
      }
    }
    if ((scala2Compat && scalaVersion.startsWith("2.")) || scalaVersion.startsWith("3.")) {
      val makeCompiler = MakeCompiler { javaHome =>
        scribe.info("Loading Scala 3 PC")
        lazy val pc: Scala3PresentationCompiler = new Scala3PresentationCompiler(
          javaHome.toNIO,
          () => logger().consumer,
          emitDiagnostics = (uri, diags) => {
            val mappedToOpt = bspData.mappedTo(targetId, uri.toOsPath)
            val updateLine: Int => Int = mappedToOpt match {
              case Some(mappedTo) => l => mappedTo.lineForClient(l).getOrElse(l)
              case None           => identity
            }
            val logger0 = logger()
            for (diag <- diags if !diag.pos.span.exists)
              logger0.log(s"No span diagnostic: $diag")
            val diags0 = diags.collect {
              case diag if diag.pos.span.exists =>
                new l.Diagnostic(
                  new l.Range(
                    new l.Position(updateLine(diag.pos.startLine), diag.pos.startColumn),
                    new l.Position(updateLine(diag.pos.endLine), diag.pos.endColumn)
                  ),
                  diag.message,
                  diag.level match {
                    case dotty.tools.dotc.interfaces.Diagnostic.WARNING =>
                      l.DiagnosticSeverity.Warning
                    case dotty.tools.dotc.interfaces.Diagnostic.ERROR => l.DiagnosticSeverity.Error
                    case _ => l.DiagnosticSeverity.Information
                  },
                  s"Scala ${Properties.versionNumberString} presentation compiler"
                )
            }
            emitDiagnostics(pc, targetId.module, uri, diags0, logger0)
          },
          module = targetId.module
        )
        setupPc(pc)
        pc
      }
      val extraCp =
        if (scalaVersion.startsWith("2.")) Seq(scala3Library)
        else Nil
      (makeCompiler, extraCp)
    }
    else {
      val makeCompiler = MakeCompiler { javaHome =>
        scribe.error(s"Was asked presentation compiler for Scala $scalaVersion")
        val pc0 = new Scala2PresentationCompilerHandler().create(
          javaHome.toNIO,
          () => logger().consumer,
          targetId.module
        ) match {
          case pc1: (PresentationCompiler & pc.HasCompilerAccess) => pc1
          case _                                                  => ???
        }
        setupPc(pc0)
        pc0
      }
      (makeCompiler, Nil)
    }
  }

  private def withKeyAndDefault[T](
    targetId: b.BuildTargetIdentifier,
    isCompletion: Boolean,
    debug: Boolean
  )(
    f: (PresentationCompilerKey, () => MtagsPresentationCompiler) => Option[T]
  ): Option[T] =
    bspData.scalaTarget(targetId).flatMap { scalaTarget =>
      val scalaVersion            = scalaTarget.scalaVersion
      val (mtags, extraClassPath) = resolve(scalaTarget.info.getId, scalaVersion)

      def default() = {
        scribe.info(
          s"Loading presentation compiler for Scala ${scalaTarget.scalaVersion}" +
            (if (isCompletion) " (completions)" else "")
        )
        ScalaLazyCompiler(
          scalaTarget,
          scalaVersion,
          mtags,
          javaHome,
          symbolSearch,
          completionItemPriority(),
          extraClassPath,
          workspace,
          ec,
          sh,
          config0,
          plugins,
          bspData,
          debug = debug
        )
      }
      val key =
        PresentationCompilerKey.ScalaBuildTarget(scalaTarget.info.getId)
      f(key, default)
    }

  private def withUncachedCompiler[T](
    targetId: b.BuildTargetIdentifier,
    debug: Boolean
  )(f: PresentationCompiler => T): Option[T] =
    withKeyAndDefault(targetId, isCompletion = false, debug = debug) {
      case (key, getCompiler) =>
        val (out, shouldShutdown) = Option(jcache.get(key))
          .map((_, false))
          .getOrElse((getCompiler(), true))
        if (shouldShutdown)
          scribe.debug(s"starting uncached presentation compiler for $targetId")
        val compilerOpt = out.await
        val result      = compilerOpt.map(f)
        if (shouldShutdown) compilerOpt.foreach(_.shutdown())
        result
    }

  private def withPCAndAdjustLsp[T](
    params: l.SelectionRangeParams
  )(
    fn: (PresentationCompiler, JList[Position]) => T
  ): Option[T] = {
    val path = params.getTextDocument.getUri.osPathFromUri
    loadCompiler(path).map { compiler =>
      val input = path
        .toInputFromBuffers(buffers)
        .copy(path = params.getTextDocument.getUri)

      val positions =
        params.getPositions.asScala.flatMap(_.toMeta(input)).asJava

      fn(compiler, positions)
    }
  }

  private def withPCAndAdjustLsp[T](
    params: l.TextDocumentPositionParams,
    isCompletion: Boolean = false
  )(
    fn: (PresentationCompiler, Position, AdjustLspData) => T
  ): Option[T] = {
    val path = params.getTextDocument.getUri.osPathFromUri
    loadCompiler(path, isCompletion = isCompletion).flatMap { compiler =>
      val (input, pos, adjust) = sourceAdjustments(
        new b.BuildTargetIdentifier(compilerTargetId(compiler)),
        params
      )
      pos
        .toMeta(input)
        .map(metaPos => fn(compiler, metaPos, adjust))
    }
  }

  private def withPCAndAdjustLsp[T](
    params: l.InlayHintParams
  )(
    fn: (PresentationCompiler, Position, AdjustLspData) => T
  ): Option[T] = {
    val path = params.getTextDocument.getUri.osPathFromUri
    loadCompiler(path).flatMap { compiler =>
      val (input, pos, adjust) =
        sourceAdjustments(new b.BuildTargetIdentifier(compilerTargetId(compiler)), params)
      pos
        .toMeta(input)
        .map(metaPos => fn(compiler, metaPos, adjust))
    }
  }

  private def withPCAndAdjustLsp[T](
    uri: String,
    range: l.Range,
    extractionPos: l.Position
  )(
    fn: (
      PresentationCompiler,
      Position,
      Position,
      AdjustLspData
    ) => T
  ): Option[T] = {
    val path = uri.osPathFromUri
    loadCompiler(path).flatMap { compiler =>
      val (input, adjustRequest, adjustResponse) =
        sourceAdjustments(new b.BuildTargetIdentifier(compilerTargetId(compiler)), uri)
      for {
        metaRange <- new l.Range(
          adjustRequest(range.getStart),
          adjustRequest(range.getEnd)
        ).toMeta(input)
        metaExtractionPos <- adjustRequest(extractionPos).toMeta(input)
      } yield fn(
        compiler,
        metaRange,
        metaExtractionPos,
        adjustResponse
      )
    }
  }

  private def compilerTargetId(pc: PresentationCompiler): String =
    pc match {
      case s: Scala3PresentationCompiler => s.module.targetId
      case s: Scala2PresentationCompiler => s.module.targetId
      case j: JavaPresentationCompiler =>
        GlobalSymbolIndex.Module.fromString(j.moduleString).targetId
      case _ => "" // ???
    }

  private def withPCAndAdjustLsp[T](
    params: HoverExtParams
  )(
    fn: (
      PresentationCompiler,
      Position,
      AdjustLspData
    ) => T
  ): Option[T] = {

    val path = params.textDocument.getUri.osPathFromUri
    loadCompiler(path).flatMap { compiler =>
      val targetId = new b.BuildTargetIdentifier(compilerTargetId(compiler))
      if (params.range != null) {
        val (input, range, adjust) = sourceAdjustments(targetId, params)
        range.toMeta(input).map(fn(compiler, _, adjust))

      }
      else {
        val positionParams =
          new l.TextDocumentPositionParams(
            params.textDocument,
            params.getPosition
          )
        val (input, pos, adjust) = sourceAdjustments(targetId, positionParams)
        pos.toMeta(input).map(fn(compiler, _, adjust))
      }
    }
  }

  private def withPCAndAdjustLsp[T](
    params: l.TextDocumentIdentifier,
    targetId: b.BuildTargetIdentifier
  )(fn: (Input.VirtualFile, PresentationCompiler, AdjustLspData) => T): Option[T] =
    loadCompiler(targetId).map { compiler =>
      val (input, _, adjust) = sourceAdjustments(targetId, params.getUri)
      fn(input, compiler, adjust)
    }

  private def sourceAdjustments(
    targetId: b.BuildTargetIdentifier,
    params: l.TextDocumentPositionParams
  ): (Input.VirtualFile, l.Position, AdjustLspData) = {
    val (input, adjustRequest, adjustResponse) =
      sourceAdjustments(targetId, params.getTextDocument.getUri)
    (input, adjustRequest(params.getPosition), adjustResponse)
  }

  private def sourceAdjustments(
    targetId: b.BuildTargetIdentifier,
    params: l.InlayHintParams
  ): (Input.VirtualFile, l.Range, AdjustLspData) = {
    val (input, adjustRequest, adjustResponse) =
      sourceAdjustments(targetId, params.getTextDocument.getUri)
    val start    = params.getRange.getStart
    val end      = params.getRange.getEnd
    val newRange = new l.Range(adjustRequest(start), adjustRequest(end))
    (input, newRange, adjustResponse)
  }

  private def sourceAdjustments(
    targetId: b.BuildTargetIdentifier,
    params: HoverExtParams
  ): (Input.VirtualFile, l.Range, AdjustLspData) = {
    val (input, adjustRequest, adjustResponse) =
      sourceAdjustments(targetId, params.textDocument.getUri)
    val start    = params.range.getStart
    val end      = params.range.getEnd
    val newRange = new l.Range(adjustRequest(start), adjustRequest(end))
    (input, newRange, adjustResponse)
  }

  private def sourceAdjustments(
    target: b.BuildTargetIdentifier,
    uri: String
  ): (Input.VirtualFile, l.Position => l.Position, AdjustLspData) =
    sourceMapper.pcMapping(target, uri.osPathFromUri)

  private def toDebugCompletionType(
    kind: l.CompletionItemKind
  ): d.CompletionItemType =
    kind match {
      case l.CompletionItemKind.Constant      => d.CompletionItemType.VALUE
      case l.CompletionItemKind.Value         => d.CompletionItemType.VALUE
      case l.CompletionItemKind.Keyword       => d.CompletionItemType.KEYWORD
      case l.CompletionItemKind.Class         => d.CompletionItemType.CLASS
      case l.CompletionItemKind.TypeParameter => d.CompletionItemType.CLASS
      case l.CompletionItemKind.Operator      => d.CompletionItemType.FUNCTION
      case l.CompletionItemKind.Field         => d.CompletionItemType.FIELD
      case l.CompletionItemKind.Method        => d.CompletionItemType.METHOD
      case l.CompletionItemKind.Unit          => d.CompletionItemType.UNIT
      case l.CompletionItemKind.Enum          => d.CompletionItemType.ENUM
      case l.CompletionItemKind.Interface     => d.CompletionItemType.INTERFACE
      case l.CompletionItemKind.Constructor   => d.CompletionItemType.CONSTRUCTOR
      case l.CompletionItemKind.Folder        => d.CompletionItemType.FILE
      case l.CompletionItemKind.Module        => d.CompletionItemType.MODULE
      case l.CompletionItemKind.EnumMember    => d.CompletionItemType.ENUM
      case l.CompletionItemKind.Snippet       => d.CompletionItemType.SNIPPET
      case l.CompletionItemKind.Function      => d.CompletionItemType.FUNCTION
      case l.CompletionItemKind.Color         => d.CompletionItemType.COLOR
      case l.CompletionItemKind.Text          => d.CompletionItemType.TEXT
      case l.CompletionItemKind.Property      => d.CompletionItemType.PROPERTY
      case l.CompletionItemKind.Reference     => d.CompletionItemType.REFERENCE
      case l.CompletionItemKind.Variable      => d.CompletionItemType.VARIABLE
      case l.CompletionItemKind.Struct        => d.CompletionItemType.MODULE
      case l.CompletionItemKind.File          => d.CompletionItemType.FILE
      case _                                  => d.CompletionItemType.TEXT
    }

  private def toDebugCompletionItem(
    item: l.CompletionItem,
    adjustStart: Int,
    insertTextPosition: Position.Range
  ): d.CompletionItem = {
    val debugItem = new d.CompletionItem()
    debugItem.setLabel(item.getLabel)
    val (newText, range) = Option(item.getTextEdit).map(_.asScala) match {
      case Some(Left(textEdit)) =>
        (textEdit.getNewText, textEdit.getRange)
      case Some(Right(insertReplace)) =>
        (insertReplace.getNewText, insertReplace.getReplace)
      case None =>
        Option(item.getInsertText).orElse(Option(item.getLabel)) match {
          case Some(text) =>
            (text, insertTextPosition.toLsp)
          case None =>
            throw new RuntimeException(
              "Completion item does not contain expected data"
            )
        }
    }
    val start = range.getStart.getCharacter + adjustStart

    val length = range.getEnd.getCharacter - range.getStart.getCharacter
    debugItem.setLength(length)

    // remove snippets, since they are not supported in DAP
    val fullText = newText.replaceAll("\\$[1-9]+", "")

    val selection = fullText.indexOf("$0")

    // Find the spot for the cursor
    if (selection >= 0)
      debugItem.setSelectionStart(selection)

    debugItem.setDetail(item.getDetail)
    debugItem.setText(fullText.replace("$0", ""))
    debugItem.setStart(start)
    debugItem.setType(toDebugCompletionType(item.getKind))
    debugItem.setSortText(item.getFilterText)
    debugItem
  }

  def semanticdbTextDocument(
    source: os.Path,
    text: String,
    targetId: b.BuildTargetIdentifier
  ): s.TextDocument = {
    val pc = loadCompiler(targetId).getOrElse {
      sys.error(s"Got no compiler for target ${targetId.getUri}")
    }

    val (prependedLinesSize, modifiedText) =
      Option
        .when(source.isSbt)(
          bspData.sbtAutoImports(source)
        )
        .flatten
        .map(imports => (imports.size, SbtBuildTool.prependAutoImports(text, imports)))
        .getOrElse((0, text))

    // NOTE(olafur): it's unfortunate that we block on `semanticdbTextDocument`
    // here but to avoid it we would need to refactor the `Semanticdbs` trait,
    // which requires more effort than it's worth.
    val params = new CompilerVirtualFileParams(
      source.toNIO.toUri,
      modifiedText,
      token = EmptyCancelToken,
      outlineFiles = outlineFilesProvider.getOutlineFiles(pc.buildTargetId())
    )
    val bytes = pc
      .semanticdbTextDocument(params)
      .get(config0.timeoutDelay, config0.timeoutUnit)
    val textDocument = {
      val doc = s.TextDocument.parseFrom(bytes)
      if (doc.text.isEmpty()) doc.withText(text)
      else doc
    }
    if (prependedLinesSize > 0)
      cleanupAutoImports(textDocument, text, prependedLinesSize)
    else textDocument
  }

  private def cleanupAutoImports(
    document: s.TextDocument,
    originalText: String,
    linesSize: Int
  ): s.TextDocument = {

    def adjustRange(range: s.Range): Option[s.Range] = {
      val nextStartLine = range.startLine - linesSize
      val nextEndLine   = range.endLine - linesSize
      if (nextEndLine >= 0) {
        val nextRange = range.copy(
          startLine = nextStartLine,
          endLine = nextEndLine
        )
        Some(nextRange)
      }
      else None
    }

    val adjustedOccurences =
      document.occurrences.flatMap { occurence =>
        occurence.range
          .flatMap(adjustRange)
          .map(r => occurence.copy(range = Some(r)))
      }

    val adjustedDiagnostic =
      document.diagnostics.flatMap { diagnostic =>
        diagnostic.range
          .flatMap(adjustRange)
          .map(r => diagnostic.copy(range = Some(r)))
      }

    val adjustedSynthetic =
      document.synthetics.flatMap { synthetic =>
        synthetic.range
          .flatMap(adjustRange)
          .map(r => synthetic.copy(range = Some(r)))
      }

    s.TextDocument(
      schema = document.schema,
      uri = document.uri,
      text = originalText,
      md5 = MD5.compute(originalText),
      language = document.language,
      symbols = document.symbols,
      occurrences = adjustedOccurences,
      diagnostics = adjustedDiagnostic,
      synthetics = adjustedSynthetic
    )
  }

  def asJson: PresentationCompilers.AsJson =
    PresentationCompilers.AsJson(
      interactiveCompilersStatuses = interactiveCompilersStatuses.asScala.toMap.map {
        case (k, v) =>
          (k.toString, v)
      },
      debug = debug,
      symbolSearch = symbolSearch.asJson,
      compilerPlugins = plugins.asJson,
      outlineFilesProvider = outlineFilesProvider.asJson,
      cache = jcache.asScala.toMap.map {
        case (k, v) =>
          (k.asString, v.asJson)
      },
      completionCache = jCompletionCache.asScala.toMap.map {
        case (k, v) =>
          (k.asString, v.asJson)
      }
    )
}

object PresentationCompilers {

  sealed trait PresentationCompilerKey {
    def asString: String
  }
  object PresentationCompilerKey {
    final case class ScalaBuildTarget(id: b.BuildTargetIdentifier) extends PresentationCompilerKey {
      def asString = s"Scala(${id.getUri})"
    }
    final case class JavaBuildTarget(id: b.BuildTargetIdentifier) extends PresentationCompilerKey {
      def asString = s"Java(${id.getUri})"
    }
  }

  def noopCompiler: PresentationCompiler =
    new PresentationCompiler {
      def autoImports(
        x$1: String,
        x$2: scala.meta.pc.OffsetParams,
        x$3: java.lang.Boolean
      ): CompletableFuture[JList[scala.meta.pc.AutoImportsResult]] =
        CompletableFuture.completedFuture(Nil.asJava)
      def compile(
        params: scala.meta.pc.VirtualFileParams
      ): CompletableFuture[CompileResult] =
        CompletableFuture.completedFuture(
          new CompileResult {
            def diagnostics: JList[String] = Nil.asJava
            def fullTree: String           = ""
          }
        )
      def complete(
        params: scala.meta.pc.OffsetParams
      ): CompletableFuture[l.CompletionList] =
        CompletableFuture.completedFuture(new l.CompletionList)
      def completionItemResolve(
        x$1: l.CompletionItem,
        x$2: String
      ): CompletableFuture[l.CompletionItem] =
        CompletableFuture.completedFuture(new l.CompletionItem(""))
      def convertToNamedArguments(
        params: scala.meta.pc.OffsetParams,
        x$2: JList[Integer]
      ): CompletableFuture[JList[l.TextEdit]] =
        CompletableFuture.completedFuture(Nil.asJava)
      def definition(
        params: scala.meta.pc.OffsetParams
      ): CompletableFuture[scala.meta.pc.DefinitionResult] =
        CompletableFuture.completedFuture(
          new scala.meta.pc.DefinitionResult {
            def symbol: String               = ""
            def locations: JList[l.Location] = Nil.asJava
          }
        )
      def didChange(
        params: scala.meta.pc.VirtualFileParams
      ): CompletableFuture[JList[l.Diagnostic]] =
        CompletableFuture.completedFuture(Nil.asJava)
      def documentHighlight(
        params: scala.meta.pc.OffsetParams
      ): CompletableFuture[JList[l.DocumentHighlight]] =
        CompletableFuture.completedFuture(Nil.asJava)
      def extractMethod(
        params: scala.meta.pc.RangeParams,
        x$2: scala.meta.pc.OffsetParams
      ): CompletableFuture[JList[l.TextEdit]] =
        CompletableFuture.completedFuture(Nil.asJava)
      def getTasty(x$1: java.net.URI, x$2: Boolean): CompletableFuture[String] =
        CompletableFuture.completedFuture("")
      def hover(
        params: scala.meta.pc.OffsetParams
      ): CompletableFuture[Optional[scala.meta.pc.HoverSignature]] =
        CompletableFuture.completedFuture(Optional.empty())
      def implementAbstractMembers(
        params: scala.meta.pc.OffsetParams
      ): CompletableFuture[JList[l.TextEdit]] =
        CompletableFuture.completedFuture(Nil.asJava)
      def insertInferredType(
        params: scala.meta.pc.OffsetParams
      ): CompletableFuture[JList[l.TextEdit]] =
        CompletableFuture.completedFuture(Nil.asJava)
      def prepareRename(
        params: scala.meta.pc.OffsetParams
      ): CompletableFuture[Optional[l.Range]] =
        CompletableFuture.completedFuture(Optional.empty())
      def rename(
        params: scala.meta.pc.OffsetParams,
        x$2: String
      ): CompletableFuture[JList[l.TextEdit]] =
        CompletableFuture.completedFuture(Nil.asJava)
      def selectionRange(
        params: JList[scala.meta.pc.OffsetParams]
      ): CompletableFuture[JList[l.SelectionRange]] =
        CompletableFuture.completedFuture(Nil.asJava)
      def semanticdbTextDocument(
        x$1: java.net.URI,
        x$2: String
      ): CompletableFuture[Array[Byte]] =
        CompletableFuture.completedFuture(Array.emptyByteArray)
      def signatureHelp(
        params: scala.meta.pc.OffsetParams
      ): CompletableFuture[l.SignatureHelp] =
        CompletableFuture.completedFuture(new l.SignatureHelp)
      def typeDefinition(
        params: scala.meta.pc.OffsetParams
      ): CompletableFuture[scala.meta.pc.DefinitionResult] =
        CompletableFuture.completedFuture(
          new scala.meta.pc.DefinitionResult {
            def symbol: String               = ""
            def locations: JList[l.Location] = Nil.asJava
          }
        )

      def diagnosticsForDebuggingPurposes(): JList[String] = Nil.asJava
      def didClose(x$1: java.net.URI): Unit                = ()
      def isLoaded(): Boolean                              = true
      def shutdown(): Unit                                 = ()
      def restart(): Unit                                  = ()
      def scalaVersion(): String                           = ""

      def newInstance(
        x$1: String,
        x$2: JList[java.nio.file.Path],
        x$3: JList[String]
      ): scala.meta.pc.PresentationCompiler =
        this
      def withConfiguration(
        x$1: scala.meta.pc.PresentationCompilerConfig
      ): scala.meta.pc.PresentationCompiler =
        this
      def withExecutorService(
        x$1: java.util.concurrent.ExecutorService
      ): scala.meta.pc.PresentationCompiler =
        this
      def withScheduledExecutorService(
        x$1: java.util.concurrent.ScheduledExecutorService
      ): scala.meta.pc.PresentationCompiler =
        this
      def withSearch(
        x$1: scala.meta.pc.SymbolSearch
      ): scala.meta.pc.PresentationCompiler =
        this
      def withWorkspace(x$1: java.nio.file.Path): scala.meta.pc.PresentationCompiler =
        this
    }

  private object InlayHintCompat {
    // for compatibility with old inlay hint data
    def maybeFixInlayHintData(hint: l.InlayHint, uri: String): Unit =
      if (hint.getData.isInstanceOf[Array[?]])
        try {
          val labelParts = hint
            .getData
            .asInstanceOf[Array[Any]]
            .map {
              case data: l.Position => Right(data)
              case data: String     => Left(data)
            }
            .toList
          hint.setData(InlayHints.toData(uri, labelParts))
        }
        catch {
          case e: Exception =>
            scribe.warn(s"Failed to fix inlay hint data: $e")
        }
  }

  sealed trait MtagsPresentationCompiler {
    def compilerOpt: Option[PresentationCompiler]
    def await: Option[PresentationCompiler]
    def shutdown(): Unit

    def asJson: RawJson
  }

  trait LazyCompiler extends MtagsPresentationCompiler {

    protected def workspace: os.Path
    protected implicit def ec: ExecutionContextExecutorService
    protected def sh: ScheduledExecutorService
    protected def config0: PresentationCompilerConfig

    protected def bspData: BspData
    def buildTargetId: b.BuildTargetIdentifier
    protected def newCompiler(classpath: Seq[java.nio.file.Path]): PresentationCompiler

    protected val presentationCompilerRef = new AtomicReference[PresentationCompiler]

    protected val presentationCompilerFuture: Future[PresentationCompiler] =
      bspData
        .targetClasspath(buildTargetId)
        .getOrElse(Future.successful(Nil))
        .map { classpath =>
          // set wasResolved to avoid races on timeout below
          val classpathSeq = classpath.toAbsoluteClasspath.map(_.toNIO).toSeq
          val result       = newCompiler(classpathSeq)
          // Request finished, we can remove and shut down the fallback
          Option(presentationCompilerRef.getAndSet(result))
            .foreach(_.shutdown())
          result
        }

    def compilerOpt: Option[PresentationCompiler] =
      Option(presentationCompilerRef.get())

    def await: Option[PresentationCompiler] = {
      val pc = presentationCompilerRef.get()
      if (pc == null) {
        val result =
          try
            Some {
              Await.result(
                presentationCompilerFuture,
                Duration(config0.timeoutDelay, config0.timeoutUnit)
              )
            }
          catch {
            case _: TimeoutException =>
              scribe.warn(
                "Did not get information about classpath in time, no presentation compiler for now"
              )
              None
          }
        result
      }
      else
        Some(pc)
    }

    def shutdown(): Unit = {
      presentationCompilerFuture.onComplete {
        case Success(value) => value.shutdown()
        case _              =>
      }
      Option(presentationCompilerRef.get()).foreach(_.shutdown())
    }
  }

  case class ScalaLazyCompiler(
    scalaTarget: ScalaTarget,
    scalaVersion: String,
    makeCompiler: MakeCompiler,
    javaHome: os.Path,
    search: SymbolSearch,
    referenceCounter: CompletionItemPriority,
    additionalClasspath: Seq[os.Path] = Nil,
    workspace: os.Path,
    ec: ExecutionContextExecutorService,
    sh: ScheduledExecutorService,
    config0: PresentationCompilerConfig,
    plugins: CompilerPlugins,
    bspData: BspData,
    debug: Boolean
  ) extends LazyCompiler {

    def buildTargetId: b.BuildTargetIdentifier = scalaTarget.id

    protected def newCompiler(classpath: Seq[java.nio.file.Path]): PresentationCompiler = {
      val options = enrichWithReleaseOption(scalaTarget)
      // Best Effort option `-Ybest-effort` is useless for PC,
      // and it may unnecesarily dump semanticdb and tasty files
      val bestEffortOpt  = "-Ybest-effort"
      val withBetastyOpt = "-Ywith-best-effort-tasty"
      val nonBestEffortOptions =
        if (scalaTarget.isBestEffort)
          options
            .filter(_ != bestEffortOpt)
            .filter(_ != withBetastyOpt) :+ withBetastyOpt
        else options

      val bestEffortDirs = scalaTarget.info
        .getDependencies
        .asScala
        .flatMap { buildId =>
          if (scalaTarget.isBestEffort)
            bspData.scalaTarget(buildId).map(_.bestEffortPath)
          else None
        }
        .toSeq
      val selfBestEffortDir =
        if (scalaTarget.isBestEffort) Seq(scalaTarget.bestEffortPath)
        else Seq.empty

      fromMtags(
        makeCompiler,
        nonBestEffortOptions,
        classpath.map(os.Path(_)) ++ additionalClasspath ++ bestEffortDirs ++ selfBestEffortDir,
        javaHome,
        buildTargetId.module,
        search,
        referenceCounter,
        workspace,
        ec,
        sh,
        config0,
        plugins,
        debug = debug
      )
        .withBuildTargetName(scalaTarget.displayName)
    }

    def asJson: RawJson = {
      val refValue = presentationCompilerRef.get()
      val helper = ScalaLazyCompiler.AsJson(
        additionalClasspath = additionalClasspath,
        ref = Option(refValue).map {
          case pc: Scala3PresentationCompiler => ScalaLazyCompiler.pcAsJson(pc)
          case other                          => sys.error(s"Unsupported PC type: $other")
        },
        future = presentationCompilerFuture.value match {
          case None                                => "[on-going]"
          case Some(Failure(ex))                   => s"Failed: $ex"
          case Some(Success(pc)) if pc eq refValue => "[same as ref]"
          case Some(Success(pc))                   => s"$pc (ref: $refValue)"
        }
      )
      RawJson(writeToArrayReentrant(helper))
    }
  }

  object ScalaLazyCompiler {
    final case class AsJson(
      additionalClasspath: Seq[os.Path],
      ref: Option[PcAsJson],
      future: String
    )
    final case class PcAsJson()
    def pcAsJson(pc: Scala3PresentationCompiler): PcAsJson =
      PcAsJson()
    given JsonValueCodec[AsJson] =
      JsonCodecMaker.make
  }

  case class JavaLazyCompiler(
    javaFileManager: () => JavaFileManager,
    module: GlobalSymbolIndex.Module,
    search: SymbolSearch,
    completionItemPriority: CompletionItemPriority,
    javaUserLoggerMaker: java.util.function.Function[String, java.util.function.Consumer[String]],
    workspace: os.Path,
    ec: ExecutionContextExecutorService,
    sh: ScheduledExecutorService,
    config0: PresentationCompilerConfig,
    bspData: BspData,
    languageClient: PlasmonLanguageClient
  ) extends LazyCompiler {

    def buildTargetId: b.BuildTargetIdentifier =
      module match {
        case t: GlobalSymbolIndex.BuildTarget =>
          new b.BuildTargetIdentifier(t.targetId)
        case _: GlobalSymbolIndex.Standalone =>
          ???
      }

    protected def newCompiler(classpath: Seq[java.nio.file.Path]): PresentationCompiler = {
      val idSuffix   = BspUtil.targetShortId(bspData, buildTargetId)
      val nameSuffix = BspUtil.targetShortId(bspData, buildTargetId)
      val id         = s"interactive-java-$idSuffix"
      val label      = s"Interactive Java $nameSuffix"

      val pc = JavaPresentationCompiler(
        javaFileManager,
        javaUserLoggerMaker(module.targetId),
        module.asString,
        ec,
        wrapper = new JavaPresentationCompiler.Wrapper {
          def run[T](name: String)(f: => T): T = {
            val uuid = UUID.randomUUID().toString
            languageClient.progress(
              PlasmonLanguageClient.ProgressDetails(id, label, uuid, name, done = false)
            )
            try f
            finally
              languageClient.progress(
                PlasmonLanguageClient.ProgressDetails(id, label, uuid, name, done = true)
              )
          }
        }
      )
      configure(pc, search, completionItemPriority, workspace, ec, sh, config0)
        .newInstance(
          module.asString,
          classpath.asJava,
          Nil.asJava
        )
    }

    def asJson: RawJson = {
      val helper = JavaLazyCompiler.AsJson(
        ref = Option(presentationCompilerRef.get()).map(_.toString),
        future = presentationCompilerFuture.value match {
          case None              => "[on-going]"
          case Some(Failure(ex)) => s"Failed: $ex"
          case Some(Success(pc)) => pc.toString
        }
      )
      RawJson(writeToArrayReentrant(helper))
    }
  }

  object JavaLazyCompiler {
    final case class AsJson(
      ref: Option[String],
      future: String
    )
    given JsonValueCodec[AsJson] =
      JsonCodecMaker.make
  }

  private def fromMtags(
    makeCompiler: MakeCompiler,
    options: Seq[String],
    classpathSeq: Seq[os.Path],
    javaHome: os.Path,
    module: GlobalSymbolIndex.Module,
    symbolSearch: SymbolSearch,
    referenceCounter: CompletionItemPriority,
    workspace: os.Path,
    ec: ExecutorService,
    sh: ScheduledExecutorService,
    config0: PresentationCompilerConfig,
    plugins: CompilerPlugins,
    debug: Boolean
  ): PresentationCompiler = {
    val pc              = makeCompiler.createCompiler(javaHome)
    val filteredOptions = plugins.filterSupportedOptions(options)
    configure(pc, symbolSearch, referenceCounter, workspace, ec, sh, config0)
      .newInstance(
        module.asString,
        classpathSeq.map(_.toNIO).asJava,
        ((if (debug) log(workspace) else Nil) ++ filteredOptions).asJava
      )
  }

  private def configure(
    pc: PresentationCompiler,
    search: SymbolSearch,
    completionItemPriority: CompletionItemPriority,
    workspace: os.Path,
    ec: ExecutorService,
    sh: ScheduledExecutorService,
    config0: PresentationCompilerConfig
  ): PresentationCompiler =
    pc.withSearch(search)
      .withExecutorService(ec)
      .withCompletionItemPriority(completionItemPriority)
      .withWorkspace(workspace.toNIO)
      .withScheduledExecutorService(sh)
      .withReportsLoggerLevel("debug")
      .withReportContext(NopReportContext)
      .withConfiguration(config0)

  def logDest(workspace: os.Path): os.Path =
    workspace / ".plasmon/pc.log"

  private def log(workspace: os.Path): List[String] = {
    val logDest0 = logDest(workspace)
    os.makeDir.all(logDest0 / os.up)
    List(
      "-Ypresentation-debug",
      "-Ypresentation-verbose",
      "-Ypresentation-log",
      logDest0.toString
    )
  }

  private def enrichWithReleaseOption(scalaTarget: ScalaTarget) = {
    val isScala2 = scalaTarget.scalaInfo.getScalaVersion.startsWith("2.")

    // Try to adapt Scala 2 options to Scala 3, so that the compiler
    // doesn't error out

    var options = scalaTarget.scalac.getOptions.asScala.toSeq.flatMap {
      case opt if opt.startsWith("-release:") =>
        val opt0 = opt.stripPrefix("-release:").toIntOption match {
          case Some(n) if n < 17 => "-release:17"
          case _                 => opt
        }
        Seq(opt0)
      case "-language:_" if isScala2 =>
        // -language:_ not supported any more with Scala 3
        // implicitConversions should be the only non-enabled by default in Scala 3
        // that Scala 2 users should want
        Seq("-language:implicitConversions")
      case opt if opt.startsWith("-language:") && isScala2 =>
        val features = opt.stripPrefix("-language:").split(',')
        if (features.contains("implicitConversions")) Seq("-language:implicitConversions")
        else Nil
      case "-Wunused" if isScala2 =>
        Seq("-Wunused:all")
      case opt if opt.startsWith("-Xplugin:") && isScala2 =>
        // FIXME Log that we drop those options
        // No need to pass Scala 2 plugins to Scala 3 compiler
        Nil
      case opt if opt.startsWith("-Xlint:") && isScala2 =>
        // FIXME Log that we drop those options
        Nil
      case "-Wdead-code" | "-Wextra-implicit" | "-Wnumeric-widen" | "-Xsource:3" | "-Yrangepos"
          if isScala2 =>
        // FIXME Log that we drop those options
        Nil
      case opt => Seq(opt)
    }

    val releaseIndices = options.zipWithIndex.collect {
      case ("--release" | "-release", idx) => idx
    }
    for (releaseIdx <- releaseIndices if releaseIdx >= 0 && releaseIdx + 1 < options.length)
      options(releaseIdx + 1).toIntOption match {
        case Some(n) if n < 17 =>
          options = options.take(releaseIdx + 1) ++
            Seq("17") ++
            options.drop(releaseIdx + 2)
        case _ =>
      }

    options
  }

  final case class AsJson(
    interactiveCompilersStatuses: Map[String, Seq[(String, String)]],
    debug: Boolean,
    symbolSearch: SymbolSearchImpl.AsJson,
    compilerPlugins: CompilerPlugins.AsJson,
    outlineFilesProvider: OutlineFilesProvider.AsJson,
    cache: Map[String, RawJson],
    completionCache: Map[String, RawJson]
  )

  given JsonValueCodec[AsJson] =
    JsonCodecMaker.make
}
