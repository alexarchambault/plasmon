package plasmon.handlers

import org.eclipse.{lsp4j => l}
import plasmon.jsonrpc.RequestHandler
import plasmon.jsonrpc.Handlers
import plasmon.Server

import java.util.{List => JList}

import scala.concurrent.{ExecutionContext, Future}
import scala.meta.pc.CancelToken
import scala.meta.internal.semanticdb
import scala.concurrent.ExecutionContextExecutorService
import scala.meta.internal.mtags.SourcePath
import scala.meta.internal.mtags.GlobalSymbolIndex
import java.net.URI
import java.util.concurrent.CompletableFuture
import scala.meta.internal.semanticdb.SymbolOccurrence
import scala.meta.internal.semanticdb.TextDocument
import scala.meta.internal.mtags.Mtags
import ch.epfl.scala.{bsp4j => b}
import scala.meta.dialects
import plasmon.ide.{Buffers, CancelTokens, TokenEditDistance, Trees}
import plasmon.index.BspData
import plasmon.pc.PresentationCompilers

import plasmon.PlasmonEnrichments._
import scala.jdk.CollectionConverters._
import plasmon.ide.PatchedSymbolIndex
import plasmon.semdb.AggregateSemanticdbs
import plasmon.Logger
import plasmon.semdb.TextDocumentLookup
import scala.util.Success
import scala.util.Failure

object Definition {

  // Many things here originally based on https://github.com/scalameta/metals/blob/2a6f8a437a1ce7c44140673edfe34bb74dfd33be/metals/src/main/scala/scala/meta/internal/metals/DefinitionProvider.scala#L319 or an earlier version of that file

  private def fromCompiler(
    presentationCompilers: PresentationCompilers,
    params: l.TextDocumentPositionParams,
    token: CancelToken,
    logger: Logger,
    dummyEc: ExecutionContext
  ): Future[Seq[l.Location]] = {
    val uri = params.getTextDocument.getUri
    if (
      uri.endsWith(".scala") || uri.endsWith(".sc") || uri.endsWith(".sbt") || uri.endsWith(".mill")
    ) {
      val f = presentationCompilers.definition(params, token)
      f.onComplete {
        case Success(locations) =>
          if (locations.isEmpty)
            logger.log("No location found via presentation compiler")
          else {
            logger.log(
              s"Found ${locations.length} " +
                (if (locations.lengthCompare(1) > 0) "locations" else "location")
            )
            for (loc <- locations)
              logger.log(s"  $loc")
          }
        case Failure(_) =>
      }(using dummyEc)
      f
    }
    else {
      logger.log("Not a Scala file")
      Future.successful(Nil)
    }
  }

  private def fromSemanticDb(
    semanticDbs: AggregateSemanticdbs,
    module: GlobalSymbolIndex.Module,
    params: l.TextDocumentPositionParams,
    buffers: Buffers,
    trees: Trees,
    bspData: BspData,
    patchedSymbolIndex: PatchedSymbolIndex,
    logger: Logger
  ): Option[Seq[l.Location]] = SourcePath.withContext { implicit ctx =>

    val path = params.getTextDocument.getUri.osPathFromUri

    def fromMtags(pos: l.Position): Option[SymbolOccurrence] = {
      val dialectOpt =
        if (path.isSbt) Some(dialects.Sbt)
        else {
          val targetIdOpt = module match {
            case t: GlobalSymbolIndex.BuildTarget => Some(new b.BuildTargetIdentifier(t.targetId))
            case _: GlobalSymbolIndex.Standalone  => None
          }
          targetIdOpt.flatMap { targetId =>
            bspData.getDialect(path.ext, path.isMill, targetId)
          }
        }
      logger.log(s"Using dialect ${dialectOpt.fold("none")(_.toString)}")
      dialectOpt.flatMap { dialect =>
        val occurrences = Mtags
          .allToplevels(path.toInput, dialect)
          .occurrences
        logger.log(s"Found ${occurrences.length} top-level symbols")
        val res = occurrences.find(_.encloses(pos))
        logger.log(if (res.isDefined) "Found symbol" else "Symbol not found")
        res
      }
    }

    def definitionFromSnapshot(semDb: TextDocument): Seq[l.Location] = {
      val sourceDistance = buffers.tokenEditDistance(module, path, semDb.text, trees)
      val occurrenceOpt =
        for {
          queryPosition <- sourceDistance
            .toOriginal(params.getPosition)
            .toPosition(params.getPosition)
          occurrence <- {
            val occOpt = semDb.occurrences.find { occ =>
              // empty range is set for anon classes definition
              occ.range.exists(!_.isPoint) && occ.encloses(queryPosition, true)
            }
            if (occOpt.isDefined) {
              logger.log(s"Found symbol: ${occOpt.mkString}")
              occOpt
            }
            else {
              logger.log(
                s"No symbol found at ${queryPosition.getLine}:${queryPosition.getCharacter} in semanticdb ${semDb.toString.take(200)}"
              )
              logger.log("Trying to get symbol via mtags")
              // In case of macros we might need to get the position from the presentation compiler
              val occOpt0 = fromMtags(queryPosition)
              logger.log {
                occOpt0 match {
                  case Some(occ0) =>
                    s"Found symbol via mtags: $occ0"
                  case None =>
                    "No symbol found"
                }
              }
              occOpt0
            }
          }
        } yield occurrence

      def syntheticApplyOccurrence(queryPosition: semanticdb.Range) =
        semDb.synthetics.collectFirst {
          case semanticdb.Synthetic(
                Some(range),
                semanticdb.SelectTree(_: semanticdb.OriginalTree, Some(semanticdb.IdTree(symbol)))
              )
              if queryPosition == range =>
            SymbolOccurrence(Some(range), symbol, SymbolOccurrence.Role.REFERENCE)
        }

      def definitionResult(occ: SymbolOccurrence): Seq[l.Location] = {
        import scala.meta.internal.semanticdb.Scala._
        if (occ.symbol.isLocal || semDb.definesSymbol(occ.symbol)) {
          logger.log("Symbol is in current file")
          // symbol is local so it is defined within the source
          val locationOpt = Definition.locationOpt(
            semDb,
            sourceDistance,
            occ.symbol,
            params.getTextDocument.getUri
          )
          logger.log {
            locationOpt.map(_.getRange) match {
              case Some(range) => s"Range: $range"
              case None        => "No location found"
            }
          }
          locationOpt.toSeq
        }
        else {
          logger.log("Looking for symbol in global index")
          val locations = patchedSymbolIndex.fromSymbol(module, occ.symbol).asScala.toSeq
          if (locations.isEmpty)
            logger.log("No location found for symbol")
          else {
            logger.log(
              s"Found ${locations.length} " +
                (if (locations.lengthCompare(1) > 0) "locations" else "location")
            )
            for (loc <- locations)
              logger.log(s"  $loc")
          }
          locations
        }
      }

      val applyResults = occurrenceOpt.flatMap(_.range).flatMap(syntheticApplyOccurrence)

      for (res <- applyResults)
        logger.log(s"Adding synthetic symbol: $res")

      (occurrenceOpt.toSeq ++ applyResults.toSeq).flatMap(definitionResult)
    }

    def logLookup(lookup: TextDocumentLookup): Unit =
      lookup match {
        case success: TextDocumentLookup.Success =>
          logger.log(s"Found semandicdb at ${success.path}")
        case stale: TextDocumentLookup.Stale =>
          logger.log(s"Found stale semandicdb at ${stale.file}")
        case _: TextDocumentLookup.NotFound =>
          logger.log("Semanticdb not found")
        case agg: TextDocumentLookup.Aggregate =>
          for (elem <- agg.errors)
            logLookup(elem)
        case err: TextDocumentLookup.Error =>
          logger.log("Error looking for semanticdb:")
          logger.log(err.e)
        case n: TextDocumentLookup.NoMatchingUri =>
          logger.log(s"No matching URI found for semanticdb ($n)")
      }

    val semdbLookup = semanticDbs.textDocument(path, module).toOption
    for (l <- semdbLookup)
      logLookup(l)
    val semdbOpt = semdbLookup.flatMap(_.documentIncludingStale)
    logger.log(if (semdbOpt.isDefined) "Got semanticdb" else "No semanticdb")
    semdbOpt.map(definitionFromSnapshot(_))
  }

  private def definitionResult(
    module: GlobalSymbolIndex.Module,
    server: Server,
    position: l.TextDocumentPositionParams,
    logger: Logger,
    token: CancelToken
  ): Future[Seq[l.Location]] = {
    // val source = position.getTextDocument.getUri.osPathFromUri
    // assert(source.isScalaFilename || source.isJavaFilename)
    // server.symbolIndex.indexSource()

    val useSemDb = true

    def fromSemDbOpt = fromSemanticDb(
      server.semanticdbs,
      module,
      position,
      server.editorState.buffers,
      server.editorState.trees,
      server.bspData,
      server.patchedSymbolIndex,
      logger
    )
    def fromCompilerFuture =
      fromCompiler(
        server.presentationCompilers,
        position,
        token,
        logger,
        server.pools.dummyEc
      )
    if (useSemDb)
      fromCompilerFuture.map { fromCompiler =>
        if (fromCompiler.isEmpty)
          fromSemDbOpt.getOrElse(fromCompiler)
        else
          fromCompiler
      }(using server.pools.definitionProviderEc)
    else
      fromCompilerFuture
  }

  def definitionHandler(
    server: Server,
    cancelTokensEces: ExecutionContextExecutorService,
    definitionStuffEc: ExecutionContext
  ) =
    RequestHandler.of[l.DefinitionParams, JList[l.Location]]("textDocument/definition") {
      (params, logger) =>
        val f = Future {
          val path = params.getTextDocument.getUri.osPathFromUri
          server.bspData.inverseSources(path) match {
            case Some(targetId) =>
              logger.log(s"Build target: ${targetId.getUri}")
              val source = params.getTextDocument.getUri.osPathFromUri
              if (source.isScalaFilename || source.isJavaFilename)
                CancelTokens.future { token =>
                  definitionResult(targetId.module, server, params, logger, token)
                    .map(_.asJava)(using definitionStuffEc)
                }(using cancelTokensEces)
              else
                CompletableFuture.completedFuture(Nil.asJava)
            case None =>
              logger.log(s"No build target found for $path")
              CompletableFuture.completedFuture(Nil.asJava)
          }
        }(using server.pools.requestsEces)

        f.asJava.thenCompose(x => x)
    }

  def handlers(
    server: Server,
    cancelTokensEces: ExecutionContextExecutorService,
    definitionStuffEc: ExecutionContext
  ): Handlers =
    Handlers(
      Nil,
      Seq(
        definitionHandler(server, cancelTokensEces, definitionStuffEc)
      ),
      Nil
    )

  private def locationOpt(
    snapshot: TextDocument,
    distance: TokenEditDistance,
    symbol: String,
    uri: String
  ): Option[l.Location] =
    for {
      location <- snapshot.toLocation(uri, symbol)
      result   <- distance.toRevised(location.getRange.getStart).toLocation(location)
    } yield result
}
