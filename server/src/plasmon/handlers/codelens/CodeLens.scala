package plasmon.handlers.codelens

import org.eclipse.{lsp4j => l}
import plasmon.jsonrpc.CommandHandler.ParamsHelpers.*
import plasmon.jsonrpc.{CommandHandler, RequestHandler}
import plasmon.jsonrpc.Handlers
import plasmon.Server

import java.util.{List => JList}

import scala.concurrent.Future
import plasmon.ide.ClientCommands
import scala.meta.internal.mtags.SourcePath
import java.util.concurrent.CompletableFuture
import scala.meta.internal.mtags.GlobalSymbolIndex
import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker

import plasmon.PlasmonEnrichments._
import scala.jdk.CollectionConverters._
import plasmon.semdb.Semanticdbs
import scala.concurrent.ExecutionContext

object CodeLens {

  private def findLenses(
    codeLensProviders: Seq[CodeLensProvider],
    semanticdbs: Semanticdbs,
    module: GlobalSymbolIndex.Module,
    path: os.Path,
    dummyEc: ExecutionContext
  ): Future[Seq[l.CodeLens]] = {
    val semanticdbCodeLenses = semanticdbs
      .textDocument(path, module)
      .toOption
      .flatMap(_.documentIncludingStale)
      .map { textDocument =>
        codeLensProviders
          .filter(_.isEnabled)
          .map { provider =>
            provider.codeLenses(module, SourcePath.Standard(path.toNIO), textDocument)
          }
      }
      .getOrElse(Nil)

    {
      implicit val ec0: ExecutionContext = dummyEc
      Future.sequence(semanticdbCodeLenses).map(_.flatten)
    }
  }

  def handler(server: Server) =
    RequestHandler.of[l.CodeLensParams, JList[l.CodeLens]]("textDocument/codeLens") {
      (params, logger) =>
        val path = params.getTextDocument.getUri.osPathFromUri
        server.bspData.inverseSources(path) match {
          case Some(target) =>

            val codeLensProviders = Seq[CodeLensProvider](
              new SuperMethodCodeLens(
                server.editorState.buffers,
                server.editorState.trees
              )(using server.pools.codeLensEc)
            )

            val futureLenses = findLenses(
              codeLensProviders,
              server.semanticdbs,
              target.module,
              path,
              server.pools.dummyEc
            )

            futureLenses
              .map { lenses =>
                lenses
                  .toList
                  .sortBy(_.getRange.getStart.getLine)
                  .asJava
              }(using server.pools.codeLensEc)
              .asJava
          case None =>
            scribe.info(s"textDocument/codeLens: no build target found for $path")
            CompletableFuture.completedFuture(Nil.asJava)
        }
    }

  final case class GotoCommandParams(symbol: String, module: String)
  object GotoCommandParams {
    implicit val codec: JsonValueCodec[GotoCommandParams] = JsonCodecMaker.make
  }
  def commandHandlers(server: Server) =
    Seq(
      CommandHandler.of("goto") { (params, logger) =>
        params.as[GsonValue[GotoCommandParams]]("goto") { params =>
          val module = GlobalSymbolIndex.Module.fromString(params.value.module)
          Future {
            val locations = SourcePath.withContext { implicit ctx =>
              server.patchedSymbolIndex
                .fromSymbol(module, params.value.symbol)
                .asScala
            }
            for (location <- locations.headOption)
              server.languageClient.metalsExecuteClientCommand(
                ClientCommands.GotoLocation.toExecuteCommandParams(
                  ClientCommands.WindowLocation(location.getUri, location.getRange)
                )
              )
          }(using server.pools.definitionProviderEc).asJavaObject
        }(using ArgParser.gson[GotoCommandParams])
      }
    )

  def handlers(server: Server): Handlers =
    Handlers(Nil, Seq(handler(server)), commandHandlers(server))
}
