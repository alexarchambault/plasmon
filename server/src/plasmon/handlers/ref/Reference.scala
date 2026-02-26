package plasmon.handlers.ref

import org.eclipse.lsp4j as l
import plasmon.Server
import plasmon.PlasmonEnrichments.*
import plasmon.jsonrpc.{Handlers, RequestHandler}

import java.net.URI
import java.util.List as JList
import java.util.concurrent.CompletableFuture

import scala.concurrent.{ExecutionContext, Future}
import scala.jdk.CollectionConverters.*
import scala.meta.internal.mtags.{GlobalSymbolIndex, SourcePath}

object Reference {

  private def referencesHandler(
    server: Server,
    definitionStuffEc: ExecutionContext
  ) =
    RequestHandler.of[l.ReferenceParams, JList[l.Location]]("textDocument/references") {
      (params, logger) =>
        val f = Future {
          val path = params.getTextDocument.getUri.osPathFromUri
          server.bspData.inverseSources(path) match {
            case Some(targetId) =>
              SourcePath.withContext { ctx => // FIXME Escapes
                server.referenceIndex
                  .references(targetId.module, params)(using ctx)
                  .map { references =>
                    references
                      .flatMap(_.locations)
                      .asJava
                  }(using definitionStuffEc)
              }
            case None =>
              scribe.warn(s"No build target found for $path in references handler")
              Future.successful(Nil.asJava)
          }
        }(using server.pools.requestsEces)

        val f0 = {
          implicit val ec = server.pools.requestsEces
          f.flatten
        }

        f0.asJava
    }

  def handlers(
    server: Server,
    definitionStuffEc: ExecutionContext
  ): Handlers =
    Handlers(
      Nil,
      Seq(
        referencesHandler(server, definitionStuffEc)
      ),
      Nil
    )
}
