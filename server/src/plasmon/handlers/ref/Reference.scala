package plasmon.handlers.ref

import org.eclipse.{lsp4j => l}
import plasmon.jsonrpc.RequestHandler
import plasmon.jsonrpc.Handlers
import plasmon.Server

import java.util.{List => JList}

import scala.concurrent.ExecutionContext
import scala.meta.internal.mtags.SourcePath
import scala.meta.internal.mtags.GlobalSymbolIndex
import java.net.URI
import java.util.concurrent.CompletableFuture

import plasmon.PlasmonEnrichments._
import scala.jdk.CollectionConverters._
import scala.concurrent.Future

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
