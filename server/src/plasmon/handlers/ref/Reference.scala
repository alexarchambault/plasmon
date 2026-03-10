package plasmon.handlers.ref

import org.eclipse.lsp4j as l
import plasmon.Server
import plasmon.PlasmonEnrichments.*
import plasmon.jsonrpc.{Handlers, RequestHandler}

import java.util.List as JList

import scala.concurrent.{ExecutionContext, Future}
import scala.jdk.CollectionConverters.*
import scala.meta.internal.mtags.SourcePath

object Reference {

  private def referencesHandler(
    server: Server,
    definitionStuffEc: ExecutionContext
  ) =
    RequestHandler.of[l.ReferenceParams, JList[l.Location]]("textDocument/references") {
      (params, _) =>
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

        f.flatten.asJava
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
