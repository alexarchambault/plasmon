package plasmon.handlers.docchange

import plasmon.PlasmonEnrichments.*
import plasmon.Server
import plasmon.jsonrpc.{Handlers, RequestHandler}
import org.eclipse.{lsp4j => l}

import scala.jdk.CollectionConverters._
import java.util.concurrent.CompletableFuture
import scala.concurrent.Future

// FIXME Unused?
object Watch {

  def handler(server: Server) =
    RequestHandler.of[l.DidChangeWatchedFilesParams, Unit]("workspace/didChangeWatchedFiles") {
      (params, _) =>
        Future {
          for (event <- params.getChanges.asScala) {
            val path = event.getUri.osPathFromUri
            if (path.last.endsWith(".scala") || path.last.endsWith(".java"))
              event.getType match {
                case l.FileChangeType.Created =>
                  server.fileChangedOrCreated(path, created = true)
                case l.FileChangeType.Changed =>
                  server.fileChangedOrCreated(path, created = false)
                case l.FileChangeType.Deleted =>
                  server.fileDeleted(path)
              }
          }
        }(using server.pools.requestsEces).asJava
    }

  def handlers(server: Server) =
    Handlers(Nil, Seq(handler(server)), Nil)

}
