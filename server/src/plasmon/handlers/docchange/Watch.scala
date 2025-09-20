package plasmon.handlers.docchange

import plasmon.PlasmonEnrichments.*
import plasmon.Server
import plasmon.jsonrpc.{Handlers, RequestHandler}
import org.eclipse.{lsp4j => l}

import scala.jdk.CollectionConverters._
import java.util.concurrent.CompletableFuture

// FIXME Unused?
object Watch {

  def handler(server: Server) =
    RequestHandler.of[l.DidChangeWatchedFilesParams, Unit]("workspace/didChangeWatchedFiles") {
      (params, _) =>
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
        CompletableFuture.completedFuture(())
    }

  def handlers(server: Server) =
    Handlers(Nil, Seq(handler(server)), Nil)

}
