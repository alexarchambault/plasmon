package plasmon.handlers.docchange

import org.eclipse.lsp4j as l
import plasmon.Server
import plasmon.PlasmonEnrichments.*
import plasmon.index.Indexer
import plasmon.jsonrpc.{Handlers, NotificationHandler}

import scala.concurrent.Future
import scala.jdk.CollectionConverters.*
import scala.meta.internal.mtags.SourcePath
import scala.util.{Failure, Success}

object DocumentChange {

  private def didOpenHandler(server: Server) =
    NotificationHandler.of[l.DidOpenTextDocumentParams]("textDocument/didOpen") { (params, _) =>
      server.editorFileOpened(
        params.getTextDocument.getUri.osPathFromUri,
        params.getTextDocument.getText,
        params.getTextDocument.getVersion
      )
    }

  private def didChangeHandler(server: Server) =
    NotificationHandler.of[l.DidChangeTextDocumentParams]("textDocument/didChange") { (params, _) =>
      for (change <- params.getContentChanges.asScala)
        server.editorFileChanged(params.getTextDocument.getUri.osPathFromUri, change.getText)
    }

  private def didCloseHandler(server: Server) =
    NotificationHandler.of[l.DidCloseTextDocumentParams]("textDocument/didClose") { (params, _) =>
      server.editorFileClosed(params.getTextDocument.getUri.osPathFromUri)
    }

  private def didSaveHandler(server: Server) =
    NotificationHandler.of[l.DidSaveTextDocumentParams]("textDocument/didSave") { (params, _) =>
      server.editorFileSaved(params.getTextDocument.getUri.osPathFromUri)
    }

  def handlers(server: Server): Handlers =
    Handlers(
      Seq(
        didOpenHandler(server),
        didChangeHandler(server),
        didCloseHandler(server),
        didSaveHandler(server)
      ),
      Nil,
      Nil
    )
}
