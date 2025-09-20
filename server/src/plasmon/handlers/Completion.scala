package plasmon.handlers

import org.eclipse.{lsp4j => l}
import plasmon.jsonrpc.RequestHandler
import plasmon.jsonrpc.Handlers
import plasmon.Server

import scala.concurrent.{ExecutionContextExecutorService, Future}
import scala.meta.internal.metals.EmptyCancelToken
import java.util.concurrent.CompletableFuture
import plasmon.ide.CancelTokens

object Completion {

  def completions(server: Server, params: l.CompletionParams): Future[l.CompletionList] =
    server.presentationCompilers.completions(params, EmptyCancelToken)

  private def handler(
    server: Server,
    cancelTokensEces: ExecutionContextExecutorService
  ) =
    RequestHandler.of[l.CompletionParams, l.CompletionList]("textDocument/completion") {
      (params, logger) =>
        CancelTokens.future { token =>
          server.presentationCompilers.completions(params, token)
        }(cancelTokensEces)
    }

  private def resolveItemHandler(
    server: Server,
    cancelTokensEces: ExecutionContextExecutorService
  ) =
    RequestHandler.of[l.CompletionItem, l.CompletionItem]("completionItem/resolve") {
      (item, logger) =>
        CancelTokens.future { _ =>
          server.presentationCompilers.completionItemResolve(item)
        }(cancelTokensEces)
    }

  def handlers(
    server: Server,
    cancelTokensEces: ExecutionContextExecutorService
  ): Handlers =
    Handlers(
      Nil,
      Seq(
        handler(server, cancelTokensEces),
        resolveItemHandler(server, cancelTokensEces)
      ),
      Nil
    )

}
