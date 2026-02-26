package plasmon.handlers

import org.eclipse.lsp4j as l
import plasmon.Server
import plasmon.ide.CancelTokens
import plasmon.jsonrpc.{Handlers, RequestHandler}

import java.util.concurrent.CompletableFuture

import scala.concurrent.{ExecutionContextExecutorService, Future}
import scala.meta.internal.metals.EmptyCancelToken

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
        }(using cancelTokensEces)
    }

  private def resolveItemHandler(
    server: Server,
    cancelTokensEces: ExecutionContextExecutorService
  ) =
    RequestHandler.of[l.CompletionItem, l.CompletionItem]("completionItem/resolve") {
      (item, logger) =>
        CancelTokens.future { _ =>
          server.presentationCompilers.completionItemResolve(item)
        }(using cancelTokensEces)
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
