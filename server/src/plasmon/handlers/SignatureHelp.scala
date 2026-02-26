package plasmon.handlers

import org.eclipse.lsp4j as l
import plasmon.Server
import plasmon.ide.CancelTokens
import plasmon.jsonrpc.{Handlers, RequestHandler}

import scala.concurrent.ExecutionContextExecutorService

object SignatureHelp {
  def handlers(
    server: Server,
    cancelTokensEces: ExecutionContextExecutorService
  ): Handlers =
    Handlers(Nil, Seq(handler(server, cancelTokensEces)), Nil)

  // CancelTokens.future()

  def handler(
    server: Server,
    cancelTokensEces: ExecutionContextExecutorService
  ) =
    RequestHandler.of[l.TextDocumentPositionParams, l.SignatureHelp]("textDocument/signatureHelp") {
      (params, logger) =>
        CancelTokens.future { token =>
          server.presentationCompilers.signatureHelp(params, token)
        }(using cancelTokensEces)
    }
}
