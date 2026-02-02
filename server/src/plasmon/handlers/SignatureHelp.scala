package plasmon.handlers

import org.eclipse.{lsp4j => l}
import plasmon.jsonrpc.RequestHandler
import plasmon.jsonrpc.Handlers
import plasmon.Server

import scala.concurrent.ExecutionContextExecutorService
import plasmon.ide.CancelTokens

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
