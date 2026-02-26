package plasmon
package handlers

import org.eclipse.lsp4j as l
import plasmon.ide.{CancelTokens, HoverExtParams}
import plasmon.jsonrpc.*

import scala.concurrent.{ExecutionContext, ExecutionContextExecutorService}

object Hover {

  def handler(
    server: Server,
    cancelTokensEces: ExecutionContextExecutorService,
    hoverStuffEc: ExecutionContext
  ): RequestHandler[HoverExtParams, l.Hover] =
    RequestHandler.of[HoverExtParams, l.Hover]("textDocument/hover") { (params, logger) =>
      CancelTokens.future { token =>
        server.presentationCompilers.hover(params, token)
          .map(_.map(_.toLsp()).orNull)(using hoverStuffEc)
      }(using cancelTokensEces)
    }

  def handlers(
    server: Server,
    cancelTokensEces: ExecutionContextExecutorService,
    hoverStuffEc: ExecutionContext
  ): Handlers = {
    val handler0 = handler(server, cancelTokensEces, hoverStuffEc)
    Handlers(Nil, Seq(handler0), Nil)
  }
}
