package plasmon.handlers.ontypefmt

import org.eclipse.lsp4j as l
import plasmon.Server
import plasmon.PlasmonEnrichments.*
import plasmon.jsonrpc.{Handlers, RequestHandler}

import java.util.List as JList

import scala.concurrent.{ExecutionContext, Future}
import scala.jdk.CollectionConverters.*

object OnTypeFormatting {

  def handler(server: Server, ec: ExecutionContext) =
    RequestHandler.of[l.DocumentOnTypeFormattingParams, JList[l.TextEdit]](
      "textDocument/onTypeFormatting"
    ) { (params, logger) =>
      val onTypeFormattingProvider =
        new OnTypeFormattingProvider(
          server.editorState.buffers,
          server.editorState.trees,
          server.bspData
        )
      Future {
        val isJava = params.getTextDocument.getUri.endsWith(".java")
        if (isJava)
          new JavaFormattingProvider(
            server.editorState.buffers,
            server.bspData
          )(using server.pools.onTypeFormattingEc)
            .format()
        else
          onTypeFormattingProvider.format(params).asJava
      }(using ec).asJava
    }

  def handlers(server: Server, ec: ExecutionContext): Handlers =
    Handlers(Nil, Seq(handler(server, ec)), Nil)
}
