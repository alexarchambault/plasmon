package plasmon.handlers.ontypefmt

import org.eclipse.{lsp4j => l}
import plasmon.jsonrpc.RequestHandler
import plasmon.jsonrpc.Handlers
import plasmon.Server

import java.util.{List => JList}

import scala.concurrent.{ExecutionContext, Future}

import plasmon.PlasmonEnrichments._
import scala.jdk.CollectionConverters._

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
          )(server.pools.onTypeFormattingEc)
            .format()
        else
          onTypeFormattingProvider.format(params).asJava
      }(ec).asJava
    }

  def handlers(server: Server, ec: ExecutionContext): Handlers =
    Handlers(Nil, Seq(handler(server, ec)), Nil)
}
