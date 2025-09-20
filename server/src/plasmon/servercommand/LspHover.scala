package plasmon.servercommand

import caseapp.core.RemainingArgs
import org.eclipse.{lsp4j => l}
import plasmon.handlers.Hover
import plasmon.protocol.CommandClient
import plasmon.index.Indexer
import plasmon.{Logger, Server}

import java.net.URI

import scala.jdk.CollectionConverters.*
import plasmon.ide.HoverExtParams
import plasmon.PlasmonEnrichments.StringThingExtensions

final case class LspHover(
  server: Server,
  client: CommandClient,
  pools: plasmon.command.ServerCommandThreadPools
) extends ServerCommandInstance[LspHoverOptions](client) {
  override def names = List(
    List("lsp", "hover"),
    List("lsp-hover")
  )
  def run(options: LspHoverOptions, args: RemainingArgs): Unit = {

    val (path, uri) = (args.all, options.uri) match {
      case (Seq(), None) =>
        sys.error("No file specified")
      case (Seq(strPath), None) =>
        val path0 = os.Path(strPath, os.pwd)
        (path0, path0.toNIO.toUri.toASCIIString)
      case (Seq(), Some(uri)) =>
        (uri.osPathFromUri, uri)
      case (Seq(_), Some(_)) =>
        sys.error("Cannot specify both a file and a URI")
      case (other, _) =>
        assert(other.length > 1)
        sys.error("Too many files specified (only one file accepted)")
    }

    val handler = Hover.handler(
      server,
      pools.cancelTokensEces,
      pools.hoverStuffEc
    )

    val params = HoverExtParams(
      textDocument = new l.TextDocumentIdentifier(uri),
      position = new l.Position(options.line, options.col)
    )

    val loggerManager = Logger.Manager.create {
      channel => msg =>
        printLine(s"[${channel.label}] $msg")
    }

    val logger = loggerManager.create("request", "Request")

    val res = handler.call(params, logger).get()

    if (res != null) {
      for (range <- Option(res.getRange)) {
        val lineCount = range.getEnd.getLine - range.getStart.getLine + 1
        val content = os.read.lines.stream(path)
          .drop(range.getStart.getLine)
          .take(lineCount)
          .toVector
          .iterator
          .zipWithIndex
          .map {
            case (line, idx) =>
              var line0 = line
              if (idx == lineCount - 1)
                line0 = line0.take(range.getEnd.getCharacter)
              if (idx == 0)
                line0 =
                  if (lineCount == 1)
                    line0.drop(range.getStart.getCharacter)
                  else
                    (" " * range.getStart.getCharacter) + line0.drop(range.getStart.getCharacter)
              line0 + System.lineSeparator()
          }
          .mkString

        printLine(content)
        printLine("")
      }

      // not sure we print that correctly…
      if (res.getContents.isLeft)
        for (elem <- res.getContents.getLeft.asScala)
          printLine {
            if (elem.isLeft) elem.getLeft
            else elem.getRight.getValue
          }
      else
        // FIXME Take into account res.getContents.getRight.getKind
        printLine(res.getContents.getRight.getValue)
    }
  }
}

object LspHover extends ServerCommand[LspHoverOptions] {
  def instance(
    server: Server,
    indexer: Indexer,
    client: CommandClient,
    lspServer: plasmon.jsonrpc.JsonrpcServer,
    pool: plasmon.command.ServerCommandThreadPools
  ): ServerCommandInstance[LspHoverOptions] =
    LspHover(server, client, pool)
  override def names = List(
    List("lsp", "hover"),
    List("lsp-hover")
  )
}
