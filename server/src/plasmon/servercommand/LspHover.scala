package plasmon.servercommand

import caseapp.core.RemainingArgs
import org.eclipse.lsp4j as l
import plasmon.{Logger, Server}
import plasmon.handlers.Hover
import plasmon.ide.HoverExtParams
import plasmon.index.Indexer
import plasmon.protocol.CommandClient

import scala.jdk.CollectionConverters.*

final case class LspHover(
  server: Server,
  indexer: Indexer,
  client: CommandClient,
  pools: plasmon.command.ServerCommandThreadPools
) extends ServerCommandInstance[LspHoverOptions](client) {
  override def names = LspHover.names
  def run(options: LspHoverOptions, args: RemainingArgs): Unit = {

    val (path, uri) = FileArg.single(args.all, options.uri, server.workingDir)

    if (options.auto)
      AutoLoad(server, indexer, pools, path, printLine(_, toStderr = true))

    val handler = Hover.handler(
      server,
      pools.cancelTokensEces,
      pools.hoverStuffEc
    )

    val params = HoverExtParams(
      textDocument = new l.TextDocumentIdentifier(uri),
      position = new l.Position(options.line, options.col)
    )

    // Out of the way of the JSON on stdout, but not lost
    val loggerManager = Logger.Manager.create {
      channel => msg =>
        printLine(s"[${channel.label}] $msg", toStderr = options.json)
    }

    val logger = loggerManager.create("request", "Request")

    val res = handler.call(params, logger).get()

    if (options.json)
      printJson(res)
    else if (res != null) {
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
    LspHover(server, indexer, client, pool)
  override def names = List(
    List("lsp", "hover"),
    List("lsp-hover")
  )
}
