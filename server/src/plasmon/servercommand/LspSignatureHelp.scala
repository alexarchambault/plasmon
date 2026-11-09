package plasmon.servercommand

import caseapp.core.RemainingArgs
import org.eclipse.lsp4j as l
import plasmon.Server
import plasmon.handlers.SignatureHelp
import plasmon.index.Indexer
import plasmon.protocol.CommandClient

import scala.jdk.CollectionConverters.*

final case class LspSignatureHelp(
  server: Server,
  indexer: Indexer,
  client: CommandClient,
  pools: plasmon.command.ServerCommandThreadPools
) extends ServerCommandInstance[LspSignatureHelpOptions](client) {
  override def names = LspSignatureHelp.names
  def run(options: LspSignatureHelpOptions, args: RemainingArgs): Unit = {

    val (path, uri) = FileArg.single(args.all, options.uri, server.workingDir)

    ensureFileContent(server, path)

    if (options.auto)
      AutoLoad(server, indexer, pools, path, printLine(_, toStderr = true), options.quiet)

    val handler = SignatureHelp.handler(server, pools.cancelTokensEces)

    val params = new l.TextDocumentPositionParams(
      new l.TextDocumentIdentifier(uri),
      new l.Position(options.line, options.col)
    )

    val res = handler.call(params, requestLogger(options.quiet)).get()

    if (options.json)
      printJson(res)
    else if (res != null)
      for (signature <- res.getSignatures.asScala)
        printLine(signature.getLabel)
  }
}

object LspSignatureHelp extends ServerCommand[LspSignatureHelpOptions] {
  def instance(
    server: Server,
    indexer: Indexer,
    client: CommandClient,
    lspServer: plasmon.jsonrpc.JsonrpcServer,
    pool: plasmon.command.ServerCommandThreadPools
  ): ServerCommandInstance[LspSignatureHelpOptions] =
    LspSignatureHelp(server, indexer, client, pool)
  override def names = List(
    List("lsp", "signature-help"),
    List("lsp-signature-help")
  )
}
