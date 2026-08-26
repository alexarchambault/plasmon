package plasmon.servercommand

import caseapp.core.RemainingArgs
import org.eclipse.lsp4j as l
import plasmon.{Logger, Server}
import plasmon.handlers.SignatureHelp
import plasmon.index.Indexer
import plasmon.protocol.CommandClient

import scala.jdk.CollectionConverters.*

final case class LspSignatureHelp(
  server: Server,
  client: CommandClient,
  pools: plasmon.command.ServerCommandThreadPools
) extends ServerCommandInstance[LspSignatureHelpOptions](client) {
  override def names = LspSignatureHelp.names
  def run(options: LspSignatureHelpOptions, args: RemainingArgs): Unit = {

    val (_, uri) = FileArg.single(args.all, options.uri, server.workingDir)

    val handler = SignatureHelp.handler(server, pools.cancelTokensEces)

    val params = new l.TextDocumentPositionParams(
      new l.TextDocumentIdentifier(uri),
      new l.Position(options.line, options.col)
    )

    // Out of the way of the JSON on stdout, but not lost
    val loggerManager = Logger.Manager.create {
      channel => msg =>
        printLine(s"[${channel.label}] $msg", toStderr = options.json)
    }

    val res = handler.call(params, loggerManager.create("request", "Request")).get()

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
    LspSignatureHelp(server, client, pool)
  override def names = List(
    List("lsp", "signature-help"),
    List("lsp-signature-help")
  )
}
