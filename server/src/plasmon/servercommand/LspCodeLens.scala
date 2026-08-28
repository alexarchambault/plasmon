package plasmon.servercommand

import caseapp.core.RemainingArgs
import org.eclipse.lsp4j as l
import plasmon.{Logger, Server}
import plasmon.handlers.codelens.CodeLens
import plasmon.index.Indexer
import plasmon.protocol.CommandClient

import scala.jdk.CollectionConverters.*

final case class LspCodeLens(
  server: Server,
  client: CommandClient
) extends ServerCommandInstance[LspCodeLensOptions](client) {
  override def names = LspCodeLens.names
  def run(options: LspCodeLensOptions, args: RemainingArgs): Unit = {

    val (_, uri) = FileArg.single(args.all, options.uri, server.workingDir)

    val handler = CodeLens.handler(server)

    val params = new l.CodeLensParams(new l.TextDocumentIdentifier(uri))

    // Out of the way of the JSON on stdout, but not lost
    val loggerManager = Logger.Manager.create {
      channel => msg =>
        printLine(s"[${channel.label}] $msg", toStderr = options.json)
    }

    val lenses = handler.call(params, loggerManager.create("request", "Request")).get()

    if (options.json)
      printJson(Option(lenses).getOrElse(Nil.asJava))
    else if (lenses != null)
      for (lens <- lenses.asScala)
        printLine(Option(lens.getCommand).map(_.getTitle).getOrElse(lens.toString))
  }
}

object LspCodeLens extends ServerCommand[LspCodeLensOptions] {
  def instance(
    server: Server,
    indexer: Indexer,
    client: CommandClient,
    lspServer: plasmon.jsonrpc.JsonrpcServer,
    pool: plasmon.command.ServerCommandThreadPools
  ): ServerCommandInstance[LspCodeLensOptions] =
    LspCodeLens(server, client)
  override def names = List(
    List("lsp", "code-lens"),
    List("lsp-code-lens")
  )
}
