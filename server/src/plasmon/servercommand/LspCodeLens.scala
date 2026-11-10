package plasmon.servercommand

import caseapp.core.RemainingArgs
import org.eclipse.lsp4j as l
import plasmon.Server
import plasmon.handlers.codelens.CodeLens
import plasmon.index.Indexer
import plasmon.protocol.CommandClient

import scala.jdk.CollectionConverters.*

final case class LspCodeLens(
  server: Server,
  indexer: Indexer,
  client: CommandClient,
  pools: plasmon.command.ServerCommandThreadPools
) extends ServerCommandInstance[LspCodeLensOptions](client) {
  override def names = LspCodeLens.names
  def run(options: LspCodeLensOptions, args: RemainingArgs): Unit = {

    val (path, uri) = FileArg.single(args.all, options.uri, server.workingDir)

    ensureFileContent(server, path)

    if (options.auto)
      AutoLoad(server, indexer, pools, path, printLine(_, toStderr = true), options.quiet)

    val handler = CodeLens.handler(server)

    val params = new l.CodeLensParams(new l.TextDocumentIdentifier(uri))

    val lenses = handler.call(params, requestLogger(options.quiet)).get()

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
    LspCodeLens(server, indexer, client, pool)
  override def names = List(
    List("lsp", "code-lens"),
    List("lsp-code-lens")
  )
}
