package plasmon.servercommand

import caseapp.core.RemainingArgs
import org.eclipse.lsp4j as l
import plasmon.Server
import plasmon.handlers.Completion
import plasmon.index.Indexer
import plasmon.protocol.CommandClient

import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.jdk.CollectionConverters.*

final case class LspCompletion(
  server: Server,
  indexer: Indexer,
  client: CommandClient,
  pools: plasmon.command.ServerCommandThreadPools
) extends ServerCommandInstance[LspCompletionOptions](client) {
  override def names = LspCompletion.names
  def run(options: LspCompletionOptions, args: RemainingArgs): Unit = {

    val (path, uri) = FileArg.single(args.all, options.uri, server.workingDir)

    if (options.auto)
      AutoLoad(server, indexer, pools, path, printLine(_, toStderr = true))

    val col = options.col.filter(_ >= 0).getOrElse {
      os.read.lines(path).apply(options.line).length
    }

    val params = new l.CompletionParams(
      new l.TextDocumentIdentifier(uri),
      new l.Position(options.line, col),
      new l.CompletionContext(l.CompletionTriggerKind.Invoked)
    )
    val f   = Completion.completions(server, params)
    val res = Await.result(f, Duration.Inf)

    if (options.json)
      printJson(res)
    else
      for (item <- res.getItems.asScala)
        printLine(item.toString)
  }
}

object LspCompletion extends ServerCommand[LspCompletionOptions] {
  override def names = List(
    List("lsp", "completion"),
    List("lsp-completion"),
    List("lsp", "complete"),
    List("lsp-complete")
  )
  def instance(
    server: Server,
    indexer: Indexer,
    client: CommandClient,
    lspServer: plasmon.jsonrpc.JsonrpcServer,
    pool: plasmon.command.ServerCommandThreadPools
  ): ServerCommandInstance[LspCompletionOptions] =
    LspCompletion(server, indexer, client, pool)
}
