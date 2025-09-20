package plasmon.servercommand

import caseapp.core.RemainingArgs
import org.eclipse.{lsp4j => l}
import plasmon.handlers.Completion
import plasmon.protocol.CommandClient
import plasmon.index.Indexer
import plasmon.Server

import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.jdk.CollectionConverters.*

final case class LspCompletion(
  server: Server,
  client: CommandClient
) extends ServerCommandInstance[LspCompletionOptions](client) {
  def run(options: LspCompletionOptions, args: RemainingArgs): Unit = {
    val path = args.all match {
      case Seq()    => sys.error("No argument provided")
      case Seq(arg) => os.Path(arg, server.workingDir)
      case _        => sys.error("Too many arguments provided")
    }

    val col = options.col.filter(_ >= 0).getOrElse {
      os.read.lines(path).apply(options.line).length
    }

    val params = new l.CompletionParams(
      new l.TextDocumentIdentifier(path.toNIO.toUri.toASCIIString),
      new l.Position(options.line, col)
    )
    val f   = Completion.completions(server, params)
    val res = Await.result(f, Duration.Inf)

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
    LspCompletion(server, client)
}
