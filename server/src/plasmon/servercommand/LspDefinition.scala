package plasmon.servercommand

import caseapp.core.RemainingArgs
import org.eclipse.lsp4j as l
import plasmon.{Logger, Server}
import plasmon.PlasmonEnrichments.*
import plasmon.handlers.Definition
import plasmon.index.Indexer
import plasmon.protocol.CommandClient

import scala.jdk.CollectionConverters.*

final case class LspDefinition(
  server: Server,
  client: CommandClient,
  pools: plasmon.command.ServerCommandThreadPools
) extends ServerCommandInstance[LspDefinitionOptions](client) {
  override def names = LspDefinition.names
  def run(options: LspDefinitionOptions, args: RemainingArgs): Unit = {

    val (_, uri) = FileArg.single(args.all, options.uri, server.workingDir)

    val handler = Definition.definitionHandler(
      server,
      pools.cancelTokensEces,
      pools.definitionStuffEc
    )

    val params = new l.DefinitionParams(
      new l.TextDocumentIdentifier(uri),
      new l.Position(options.line, options.col)
    )

    // Out of the way of the JSON on stdout, but not lost
    val loggerManager = Logger.Manager.create {
      channel => msg =>
        printLine(s"[${channel.label}] $msg", toStderr = options.json)
    }

    val logger = loggerManager.create("request", "Request")

    val locations = handler.call(params, logger).get()

    if (options.json)
      printJson(Option(locations).getOrElse(Nil.asJava))
    else if (locations != null)
      for (location <- locations.asScala) {
        val path           = location.getUri.osPathFromUri
        val startLine: Int = location.getRange.getStart.getLine
        val startCol: Int  = location.getRange.getStart.getCharacter
        val endLine: Int   = location.getRange.getEnd.getLine
        val endCol: Int    = location.getRange.getEnd.getCharacter
        val coords =
          if (startLine == endLine && startCol == endCol) s"$startLine:$startCol"
          else s"$startLine:$startCol:$endLine:$endCol"
        printLine(s"$path:$coords")
      }
  }
}

object LspDefinition extends ServerCommand[LspDefinitionOptions] {
  def instance(
    server: Server,
    indexer: Indexer,
    client: CommandClient,
    lspServer: plasmon.jsonrpc.JsonrpcServer,
    pool: plasmon.command.ServerCommandThreadPools
  ): ServerCommandInstance[LspDefinitionOptions] =
    LspDefinition(server, client, pool)
  override def names = List(
    List("lsp", "definition"),
    List("lsp-definition")
  )
}
