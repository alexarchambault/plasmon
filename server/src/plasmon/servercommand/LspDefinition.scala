package plasmon.servercommand

import caseapp.core.RemainingArgs
import org.eclipse.lsp4j as l
import plasmon.{Logger, Server}
import plasmon.PlasmonEnrichments.*
import plasmon.handlers.Definition
import plasmon.index.Indexer
import plasmon.protocol.CommandClient

import java.net.URI

import scala.jdk.CollectionConverters.*

final case class LspDefinition(
  server: Server,
  client: CommandClient,
  pools: plasmon.command.ServerCommandThreadPools
) extends ServerCommandInstance[LspDefinitionOptions](client) {
  override def names = List(
    List("lsp", "definition"),
    List("lsp-definition")
  )
  def run(options: LspDefinitionOptions, args: RemainingArgs): Unit = {

    val uri = (args.all, options.uri) match {
      case (Seq(), None) =>
        sys.error("No file specified")
      case (Seq(strPath), None) =>
        os.Path(strPath, os.pwd).toNIO.toUri.toASCIIString
      case (Seq(), Some(uri)) =>
        uri
      case (Seq(_), Some(_)) =>
        sys.error("Cannot specify both a file and a URI")
      case (other, _) =>
        assert(other.length > 1)
        sys.error("Too many files specified (only one file accepted)")
    }

    val handler = Definition.definitionHandler(
      server,
      pools.cancelTokensEces,
      pools.hoverStuffEc
    )

    val params = new l.DefinitionParams(
      new l.TextDocumentIdentifier(uri),
      new l.Position(options.line, options.col)
    )

    val loggerManager = Logger.Manager.create {
      channel => msg =>
        printLine(s"[${channel.label}] $msg")
    }

    val logger = loggerManager.create("request", "Request")

    val locations = handler.call(params, logger).get()

    if (locations != null)
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
