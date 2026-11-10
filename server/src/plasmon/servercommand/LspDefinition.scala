package plasmon.servercommand

import caseapp.core.RemainingArgs
import org.eclipse.lsp4j as l
import plasmon.Server
import plasmon.PlasmonEnrichments.*
import plasmon.handlers.Definition
import plasmon.ide.JarMeta
import plasmon.index.Indexer
import plasmon.protocol.CommandClient

import scala.jdk.CollectionConverters.*

final case class LspDefinition(
  server: Server,
  indexer: Indexer,
  client: CommandClient,
  pools: plasmon.command.ServerCommandThreadPools
) extends ServerCommandInstance[LspDefinitionOptions](client) {
  override def names = LspDefinition.names
  def run(options: LspDefinitionOptions, args: RemainingArgs): Unit = {

    val (path, uri) = FileArg.single(args.all, options.uri, server.workingDir)

    if (options.auto)
      AutoLoad(server, indexer, pools, path, printLine(_, toStderr = true), options.quiet)

    val handler = Definition.definitionHandler(
      server,
      pools.cancelTokensEces,
      pools.definitionStuffEc
    )

    val params = new l.DefinitionParams(
      new l.TextDocumentIdentifier(uri),
      new l.Position(options.line, options.col)
    )

    val logger = requestLogger(options.quiet)

    val locations = handler.call(params, logger).get()

    if (options.json) {
      if (options.archiveUris && locations != null)
        for (location <- locations.asScala)
          for {
            path       <- fileUriPath(location.getUri)
            archiveUri <- JarMeta.archiveUri(path)
          }
            location.setUri(archiveUri)
      printJson(Option(locations).getOrElse(Nil.asJava))
    }
    else if (locations != null)
      for (location <- locations.asScala) {
        val path = fileUriPath(location.getUri) match {
          case Some(path0) =>
            if (options.archiveUris) JarMeta.archivePath(path0).getOrElse(path0.toString)
            else path0.toString
          case None =>
            location.getUri
        }
        // 1-based, the way compilers print positions, and editors and terminals read
        // `path:line:col` - unlike LSP positions (--line / --col, and the --json output)
        val startLine: Int = location.getRange.getStart.getLine + 1
        val startCol: Int  = location.getRange.getStart.getCharacter + 1
        val endLine: Int   = location.getRange.getEnd.getLine + 1
        val endCol: Int    = location.getRange.getEnd.getCharacter + 1
        val coords =
          if (startLine == endLine && startCol == endCol) s"$startLine:$startCol"
          else s"$startLine:$startCol:$endLine:$endCol"
        printLine(s"$path:$coords")
      }
  }

  private def fileUriPath(uri: String): Option[os.Path] =
    Option(uri).filter(_.startsWith("file:")).flatMap(_.maybeOsPathFromUri)
}

object LspDefinition extends ServerCommand[LspDefinitionOptions] {
  def instance(
    server: Server,
    indexer: Indexer,
    client: CommandClient,
    lspServer: plasmon.jsonrpc.JsonrpcServer,
    pool: plasmon.command.ServerCommandThreadPools
  ): ServerCommandInstance[LspDefinitionOptions] =
    LspDefinition(server, indexer, client, pool)
  override def names = List(
    List("lsp", "definition"),
    List("lsp-definition")
  )
}
