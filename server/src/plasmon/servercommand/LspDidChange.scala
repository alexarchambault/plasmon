package plasmon.servercommand

import caseapp.core.RemainingArgs
import plasmon.Server
import plasmon.handlers.docchange.DocumentChange
import plasmon.index.Indexer
import plasmon.protocol.CommandClient

import scala.jdk.CollectionConverters.*

/** Stands in for an editor's copy of a file changing.
  *
  * With no `--content-file` this says "the buffer now holds what is on disk", which is what a
  * terminal usually means; pass one to hand the server an unsaved draft, the way an editor would.
  */
final case class LspDidChange(
  server: Server,
  client: CommandClient
) extends ServerCommandInstance[LspDidChangeOptions](client) {
  override def names = LspDidChange.names
  def run(options: LspDidChangeOptions, args: RemainingArgs): Unit = {

    val (path, _) = FileArg.single(args.all, options.uri, server.workingDir)

    val content = options.contentFile
      .map(contentFile => os.read(os.Path(contentFile, server.workingDir)))
      .getOrElse(os.read(path))

    val reactions = DocumentChange.didChange(server, path, content)

    if (options.applyEdits)
      for (edit <- reactions.edits)
        for (changed <- WorkspaceEdits.applyToDisk(edit))
          printLine(s"Edited $changed", toStderr = options.json)

    if (options.json)
      printJson(reactions.edits.asJava)
  }
}

object LspDidChange extends ServerCommand[LspDidChangeOptions] {
  def instance(
    server: Server,
    indexer: Indexer,
    client: CommandClient,
    lspServer: plasmon.jsonrpc.JsonrpcServer,
    pool: plasmon.command.ServerCommandThreadPools
  ): ServerCommandInstance[LspDidChangeOptions] =
    LspDidChange(server, client)
  override def names = List(
    List("lsp", "did-change"),
    List("lsp-did-change")
  )
}
