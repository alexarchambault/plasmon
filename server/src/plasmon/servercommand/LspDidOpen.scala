package plasmon.servercommand

import caseapp.core.RemainingArgs
import plasmon.Server
import plasmon.handlers.docchange.DocumentChange
import plasmon.index.Indexer
import plasmon.protocol.CommandClient

import scala.jdk.CollectionConverters.*

/** Stands in for an editor opening a file.
  *
  * Opening a file is how the server is told to start looking after it - loading a presentation
  * compiler for it, compiling it, and filling in a package clause when it is a new, empty one. That
  * last part comes back as an edit for the editor to carry out, which is what `--apply-edits` does
  * here, and `--json` reports.
  */
final case class LspDidOpen(
  server: Server,
  client: CommandClient
) extends ServerCommandInstance[LspDidOpenOptions](client) {
  override def names = LspDidOpen.names
  def run(options: LspDidOpenOptions, args: RemainingArgs): Unit = {

    val (path, _) = FileArg.single(args.all, options.uri, server.workingDir)

    val content = options.contentFile
      .map(contentFile => os.read(os.Path(contentFile, server.workingDir)))
      .getOrElse(os.read(path))

    val reactions = DocumentChange.didOpen(server, path, content, options.version)

    if (options.applyEdits)
      for (edit <- reactions.edits)
        for (changed <- WorkspaceEdits.applyToDisk(edit))
          printLine(s"Edited $changed", toStderr = options.json)

    if (options.json)
      printJson(reactions.edits.asJava)
  }
}

object LspDidOpen extends ServerCommand[LspDidOpenOptions] {
  def instance(
    server: Server,
    indexer: Indexer,
    client: CommandClient,
    lspServer: plasmon.jsonrpc.JsonrpcServer,
    pool: plasmon.command.ServerCommandThreadPools
  ): ServerCommandInstance[LspDidOpenOptions] =
    LspDidOpen(server, client)
  override def names = List(
    List("lsp", "did-open"),
    List("lsp-did-open")
  )
}
