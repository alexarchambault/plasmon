package plasmon.servercommand

import caseapp.core.RemainingArgs
import com.github.plokhotnyuk.jsoniter_scala.core.{WriterConfig, writeToString}
import plasmon.Server
import plasmon.command.ServerCommandThreadPools
import plasmon.index.Indexer
import plasmon.protocol.CommandClient

/** Lists the modules a file belongs to - what [[ModuleLoad]] picks from. */
final case class ModuleList(
  server: Server,
  client: CommandClient
) extends ServerCommandInstance[ModuleListOptions](client) {
  override def names = ModuleList.names
  def run(options: ModuleListOptions, args: RemainingArgs): Unit = {

    val (file, _) = FileArg.single(args.all, options.uri, server.workingDir)

    val modules = ProjectOps.listModules(file, server)

    if (options.json)
      printLine(
        writeToString(modules, WriterConfig.withIndentionStep(2))(using
          ProjectOps.ModuleInfo.seqCodec
        )
      )
    else if (modules.isEmpty)
      printLine(s"No module found for $file", toStderr = true)
    else
      for (module <- modules)
        printLine(s"${module.uri} (${module.description})")
  }
}

object ModuleList extends ServerCommand[ModuleListOptions] {
  def instance(
    server: Server,
    indexer: Indexer,
    client: CommandClient,
    lspServer: plasmon.jsonrpc.JsonrpcServer,
    pool: ServerCommandThreadPools
  ): ServerCommandInstance[ModuleListOptions] =
    ModuleList(server, client)
  override def names = List(
    List("module", "list"),
    List("modules", "list"),
    List("module-list")
  )
}
