package plasmon.servercommand

import caseapp.core.RemainingArgs
import plasmon.Server
import plasmon.command.ServerCommandThreadPools
import plasmon.index.Indexer
import plasmon.protocol.CommandClient

import scala.concurrent.Await
import scala.concurrent.duration.Duration

/** Loads every module of every loaded build tool, the way the editor extension's "Load all modules"
  * does - that is, through the same [[ProjectOps.loadAllModules]] as `plasmon/loadAllModules`.
  *
  * [[Import]] does something similar, but lets callers tune what gets indexed and how; this is the
  * one-shot version an editor triggers right after loading a build tool.
  */
final case class ModuleLoadAll(
  server: Server,
  indexer: Indexer,
  client: CommandClient
) extends ServerCommandInstance[ModuleLoadAllOptions](client) {
  override def names = ModuleLoadAll.names
  def run(options: ModuleLoadAllOptions, args: RemainingArgs): Unit = {
    if (args.all.nonEmpty) {
      printLine(s"Unexpected arguments: ${args.all.mkString(" ")}", toStderr = true)
      exit(1)
    }
    Await.result(
      ProjectOps.loadAllModules(server, indexer, options.toplevelCacheOnly),
      Duration.Inf
    )
  }
}

object ModuleLoadAll extends ServerCommand[ModuleLoadAllOptions] {
  def instance(
    server: Server,
    indexer: Indexer,
    client: CommandClient,
    lspServer: plasmon.jsonrpc.JsonrpcServer,
    pool: ServerCommandThreadPools
  ): ServerCommandInstance[ModuleLoadAllOptions] =
    ModuleLoadAll(server, indexer, client)
  override def names = List(
    List("module", "load-all"),
    List("modules", "load-all"),
    List("module-load-all")
  )
}
