package plasmon.servercommand

import caseapp.core.RemainingArgs
import plasmon.Server
import plasmon.PlasmonEnrichments.StringThingExtensions
import plasmon.command.ServerCommandThreadPools
import plasmon.index.Indexer
import plasmon.protocol.CommandClient

import scala.concurrent.Await
import scala.concurrent.duration.Duration

/** Loads the module a file belongs to, the way the editor extension's "Load module" does.
  *
  * Which module that is is worked out server-side (see [[ProjectOps.listModules]]), so callers only
  * have to name a file - the same thing the `plasmon/listModulesOf` and `plasmon/loadModule` LSP
  * commands do together.
  */
final case class ModuleLoad(
  server: Server,
  indexer: Indexer,
  client: CommandClient,
  pools: ServerCommandThreadPools
) extends ServerCommandInstance[ModuleLoadOptions](client) {
  override def names = ModuleLoad.names
  def run(options: ModuleLoadOptions, args: RemainingArgs): Unit = {

    val (file, _) = FileArg.single(args.all, options.uri, server.workingDir)

    val modules = ProjectOps.listModules(file, server)
    if (modules.isEmpty) {
      printLine(s"No module found for $file", toStderr = true)
      exit(1)
    }

    val toLoad =
      if (options.all) modules
      else
        Seq(modules.find(!_.alreadyLoaded).getOrElse(modules.head))

    for (module <- toLoad) {
      val res = Await.result(
        ProjectOps.loadModule(
          server,
          indexer,
          pools,
          module.workspace.osPathFromUri,
          module.server,
          module.uri
        ),
        Duration.Inf
      )
      res match {
        case Left(err) =>
          printLine(s"Error loading module ${module.label}: $err", toStderr = true)
          exit(1)
        case Right(true) =>
          printLine(s"Loaded module ${module.uri}", toStderr = true)
        case Right(false) =>
          printLine(s"Module ${module.uri} already loaded", toStderr = true)
      }
    }
  }
}

object ModuleLoad extends ServerCommand[ModuleLoadOptions] {
  def instance(
    server: Server,
    indexer: Indexer,
    client: CommandClient,
    lspServer: plasmon.jsonrpc.JsonrpcServer,
    pool: ServerCommandThreadPools
  ): ServerCommandInstance[ModuleLoadOptions] =
    ModuleLoad(server, indexer, client, pool)
  override def names = List(
    List("module", "load"),
    List("module-load")
  )
}
