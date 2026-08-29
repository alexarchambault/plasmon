package plasmon.servercommand

import plasmon.Server
import plasmon.PlasmonEnrichments.StringThingExtensions
import plasmon.command.ServerCommandThreadPools
import plasmon.index.Indexer

import scala.concurrent.Await
import scala.concurrent.duration.Duration

/** Bringing a file to the point where a language feature can answer for it.
  *
  * This is what `--auto` does on the `lsp …` commands. In an editor, a build tool and a module are
  * loaded long before anyone hovers - the extension asks for them, or the user picks them from a
  * menu. From a terminal there is nobody to have done that, so a first `plasmon lsp hover` on a
  * fresh workspace has nothing to answer with. `--auto` runs what the user would otherwise have had
  * to type first: [[BuildToolLoad]], then [[ModuleLoad]].
  *
  * Nothing is done that isn't needed: a file already covered by a loaded build tool, or already
  * part of a loaded module, is left alone.
  */
object AutoLoad {

  /** Loads a build tool and a module for `file` if it has none. Returns once indexing has run.
    *
    * `log` is where the progress goes - stderr for the commands here, so that whatever the request
    * itself prints on stdout stays the only thing on it.
    */
  def apply(
    server: Server,
    indexer: Indexer,
    pools: ServerCommandThreadPools,
    file: os.Path,
    log: String => Unit
  ): Unit = {
    ensureBuildTool(server, pools, file, log)
    ensureModule(server, indexer, pools, file, log)
  }

  /** Whether a loaded build tool covers `file`. */
  private def hasBuildTool(server: Server, file: os.Path): Boolean =
    server.bspServers.list
      .iterator
      .flatMap(_._2)
      .exists(conn => file.startsWith(conn.info.workspace))

  /** Whether a loaded module holds `file` - the check [[ProjectOps.listModules]] itself makes. */
  private def hasModule(server: Server, file: os.Path): Boolean =
    server.bspData.allTargetData.exists(_.sourceBuildTargets(file).exists(_.nonEmpty))

  private def ensureBuildTool(
    server: Server,
    pools: ServerCommandThreadPools,
    file: os.Path,
    log: String => Unit
  ): Unit =
    if (hasBuildTool(server, file))
      log(s"Build tool already loaded for $file")
    else
      ProjectOps.discoverBuildTools(server, Some(file)) match {
        case Seq() =>
          log(s"No build tool found in ${server.workspace()} for $file")
          ServerCommandInstance.exit(1)
        case tool +: _ =>
          // Discovery hands back the tools in the order the extension offers them, the one we'd
          // default to first. Not asking is the whole point of --auto, so take that one rather
          // than bailing out on ambiguity the way `build-tool load` does.
          log(s"Loading build tool ${tool.buildTool.id}")
          Await.result(
            ProjectOps.loadBuildTool(
              server,
              pools,
              tool.discoverId,
              tool.buildTool.id,
              Some(file)
            ),
            Duration.Inf
          ) match {
            case Left(err) =>
              log(s"Error loading build tool ${tool.buildTool.id}: $err")
              ServerCommandInstance.exit(1)
            case Right(()) =>
              log(s"Loaded build tool ${tool.buildTool.id}")
          }
      }

  private def ensureModule(
    server: Server,
    indexer: Indexer,
    pools: ServerCommandThreadPools,
    file: os.Path,
    log: String => Unit
  ): Unit =
    if (hasModule(server, file))
      log(s"Module already loaded for $file")
    else
      ProjectOps.listModules(file, server) match {
        case Seq() =>
          log(s"No module found for $file")
          ServerCommandInstance.exit(1)
        case modules =>
          // listModules sorts the candidates best first - the recommended one, which is also the
          // one `module load` takes
          val module = modules.head
          log(s"Loading module ${module.uri}")
          Await.result(
            ProjectOps.loadModule(
              server,
              indexer,
              pools,
              module.workspace.osPathFromUri,
              module.server,
              module.uri
            ),
            Duration.Inf
          ) match {
            case Left(err) =>
              log(s"Error loading module ${module.label}: $err")
              ServerCommandInstance.exit(1)
            case Right(_) =>
              log(s"Loaded module ${module.uri}")
          }
          // Loading waits for the re-index it asks for, but the indexer goes on chewing through
          // messages of its own afterwards - workspace source symbols, notably - and a request
          // asked too early sees a half-built index. Same wait as `index --await`.
          while (indexer.actor.awaitingMessages.nonEmpty)
            Thread.sleep(100L)
      }
}
