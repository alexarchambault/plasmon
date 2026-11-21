package plasmon.servercommand

import plasmon.{Logger, Server}
import plasmon.PlasmonEnrichments.{StringThingExtensions, XtensionOsPath}
import plasmon.bsp.{BuildServerInfo, BuildTool}
import plasmon.command.ServerCommandThreadPools
import plasmon.index.{Indexer, IndexerActor}

import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.jdk.CollectionConverters.*

/** Bringing a file to the point where a language feature can answer for it.
  *
  * This is the server side of what `--auto` does on the `lsp …` commands. In an editor, a build
  * tool and a module are loaded long before anyone hovers - the extension asks for them, or the
  * user picks them from a menu. From a terminal there is nobody to have done that, so a first
  * `plasmon lsp hover` on a fresh workspace has nothing to answer with. `--auto` runs what the user
  * would otherwise have had to type first: [[BuildToolLoad]], then [[ModuleLoad]] - which compiles
  * the module it loads, see [[CompileAfterLoad]].
  *
  * The step before those two - having a server at all - is taken client-side, before this is ever
  * reached, by [[plasmon.command.AutoServer]].
  *
  * Editors in automatic mode (see [[plasmon.AutoMode]]) go through [[load]] too, for the files
  * users focus, rather than having them pick a build tool and a module from a menu.
  *
  * Nothing is done that isn't needed: a file already covered by a loaded build tool, or already
  * part of a loaded module, is left alone.
  */
object AutoLoad {

  /** The two kinds of line a load has to say, and where each of them goes.
    *
    * `--quiet` is the reason they are told apart: what is being loaded is worth silencing, what
    * went wrong on the way to an exit code isn't.
    *
    * `step` gets a short line each time a step that takes a while starts - what an editor shows in
    * its status while this runs.
    */
  final case class Out(
    progress: String => Unit,
    error: String => Unit,
    step: String => Unit = _ => ()
  )

  /** Why [[load]] stopped short of loading a module for a file. */
  sealed abstract class Failure(val message: String) extends Product with Serializable

  object Failure {

    /** No build tool in the workspace that the file could belong to. */
    final case class NoBuildTool(file: os.Path, workspace: os.Path)
        extends Failure(s"No build tool found in $workspace for $file")

    /** The loaded build tools have no module the file belongs to. */
    final case class NoModule(file: os.Path) extends Failure(s"No module found for $file")

    /** A build tool was found, and failed to load - or was skipped, having failed before. */
    final case class BuildToolLoad(buildTool: BuildTool, error: String)
        extends Failure(s"Error loading build tool ${buildTool.id}: $error")

    /** A module was found, and failed to load - or was skipped, having failed before. */
    final case class ModuleLoad(moduleUri: String, label: String, error: String)
        extends Failure(s"Error loading module $label: $error")

    /** The BSP data a build tool left in the cache couldn't stand in for it (see
      * `--stop-build-tool-after-load`).
      */
    final case class ReplayLoad(label: String, error: String)
        extends Failure(s"Error loading the cached build data of $label: $error")
  }

  /** Loads a build tool and a module for `file` if it has none. Returns once indexing has run, and
    * the module just loaded is compiled.
    *
    * `log` is where this goes - stderr for the commands here, so that whatever the request itself
    * prints on stdout stays the only thing on it. `quiet` keeps everything but the errors off it:
    * neither what is being loaded, nor what the build tool being started has to say for itself.
    */
  def apply(
    server: Server,
    indexer: Indexer,
    pools: ServerCommandThreadPools,
    file: os.Path,
    log: String => Unit,
    quiet: Boolean
  ): Unit = {

    val out = Out(
      progress = line => if (!quiet) log(line),
      error = log
    )

    // Starting a build tool is most of the wait here, and one that says what it is up to
    // meanwhile - Mill compiling its own build - is worth passing on. The sink outlives this call
    // otherwise: it is teed into a logger that the connection, not the command, keeps
    val echo = Option.when(!quiet)(new Logger.Sink(log))

    val res =
      try load(server, indexer, pools, file, out, echo, compile = true)
      finally
        echo.foreach(_.detach())

    for (failure <- res.left) {
      out.error(failure.message)
      ServerCommandInstance.exit(1)
    }
  }

  /** Loads a build tool and a module for `file` if it has none, telling why it couldn't otherwise.
    *
    * `compile` is whether to build the module just loaded before returning (see
    * [[CompileAfterLoad]]) - what the command line needs, and an editor doesn't, as it compiles the
    * files it has open once indexing is done.
    *
    * `skipBuildTool` and `skipModule` say which build tools and modules not to try loading, failing
    * as if they had. Automatic mode uses them not to go through the same failure again and again,
    * each time a file of a build tool that cannot start is focused.
    */
  def load(
    server: Server,
    indexer: Indexer,
    pools: ServerCommandThreadPools,
    file: os.Path,
    out: Out,
    echo: Option[String => Unit],
    compile: Boolean,
    skipBuildTool: BuildTool => Option[String] = _ => None,
    skipModule: String => Option[String] = _ => None
  ): Either[Failure, Unit] = {
    awaitPersistedImport(server, out)
    if (file.isDependencySource(server.workingDir)) {
      // Looked at from the modules that depend on it (see BspData.inferBuildTargets): there's
      // nothing to load for it. Build tools may offer one (Mill makes a script module of any
      // stray Scala file), which only gets in the way.
      out.progress(s"$file is a dependency source, nothing to load for it")
      Right(())
    }
    else
      for {
        _ <- ensureBuildTool(server, pools, file, out, echo, skipBuildTool)
        _ <- ensureModule(
          server,
          indexer,
          pools,
          file,
          out,
          echo,
          compile,
          skipBuildTool,
          skipModule
        )
        _ <-
          if (server.stopBuildToolAfterLoad)
            replaceBuildToolByReplay(server, indexer, pools, file, out)
          else
            Right(())
      } yield ()
  }

  /** Waits for whatever the server was already restoring when it started.
    *
    * A server imports what it persisted - build servers, then modules - in the background, and
    * answers commands while that is in flight. With [[plasmon.command.AutoServer]] starting one,
    * `--auto` is the very first thing a server ever sees, so without this it finds no build tool
    * for the file while the one it had is being restored, and then fails loading a second one over
    * it.
    */
  private def awaitPersistedImport(server: Server, out: Out): Unit = {
    val f = server.persistedImport
    if (!f.isCompleted) {
      out.step("Restoring loaded build tools and modules")
      out.progress("Waiting for the server to finish restoring what it had loaded before")
    }
    Await.ready(f, Duration.Inf)
    // Not fatal: what follows loads what's needed for the file anyway
    for (case scala.util.Failure(ex) <- f.value)
      out.error(s"Warning: error restoring what the server had loaded before: $ex")
  }

  /** Whether a loaded build tool covers `file`. */
  def hasBuildTool(server: Server, file: os.Path): Boolean =
    server.bspServers.list
      .iterator
      .flatMap(_._2)
      .exists(conn => file.startsWith(conn.info.workspace))

  /** Whether a loaded module holds `file` - the check [[ProjectOps.listModules]] itself makes. */
  def hasModule(server: Server, file: os.Path): Boolean =
    server.bspData.allTargetData.exists(_.sourceBuildTargets(file).exists(_.nonEmpty))

  private def ensureBuildTool(
    server: Server,
    pools: ServerCommandThreadPools,
    file: os.Path,
    out: Out,
    echo: Option[String => Unit],
    skip: BuildTool => Option[String]
  ): Either[Failure, Unit] =
    if (hasBuildTool(server, file)) {
      out.progress(s"Build tool already loaded for $file")
      Right(())
    }
    else
      ProjectOps.discoverBuildTools(server, Some(file)) match {
        case Seq() =>
          Left(Failure.NoBuildTool(file, server.workspace()))
        case tool +: _ =>
          // Discovery hands back the tools in the order the extension offers them, the one we'd
          // default to first. Not asking is the whole point of --auto, so take that one rather
          // than bailing out on ambiguity the way `build-tool load` does.
          skip(tool.buildTool) match {
            case Some(error) =>
              Left(Failure.BuildToolLoad(tool.buildTool, error))
            case None =>
              out.step(s"Loading build tool ${tool.buildTool.id}")
              out.progress(s"Loading build tool ${tool.buildTool.id}")
              Await.result(
                ProjectOps.loadBuildTool(
                  server,
                  pools,
                  tool.discoverId,
                  tool.buildTool.id,
                  Some(file),
                  echo
                ),
                Duration.Inf
              ) match {
                case Left(err) =>
                  Left(Failure.BuildToolLoad(tool.buildTool, err))
                case Right(()) =>
                  out.progress(s"Loaded build tool ${tool.buildTool.id}")
                  Right(())
              }
          }
      }

  private def ensureModule(
    server: Server,
    indexer: Indexer,
    pools: ServerCommandThreadPools,
    file: os.Path,
    out: Out,
    echo: Option[String => Unit],
    compile: Boolean,
    skipBuildTool: BuildTool => Option[String],
    skip: String => Option[String]
  ): Either[Failure, Unit] =
    if (hasModule(server, file)) {
      out.progress(s"Module already loaded for $file")
      Right(())
    }
    else
      ProjectOps.listModules(file, server).filter(!_.alreadyLoaded) match {
        case Seq() =>
          val restored =
            if (server.stopBuildToolAfterLoad)
              restoreBuildTool(server, indexer, pools, file, out, echo, skipBuildTool)
            else
              None
          restored match {
            case Some(res) =>
              // The build tool is back, it knows about all the modules
              res.flatMap { _ =>
                ensureModule(
                  server,
                  indexer,
                  pools,
                  file,
                  out,
                  echo,
                  compile,
                  skipBuildTool,
                  skip
                )
              }
            case None =>
              Left(Failure.NoModule(file))
          }
        case modules =>
          // listModules sorts the candidates best first - the recommended one, which is also the
          // one `module load` takes
          val module = modules.head
          skip(module.uri) match {
            case Some(error) =>
              Left(Failure.ModuleLoad(module.uri, module.label, error))
            case None =>
              out.step(s"Loading module ${module.label}")
              out.progress(s"Loading module ${module.uri}")
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
                  Left(Failure.ModuleLoad(module.uri, module.label, err))
                case Right(_) =>
                  out.progress(s"Loaded module ${module.uri}")
                  // Loading waits for the re-index it asks for, but the indexer goes on chewing
                  // through messages of its own afterwards - workspace source symbols, notably -
                  // and a request asked too early sees a half-built index. Same wait as
                  // `index --await`.
                  while (indexer.actor.awaitingMessages.nonEmpty)
                    Thread.sleep(100L)
                  // What `module load` does too: nothing else builds a module from the command
                  // line, and a request answered from an unbuilt one is quietly wrong (see
                  // CompileAfterLoad). Except with --sourcepath, where presentation compilers
                  // read modules from their sources, and nothing needs to be built.
                  if (compile && server.sourcepath.isEmpty)
                    CompileAfterLoad(server, module.uri, out.progress, out.error)
                  Right(())
              }
          }
      }

  /** Brings back the build tool a replay of `file` stands for (see [[replaceBuildToolByReplay]]).
    *
    * A replay knows the sources of the modules that were loaded when it was recorded only. For a
    * file of another module, the build tool needs to run again. The modules loaded from the replay
    * are loaded from it then. Returns how loading the build tool went if there was such a replay.
    */
  private def restoreBuildTool(
    server: Server,
    indexer: Indexer,
    pools: ServerCommandThreadPools,
    file: os.Path,
    out: Out,
    echo: Option[String => Unit],
    skipBuildTool: BuildTool => Option[String]
  ): Option[Either[Failure, Unit]] =
    server.bspServers.list.iterator.flatMap(_._2)
      .find { conn =>
        file.startsWith(conn.info.workspace) && conn.info.isInstanceOf[BuildServerInfo.Replay]
      }
      .map { conn =>
        val info    = conn.info
        val modules = indexer.targets.getOrElse(info, Nil)
        out.progress(s"No module for $file in the cached build data, starting the build tool again")
        indexer.dontReloadBuildTool(info)
        Await.result(server.bspServers.remove(info), Duration.Inf)
        server.bspServers.persist()
        ensureBuildTool(server, pools, file, out, echo, skipBuildTool).map { _ =>
          for {
            live <- server.bspServers.list.iterator.flatMap(_._2)
              .find(conn0 => file.startsWith(conn0.info.workspace))
            if modules.nonEmpty
          } {
            indexer.addTargets(live.info, modules)
            indexer.persist()
          }
        }
      }

  /** Stops the build tool that loaded the module of `file`, and has a replay of the BSP data it
    * left in the cache take its place (`plasmon server --stop-build-tool-after-load`).
    *
    * With `--sourcepath`, presentation compilers don't need anything built, so once a build tool
    * told us about the modules, it can go away. The modules it loaded are loaded again from the
    * replay, which answers like it did, from what it said when it was indexed.
    */
  private def replaceBuildToolByReplay(
    server: Server,
    indexer: Indexer,
    pools: ServerCommandThreadPools,
    file: os.Path,
    out: Out
  ): Either[Failure, Unit] =
    server.bspServers.list.iterator.flatMap(_._2)
      .find(conn => file.startsWith(conn.info.workspace))
      .filter(conn => !conn.info.isInstanceOf[BuildServerInfo.Replay]) match {
      case None => Right(())
      case Some(conn) =>
        val info = conn.info
        val dataDirOpt = IndexerActor
          .bspDataCacheDir(IndexerActor.bspDataCache(server.workingDir), server.workingDir, info)
          .filter { dir =>
            os.isFile(dir / "initializeBuildResult.json") &&
            os.isFile(dir / "workspaceBuildTargets.json")
          }
        dataDirOpt match {
          case None =>
            out.progress(s"No cached build data for ${info.label}, leaving it running")
            Right(())
          case Some(dataDir) =>
            out.progress(
              s"Stopping build tool ${info.label}, " +
                s"using the build data it left in ${dataDir.relativeTo(server.workingDir)}"
            )
            val modules = indexer.targets.getOrElse(info, Nil)
            indexer.dontReloadBuildTool(info)
            Await.result(server.bspServers.remove(info), Duration.Inf)
            val replay = BuildTool.Replay(info.workspace, dataDir)
            Await.result(
              server.bspServers.tryAdd(
                replay,
                replay.launcher(server.tools) +: replay.extraLaunchers,
                line => scribe.info("BSP: " + line),
                pools.bspEces,
                () => pools.bloopThreads
              ),
              Duration.Inf
            ) match {
              case Left(err) =>
                Left(Failure.ReplayLoad(info.label, err))
              case Right(()) =>
                server.bspServers.persist()
                val replayInfo = BuildServerInfo.Replay(info.workspace, dataDir)
                // Modules the build tool didn't report itself (for dependency sources, say) aren't
                // in the replay: they're loaded again when needed
                val known = server.bspServers.get(replayInfo).toSeq
                  .flatMap(_.conn.workspaceBuildTargets.get().getTargets.asScala.map(_.getId))
                  .toSet
                indexer.addTargets(replayInfo, modules.filter(known))
                indexer.persist()
                Await.result(indexer.reIndex(), Duration.Inf)
                while (indexer.actor.awaitingMessages.nonEmpty)
                  Thread.sleep(100L)
                out.progress(s"Stopped build tool ${info.label}")
                Right(())
            }
        }
    }
}
