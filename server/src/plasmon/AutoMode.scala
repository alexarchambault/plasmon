package plasmon

import org.eclipse.lsp4j as l
import plasmon.PlasmonEnrichments.*
import plasmon.bsp.BuildTool
import plasmon.command.ServerCommandThreadPools
import plasmon.ide.Directories
import plasmon.index.Indexer
import plasmon.servercommand.AutoLoad

import java.util.concurrent.{ConcurrentHashMap, ExecutorService, Executors}

import scala.util.control.NonFatal

/** The mode the server is in, as far as editors are concerned: manual or automatic.
  *
  * In manual mode - the historical one - nothing gets loaded unless users ask for it: they pick a
  * build tool, then the modules they want, from the menus of their editor.
  *
  * In automatic mode, each file users focus gets a build tool and a module loaded for it if it has
  * none, the way `--auto` does it on the `lsp …` commands of the command line (see
  * [[plasmon.servercommand.AutoLoad]]): the first build tool discovered for the file, then the best
  * module the file belongs to. What was loaded stays loaded when switching back to manual mode.
  *
  * The mode is persisted under `.plasmon/`, along with the build tools and modules that are loaded,
  * so that a server started later in the same directory starts in the mode this one was left in.
  * `plasmon server --auto` (or `--auto=false`) overrides it.
  *
  * Files are handled one at a time, in the order they were focused, on a thread of their own: a
  * build tool takes a while to start, and focus notifications don't wait for answers anyway.
  *
  * A build tool or a module that fails to load isn't tried again until automatic mode is switched
  * on again - each file of a build tool that cannot start would otherwise try to start it again,
  * and say so again.
  */
final class AutoMode(
  server: Server,
  indexer: Indexer,
  pools: ServerCommandThreadPools,
  stateFile: os.Path
) {

  import AutoMode.*

  @volatile private var enabled0: Boolean = readState(stateFile).getOrElse(false)

  /** What is being loaded right now, for the status - `None` when idle. */
  @volatile private var loading0: Option[String] = None

  private val failedBuildTools = new ConcurrentHashMap[BuildTool, String]
  private val failedModules    = new ConcurrentHashMap[String, String]

  /** Files waiting for their turn, so that focusing one repeatedly doesn't queue it repeatedly. */
  private val queued = ConcurrentHashMap.newKeySet[os.Path]()

  /** Files we said nothing could be loaded for, so that we only say it once. */
  private val reportedNothingToLoad = ConcurrentHashMap.newKeySet[os.Path]()

  private lazy val executor: ExecutorService =
    Executors.newSingleThreadExecutor { runnable =>
      val t = new Thread(runnable, "plasmon-automatic-mode")
      t.setDaemon(true)
      t
    }

  lazy val logger: Logger = server.loggerManager.create("automatic-mode", "Automatic mode")

  def enabled: Boolean = enabled0

  def loading: Option[String] = loading0

  def modeName: String = if (enabled0) automatic else manual

  /** Switches automatic mode on or off, and persists that. Switching it on loads what the focused
    * file needs, if anything.
    */
  def set(enabled: Boolean): Unit = {
    val changed = enabled != enabled0
    enabled0 = enabled
    writeState(stateFile, enabled)
    if (changed)
      logger.log(s"Switched to $modeName mode")
    if (enabled) {
      // A fresh start for whatever failed before: switching automatic mode on is how users ask
      // for another try
      failedBuildTools.clear()
      failedModules.clear()
      reportedNothingToLoad.clear()
      for (path <- server.editorState.focusedDocument)
        focused(path)
    }
    server.refreshStatus()
  }

  /** Called when a file is focused in the editor. */
  def focused(path: os.Path): Unit =
    if (enabled0 && eligible(path) && !covered(path) && queued.add(path))
      executor.execute { () =>
        queued.remove(path)
        try process(path)
        catch {
          case NonFatal(ex) =>
            scribe.error(s"Error loading what $path needs in automatic mode", ex)
            logger.log(s"Error loading what $path needs: $ex")
        }
      }

  /** Whether a file is one we'd load a build tool and a module for: a Scala or Java file of the
    * workspace, rather than a dependency source Plasmon extracted, or a file elsewhere on disk.
    */
  private def eligible(path: os.Path): Boolean = {
    val workspaceOpt =
      try Some(server.workspace())
      catch { case NonFatal(_) => None }
    workspaceOpt.exists { workspace =>
      path.startsWith(workspace) &&
      !path.startsWith(workspace / ".plasmon") &&
      !path.startsWith(server.workingDir / ".plasmon") &&
      !path.startsWith(server.workingDir / Directories.state) &&
      path.isScalaOrJava
    }
  }

  private def covered(path: os.Path): Boolean =
    AutoLoad.hasBuildTool(server, path) && AutoLoad.hasModule(server, path)

  private def process(path: os.Path): Unit =
    // Checked again: things may have changed while this was waiting for its turn - automatic mode
    // switched off, or the file loaded along with the one before it
    if (enabled0 && !covered(path)) {
      val out = AutoLoad.Out(
        progress = logger.log(_),
        error = logger.log(_),
        step = { step =>
          loading0 = Some(step)
          server.refreshStatus()
        }
      )
      logger.log(s"Loading what $path needs")
      val res =
        try
          AutoLoad.load(
            server,
            indexer,
            pools,
            path,
            out,
            echo = None,
            // Opened files get compiled once indexing is done, see IndexerActor
            compile = false,
            skipBuildTool = tool => Option(failedBuildTools.get(tool)),
            skipModule = uri => Option(failedModules.get(uri))
          )
        finally {
          loading0 = None
          server.refreshStatus()
        }
      res match {
        case Right(()) =>
          logger.log(s"Loaded what $path needs")
        case Left(failure) =>
          failure match {
            case f: AutoLoad.Failure.BuildToolLoad =>
              val isNew = failedBuildTools.putIfAbsent(f.buildTool, f.error) == null
              report(failure.message, isNew)
            case f: AutoLoad.Failure.ModuleLoad =>
              val isNew = failedModules.putIfAbsent(f.moduleUri, f.error) == null
              report(failure.message, isNew)
            case _: AutoLoad.Failure.ReplayLoad =>
              // --stop-build-tool-after-load only: the build tool was stopped, and nothing stands
              // for it now. The next file focused starts it again.
              report(failure.message, isNew = true)
            case _: AutoLoad.Failure.NoBuildTool | _: AutoLoad.Failure.NoModule =>
              // Nothing went wrong as such - the file is one no build tool knows about, which the
              // status already says. Not worth more than a line in the log.
              if (reportedNothingToLoad.add(path))
                logger.log(failure.message)
          }
      }
    }

  /** Says what failed, in the log and - the first time around - to the user. */
  private def report(message: String, isNew: Boolean): Unit = {
    logger.log(message)
    if (isNew)
      server.languageClient.showMessage(
        new l.MessageParams(
          l.MessageType.Error,
          s"Automatic mode: $message (switch automatic mode on again to retry)"
        )
      )
  }
}

object AutoMode {

  val automatic = "automatic"
  val manual    = "manual"

  /** Reads `"automatic"` or `"manual"`. */
  def parse(mode: String): Option[Boolean] =
    mode.trim match {
      case `automatic` => Some(true)
      case `manual`    => Some(false)
      case _           => None
    }

  def stateFile(workingDir: os.Path): os.Path =
    workingDir / Directories.state / "mode.json"

  private def readState(file: os.Path): Option[Boolean] =
    if (os.isFile(file))
      try
        ujson.read(os.read(file)).objOpt
          .flatMap(_.get("mode"))
          .flatMap(_.strOpt)
          .flatMap(parse)
      catch {
        case NonFatal(ex) =>
          scribe.warn(s"Ignoring malformed $file", ex)
          None
      }
    else
      None

  private def writeState(file: os.Path, enabled: Boolean): Unit = {
    val mode = if (enabled) automatic else manual
    os.write.over(file, ujson.write(ujson.Obj("mode" -> mode)), createFolders = true)
  }
}
