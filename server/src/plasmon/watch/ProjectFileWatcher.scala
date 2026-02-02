// Originally based on https://github.com/scalameta/metals/blob/0ad0bc184f82dbd178d01f76913ea6bdfa98db14/metals/src/main/scala/scala/meta/internal/metals/watcher/ProjectFileWatcher.scala

package plasmon.watch

import java.util.concurrent.BlockingQueue
import java.util.concurrent.LinkedBlockingQueue
import java.util.concurrent.TimeoutException
import java.util.concurrent.atomic.AtomicBoolean
import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent.locks.ReentrantLock

import scala.annotation.tailrec
import scala.collection.mutable
import scala.concurrent.Await
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.concurrent.duration._
import scala.util.Failure
import scala.util.Try

import plasmon.ide.Directories

import com.swoval.files.FileTreeViews.Observer
import com.swoval.files.PathWatcher
import com.swoval.files.PathWatchers
import com.swoval.files.PathWatchers.Event.Kind
import plasmon.index.BspData
import scala.util.Success
import scala.concurrent.Promise
import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker

/** Watch selected files and execute a callback on file events.
  *
  * This class recursively watches selected directories and selected files. File events can be
  * further filtered by the `watchFiler` parameter, which can speed by watching for changes by
  * limiting the number of files that need to be hashed.
  *
  * We don't use the LSP dynamic file watcher capability because
  *
  *   1. the glob syntax is not defined in the LSP spec making it difficult to deliver a consistent
  *      file watching experience with all editor clients on all operating systems.
  *   2. we may have a lot of file watching events and it's presumably less overhead to get the
  *      notifications directly from the OS instead of through the editor via LSP.
  */
final class ProjectFileWatcher(
  workspaceDeferred: () => os.Path,
  bspData: BspData,
  watchFilter: os.Path => Boolean,
  preprocessWatchEvent: PartialFunction[WatchEvent, Unit],
  onFileWatchEvent: PartialFunction[WatchEvent, Unit]
)(implicit ec: ExecutionContext)
    extends FileWatcher
    with AutoCloseable {
  import ProjectFileWatcher._

  private var proceedPromise = {
    val p = Promise[Unit]()
    // Start with promise in completed state, so that file watching is enabled initially
    p.success(())
    p
  }
  def suspend(): Unit = {
    if (proceedPromise.isCompleted)
      proceedPromise = Promise()
  }
  def resume(): Unit = {
    if (!proceedPromise.isCompleted)
      proceedPromise.trySuccess(())
  }

  def enqueue(event: WatchEvent): Unit =
    watchEventQueue.add(Right(event))

  @volatile
  private var stopWatcher = Option.empty[() => Unit]

  override def close(): Unit =
    stopWatcher.foreach(_())

  private val watchEventQueue: BlockingQueue[Either[() => Unit, WatchEvent]] =
    new LinkedBlockingQueue

  def start(): Unit =
    // FIXME stopWatcher access not thread-safe
    if (stopWatcher.isEmpty)
      stopWatcher = Some(
        startWatch(
          workspaceDeferred(),
          collectPathsToWatch(bspData),
          preprocessWatchEvent,
          onFileWatchEvent,
          watchFilter,
          () => proceedPromise.future,
          watchEventQueue
        )
      )

  def asJson: ProjectFileWatcher.AsJson =
    ProjectFileWatcher.AsJson()
}

object ProjectFileWatcher {
  private case class PathsToWatch(
    files: Set[os.Path],
    directories: Set[os.Path]
  )

  private def collectPathsToWatch(bspData: BspData): PathsToWatch = {
    val directories = mutable.Set.empty[os.Path]
    val files       = mutable.Set.empty[os.Path]

    def collect(path: os.Path): Unit = {
      val shouldBeWatched =
        !bspData.isInsideSourceRoot(path) &&
        !bspData.checkIfGeneratedSource(path)

      if (shouldBeWatched)
        if (bspData.isSourceFile(path))
          files.add(path)
        else
          directories.add(path)
    }

    // Watch the source directories for "goto definition" index.
    bspData.sourceRoots.foreach(collect)
    bspData.sourceItems.foreach(collect)

    bspData.allTargetRoots
      .map(_ / Directories.semanticdb)
      .foreach(directories.add)

    PathsToWatch(
      files.toSet,
      directories.toSet
    )
  }

  /** Start file watching
    *
    * Contains platform specific file watch initialization logic
    *
    * @param workspace
    *   current project workspace directory
    * @param pathsToWatch
    *   source files and directories to watch
    * @param processWatchEvent
    *   to execute on FileWatchEvent
    * @param watchFilter
    *   predicate that filters which files generate a FileWatchEvent on create/delete/change
    * @return
    *   a dispose action resources used by file watching
    */
  private def startWatch(
    workspace: os.Path,
    pathsToWatch: PathsToWatch,
    preprocessWatchEvent: PartialFunction[WatchEvent, Unit],
    processWatchEvent: PartialFunction[WatchEvent, Unit],
    watchFilter: os.Path => Boolean,
    proceedFuture: () => Future[Unit],
    watchEventQueue: BlockingQueue[Either[() => Unit, WatchEvent]]
  )(implicit ec: ExecutionContext): () => Unit = {
    val watcher: PathWatcher[PathWatchers.Event] =
      if (scala.util.Properties.isMac) {
        // Due to a hard limit on the number of FSEvents streams that can be
        // opened on macOS, only up to 32 longest common prefixes of the files to
        // watch are registered for a recursive watch.
        // However, the events are then filtered to receive only relevant events

        val trie = PathTrie(
          pathsToWatch.files ++ pathsToWatch.directories,
          workspace
        )
        val isWatched = trie.containsPrefixOf

        // Select up to `maxRoots` longest prefixes of all files in the trie for
        // watching. Watching the root of the workspace may have bad performance
        // implications if it contains many other projects that we don't need to
        // watch (eg. in a monorepo)
        val watchRoots =
          trie.longestPrefixes(os.Path(workspace.toNIO.getRoot), macOsMaxWatchRoots)

        val watcher = initWatcher(
          path => watchFilter(path) && isWatched(path),
          watchEventQueue
        )
        watchRoots.foreach { root =>
          scribe.debug(s"Registering root for file watching: $root")
          watcher.register(root.toNIO, Int.MaxValue)
        }

        watcher
      }
      else {
        // Other OSes register all the files and directories individually
        val watcher = initWatcher(watchFilter, watchEventQueue)

        for (p <- pathsToWatch.directories)
          watcher.register(p.toNIO, Int.MaxValue)
        for (p <- pathsToWatch.files)
          watcher.register(p.toNIO, -1)

        watcher
      }

    val stopWatchingSignal: AtomicBoolean = new AtomicBoolean

    val thread = new Thread("plasmon-watch-callback-thread") {
      @tailrec def loop(): Unit = {
        try {
          val firstEvent = watchEventQueue.take
          Await.result(proceedFuture(), Duration.Inf)
          val events = firstEvent ::
            Iterator.continually(watchEventQueue.poll())
              .takeWhile(_ != null)
              .toList
          events.foreach {
            case Left(_) =>
            case Right(ev) =>
              preprocessWatchEvent.lift(ev)
          }
          // WatchEvent.normalize(events)
          events.foreach {
            case Left(f) => f()
            case Right(ev) =>
              processWatchEvent.lift(ev)
          }
        }
        catch { case _: InterruptedException => }
        if (!stopWatchingSignal.get) loop()
      }
      override def run(): Unit = loop()
      start()
    }

    () => {
      val hanged: AtomicReference[Thread] = new AtomicReference(null)
      val closing = Future {
        hanged.set(Thread.currentThread())
        stopWatchingSignal.set(true)
        watcher.close()
        thread.interrupt()
        hanged.set(null)
      }
      closing.onComplete {
        case Success(_) =>
        case Failure(ex) =>
          scribe.warn("Failed to close watcher", ex)
      }
      // watcher.close() might hang so we don't want to get stuck here
      Try(Await.ready(closing, 3.seconds)) match {
        case Failure(_: TimeoutException) =>
          Option(hanged.get()).foreach(_.interrupt())
        case _ =>
      }
    }
  }

  /** Initialize file watcher
    *
    * File watch events are put into a queue which is processed separately to prevent callbacks to
    * be executed on the thread that watches for file events.
    *
    * @param watchFilter
    *   for incoming file watch events
    * @param queue
    *   for file events
    * @return
    */
  private def initWatcher(
    watchFilter: os.Path => Boolean,
    queue: BlockingQueue[Either[() => Unit, WatchEvent]]
  ): PathWatcher[PathWatchers.Event] = {
    val watcher = PathWatchers.get( /*follow symlinks*/ true)

    watcher.addObserver(new Observer[PathWatchers.Event] {
      override def onError(t: Throwable): Unit = {
        scribe.error(s"Error encountered during file watching", t)
      }

      override def onNext(event: PathWatchers.Event): Unit = {
        val path = event.getTypedPath.getPath
        for (path <- Option(event.getTypedPath.getPath).map(os.Path(_)) if watchFilter(path))
          event.getKind match {
            // Swoval PathWatcher may not disambiguate between create and modify events on macOS
            // due to how underlying OS APIs work. However such fidelity is not needed for metals.
            // If the distinction between create and modify is important some time in the future,
            // you may wish to use FileTreeRepository, which needs much more memory because
            // it caches the whole watched file tree.
            case Kind.Create =>
              queue.add(Right(WatchEvent.CreateOrModify(path)))
            case Kind.Modify =>
              queue.add(Right(WatchEvent.CreateOrModify(path)))
            case Kind.Delete =>
              queue.add(Right(WatchEvent.Delete(path)))
            case Kind.Overflow =>
              queue.add(Right(WatchEvent.Overflow(path)))
            case Kind.Error =>
              scribe.error("File watcher encountered an unknown error")
          }
      }
    })

    watcher
  }

  private def macOsMaxWatchRoots: Int = 32

  final case class AsJson()

  given JsonValueCodec[AsJson] =
    JsonCodecMaker.make
}
