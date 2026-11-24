package plasmon.handlers.docchange

import org.eclipse.lsp4j as l
import plasmon.Server
import plasmon.bsp.Diagnostics
import plasmon.ide.FutureCancelToken

import java.util.concurrent.{ConcurrentHashMap, Executors, ScheduledFuture, TimeUnit}

import scala.concurrent.{ExecutionContext, Promise}
import scala.util.{Failure, Success}

/** Has presentation compilers type check the files whose build client reports their diagnostics
  *
  * Presentation compilers otherwise only compute diagnostics as a side effect of the requests they
  * get (hover, completion, …), on the files of these requests. Clients that get no diagnostics from
  * compilations (like notebooks, whose code is only ever type checked by presentation compilers)
  * want them as their files change, and get them this way, via the build client of their files.
  */
object PcDiagnostics {

  /** How long to wait after a change before type checking, so that a burst of changes (typing) only
    * gets type checked once
    */
  private def delayMs = 200L

  private lazy val scheduler = Executors.newSingleThreadScheduledExecutor { r =>
    val t = new Thread(r, "plasmon-pc-diagnostics")
    t.setDaemon(true)
    t
  }

  private val scheduled = new ConcurrentHashMap[os.Path, ScheduledFuture[?]]

  /** Type checks `path` with its presentation compiler a bit later, if the build client of its
    * module reports presentation compiler diagnostics
    */
  def schedule(server: Server, path: os.Path): Unit =
    for {
      targetId <- server.bspData.inverseSources(path)
      client   <- server.bspData.buildClientOf(targetId)
      if client.diagnosticTypes.contains(Diagnostics.Type.PresentationCompiler)
    } {
      lazy val task: ScheduledFuture[?] = scheduler.schedule(
        (() => {
          scheduled.remove(path, task)
          typeCheck(server, path, targetId)
        }): Runnable,
        delayMs,
        TimeUnit.MILLISECONDS
      )
      Option(scheduled.put(path, task)).foreach(_.cancel(false))
    }

  private def typeCheck(
    server: Server,
    path: os.Path,
    targetId: ch.epfl.scala.bsp4j.BuildTargetIdentifier
  ): Unit = {
    given ExecutionContext = server.pools.documentChangeEc
    val token              = FutureCancelToken(Promise[Boolean]().future)
    // the diagnostics get reported by the presentation compiler itself, see the emitDiagnostics
    // passed to the presentation compilers
    val resOpt = server.presentationCompilers.compile(
      new l.TextDocumentIdentifier(path.toNIO.toUri.toASCIIString),
      targetId,
      token
    )
    for (res <- resOpt)
      res.onComplete {
        case Success(_)  =>
        case Failure(ex) => scribe.warn(s"Error type checking $path for diagnostics", ex)
      }
  }
}
