package plasmon.ide

import java.util.concurrent.LinkedBlockingQueue
import java.util.concurrent.TimeUnit

import plasmon.PlasmonEnrichments.XtensionStringDoc
import scala.concurrent.Promise
import scala.util.Try
import scala.concurrent.Future
import plasmon.bsp.PlasmonBuildClientImpl
import scala.meta.Dialect
import scala.meta.internal.mtags.GlobalSymbolIndex

class ParserQueue(
  buffers: Buffers,
  trees: Trees
) extends AutoCloseable {

  import ParserQueue.*

  private val queue = new LinkedBlockingQueue[Element]
  private val parserThread: Thread =
    new Thread("parser") {
      setDaemon(true)
      override def run(): Unit =
        try
          while (true)
            for (elem <- Option(queue.poll(1L, TimeUnit.SECONDS)))
              elem.promise.complete {
                Try {
                  if (buffers.contains(elem.path))
                    elem.buildClient.onSyntaxError(
                      elem.module,
                      elem.path,
                      trees.didChange(elem.dialect, elem.path)
                    )
                }
              }
        catch {
          case _: InterruptedException =>
          // normal exit
          case t: Throwable =>
            scribe.error("Exception caught in parser thread", t)
        }
        finally
          scribe.info("Parser thread exiting")
    }

  parserThread.start()

  def check(
    module: GlobalSymbolIndex.Module,
    path: os.Path,
    buildClient: PlasmonBuildClientImpl,
    dialect: Dialect
  ): Future[Unit] =
    if (path.last.isScalaFilename) {
      val promise = Promise[Unit]()
      queue.offer(Element(module, path, promise, buildClient, dialect))
      promise.future
    }
    else
      Future.successful(())

  def close(): Unit = {
    parserThread.interrupt()
  }
}

object ParserQueue {
  private final case class Element(
    module: GlobalSymbolIndex.Module,
    path: os.Path,
    promise: Promise[Unit],
    buildClient: PlasmonBuildClientImpl,
    dialect: Dialect
  )
}
