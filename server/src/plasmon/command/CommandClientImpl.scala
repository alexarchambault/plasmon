package plasmon.command

import plasmon.protocol.{CommandClient, PrintData}

import java.util.concurrent.{CompletableFuture, LinkedBlockingQueue}

import scala.concurrent.Promise

class CommandClientImpl(queue: LinkedBlockingQueue[(String, Boolean, Promise[Unit])])
    extends CommandClient {
  def print(data: PrintData): CompletableFuture[Unit] = {
    val promise = Promise[Unit]()
    if (data.getLine != null) // just in case
      // queue is unbounded, so this shouldn't block
      queue.add((data.getLine, data.getIsStderr, promise))
    import plasmon.PlasmonEnrichments._
    promise.future.asJava
  }
}
