package plasmon.command

import plasmon.protocol.{CommandClient, PrintData}

import java.io.IOException
import java.util.concurrent.{CompletableFuture, ConcurrentHashMap}

import scala.util.control.NonFatal

/** The client at the other end of one command connection, as the commands run for it see it.
  *
  * lsp4j never completes a request whose peer went away, so a command printing to a `plasmon`
  * process that exited would wait forever, holding a command thread. This fails such requests
  * instead: those pending when the connection closes, and any sent after that.
  */
final class ConnectionCommandClient extends CommandClient {

  @volatile private var remote: CommandClient = null
  @volatile private var closed                = false
  private val pending = ConcurrentHashMap.newKeySet[CompletableFuture[Unit]]()

  /** Sets the proxy requests go through - the launcher providing it needs the local service, which
    * needs this, hence not a constructor parameter
    */
  def connect(remote: CommandClient): Unit =
    this.remote = remote

  def isClosed: Boolean = closed

  /** Fails pending and future requests to the client, which went away */
  def close(): Unit = {
    closed = true
    pending.forEach(_.completeExceptionally(new ConnectionCommandClient.ClientGoneException))
  }

  def print(data: PrintData): CompletableFuture[Unit] =
    if (closed) CompletableFuture.failedFuture(new ConnectionCommandClient.ClientGoneException)
    else {
      val remote0 = remote
      if (remote0 == null)
        throw new IllegalStateException("Command client not connected")
      val f =
        try remote0.print(data)
        catch {
          case NonFatal(e) =>
            CompletableFuture.failedFuture(new ConnectionCommandClient.ClientGoneException(e))
        }
      pending.add(f)
      f.whenComplete((_, _) => pending.remove(f))
      // close may have run between our check and the add above, and missed f
      if (closed)
        f.completeExceptionally(new ConnectionCommandClient.ClientGoneException)
      f
    }
}

object ConnectionCommandClient {
  final class ClientGoneException(cause: Throwable = null)
      extends IOException("Command client went away", cause)
}
