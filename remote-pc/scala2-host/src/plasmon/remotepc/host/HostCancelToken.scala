package plasmon.remotepc.host

import java.util.concurrent.{CancellationException, CompletableFuture, CompletionStage}

import scala.meta.pc.CancelToken

/** A [[CancelToken]] cancelled along with `future`, the future we answer a request with.
  *
  * lsp4j cancels that future when the server sends `$/cancelRequest` for its request.
  */
final class HostCancelToken(future: CompletableFuture[_]) extends CancelToken {
  private val cancelled = new CompletableFuture[java.lang.Boolean]
  future.whenComplete { (_, _) =>
    cancelled.complete(future.isCancelled)
  }

  def onCancel(): CompletionStage[java.lang.Boolean] = cancelled
  def checkCanceled(): Unit =
    if (future.isCancelled)
      throw new CancellationException
}
