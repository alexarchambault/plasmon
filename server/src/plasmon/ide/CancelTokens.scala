// Originally based on https://github.com/scalameta/metals/blob/2a6f8a437a1ce7c44140673edfe34bb74dfd33be/metals/src/main/scala/scala/meta/internal/metals/CancelTokens.scala

package plasmon.ide

import java.util.concurrent.CompletableFuture

import scala.concurrent.{ExecutionContextExecutorService, Future, Promise}
import scala.meta.pc.CancelToken
import scala.util.{Failure, Success}

/** Constructs an async `CancelToken`.
  *
  * The `CancelChecker` API in lsp4j is un-sufficient for our needs because we want to get a
  * `Future[Boolean]` that completes to `true` when the user cancels a request, allowing us to abort
  * expensive computation like typechecking as soon as possible. With `CancelChecker`, we need to
  * explicitly call `token.checkCancelled()`, which is not possible inside the compiler.
  */
object CancelTokens {
  def apply[T](
    fn: CancelToken => T
  )(implicit ec: ExecutionContextExecutorService): CompletableFuture[T] =
    future[T](token => Future(fn(token)))
  def future[T](
    fn: CancelToken => Future[T]
  )(implicit ec: ExecutionContextExecutorService): CompletableFuture[T] = {
    val token = Promise[Unit]()
    val result = new CompletableFuture[T]() {
      override def cancel(mayInterruptIfRunning: Boolean): Boolean = {
        token.trySuccess(())
        super.cancel(mayInterruptIfRunning)
      }
    }
    fn(FutureCancelToken.fromUnit(token.future)).onComplete {
      case Failure(exception) =>
        result.completeExceptionally(exception)
      case Success(value) =>
        result.complete(value)
    }
    // NOTE(olafur): we cannot use `Future.asJava` or `CompletableFuture.handleAsync`
    // since those methods return a `CompletableFuture` that doesn't contain the
    // custom `override def cancel()` above.
    result
  }
}
