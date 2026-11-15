package plasmon.ide

import plasmon.PlasmonEnrichments.XtensionCancelChecker

import java.util.concurrent.{CancellationException, Executors, TimeUnit}

import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.meta.pc.CancelToken

class CancelTokensTests extends munit.FunSuite {

  private val pool = ExecutionContext.fromExecutorService(Executors.newCachedThreadPool())
  override def afterAll(): Unit =
    pool.shutdown()

  private def onCancel(token: CancelToken): Boolean =
    token.onCancel().toCompletableFuture.get(10L, TimeUnit.SECONDS)

  test("cancelling the request cancels its token") {
    val tokenPromise = Promise[CancelToken]()
    val never        = Promise[String]()
    val f = CancelTokens.future { token =>
      tokenPromise.success(token)
      never.future
    }(using pool)
    val token = tokenPromise.future.value.get.get
    assert(!token.isCancelled)

    assert(f.cancel(true))

    // right away, not once some callback ran
    assert(token.isCancelled)
    // not intercept, that lets InterruptedException-s through
    val checkThrew =
      try {
        token.checkCanceled()
        false
      }
      catch {
        case _: InterruptedException => true
      }
    assert(checkThrew, "checkCanceled didn't throw")
    assert(onCancel(token))
    intercept[CancellationException](f.join())
  }

  test("the token of a request done without being cancelled says it won't get cancelled") {
    val tokenPromise = Promise[CancelToken]()
    val f = CancelTokens.future { token =>
      tokenPromise.success(token)
      Future.successful("done")
    }(using pool)
    assertEquals(f.get(10L, TimeUnit.SECONDS), "done")
    val token = tokenPromise.future.value.get.get
    assert(!onCancel(token))
    assert(!token.isCancelled)
    token.checkCanceled()
  }

  test("a failing request completes its future exceptionally") {
    val ex     = new Exception("nope")
    val f      = CancelTokens.future(_ => Future.failed[String](ex))(using pool)
    val caught = intercept[java.util.concurrent.ExecutionException](f.get(10L, TimeUnit.SECONDS))
    assertEquals(caught.getCause, ex)
  }
}
