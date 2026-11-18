package plasmon.jsonrpc

import org.eclipse.lsp4j.jsonrpc.{MessageConsumer, RemoteEndpoint}
import org.eclipse.lsp4j.jsonrpc.messages.{
  CancelParams,
  Message,
  NotificationMessage,
  RequestMessage,
  ResponseErrorCode,
  ResponseMessage
}
import plasmon.TestLogger
import plasmon.ide.CancelTokens

import java.util.concurrent.{CompletableFuture, ConcurrentLinkedQueue, Executors, TimeUnit}

import scala.concurrent.{ExecutionContext, Promise}
import scala.jdk.CollectionConverters.*
import scala.meta.pc.CancelToken

class JsonrpcServerTests extends munit.FunSuite {

  private def withEndpoint[T](handlers: Handlers)(f: (
    RemoteEndpoint,
    () => Seq[Message]
  ) => T): T = {
    val sent                      = new ConcurrentLinkedQueue[Message]
    val consumer: MessageConsumer = msg => sent.add(msg)
    // lsp4j's read loop hands each incoming message to RemoteEndpoint.consume, and stops at the
    // first one that throws
    val endpoint = new RemoteEndpoint(consumer, new JsonrpcServer(handlers, TestLogger))
    f(endpoint, () => sent.asScala.toVector)
  }

  private def request(id: Int, method: String): RequestMessage = {
    val msg = new RequestMessage
    msg.setId(id)
    msg.setMethod(method)
    msg.setParams(null)
    msg
  }

  private val handlers = Handlers(
    Seq(
      NotificationHandler.of[Object]("not-implemented-notification") { (_, _) =>
        ???
      }
    ),
    Seq(
      RequestHandler.of[Object, Object]("not-implemented") { (_, _) =>
        ???
      },
      RequestHandler.of[Object, Object]("ok") { (_, _) =>
        CompletableFuture.completedFuture("ok")
      }
    ),
    Nil
  )

  test("request handler throwing an Error gets an error response") {
    withEndpoint(handlers) { (endpoint, sent) =>
      endpoint.consume(request(1, "not-implemented"))
      endpoint.consume(request(2, "ok"))

      val responses = sent().collect { case r: ResponseMessage => r.getId -> r }.toMap
      val failed    = responses.get("1").getOrElse(fail(s"No response to request 1 in ${sent()}"))
      assert(failed.getError != null, s"Expected an error response, got $failed")
      assert(
        String.valueOf(failed.getError.getData).contains("NotImplementedError"),
        failed.getError
      )
      val ok = responses.get("2").getOrElse(fail(s"No response to request 2 in ${sent()}"))
      assertEquals(ok.getError, null)
      assertEquals(ok.getResult, "ok")
    }
  }

  test("cancelling a request cancels the token of its handler") {
    val pool   = ExecutionContext.fromExecutorService(Executors.newCachedThreadPool())
    val tokens = new ConcurrentLinkedQueue[CancelToken]
    val cancellableHandlers = Handlers(
      Nil,
      Seq(
        RequestHandler.of[Object, Object]("cancellable") { (_, _) =>
          CancelTokens.future[Object] { token =>
            tokens.add(token)
            Promise[Object]().future // never completes by itself
          }(using pool)
        }
      ),
      Nil
    )
    try
      withEndpoint(cancellableHandlers) { (endpoint, sent) =>
        endpoint.consume(request(1, "cancellable"))
        val token = Option(tokens.peek()).getOrElse(fail("Handler not called"))
        assert(!token.onCancel().toCompletableFuture.isDone)

        val cancelParams = new CancelParams
        cancelParams.setId("1")
        val cancel = new NotificationMessage
        cancel.setMethod("$/cancelRequest")
        cancel.setParams(cancelParams)
        endpoint.consume(cancel)

        assert(token.onCancel().toCompletableFuture.get(10L, TimeUnit.SECONDS))
        val response = sent()
          .collectFirst { case r: ResponseMessage if r.getId == "1" => r }
          .getOrElse(fail(s"No response to request 1 in ${sent()}"))
        assert(response.getError != null, s"Expected an error response, got $response")
        assertEquals(response.getError.getCode, ResponseErrorCode.RequestCancelled.getValue)
      }
    finally pool.shutdown()
  }

  test("notification handler throwing an Error doesn't stop processing") {
    withEndpoint(handlers) { (endpoint, sent) =>
      val notification = new NotificationMessage
      notification.setMethod("not-implemented-notification")
      endpoint.consume(notification)
      endpoint.consume(request(1, "ok"))

      val responses = sent().collect { case r: ResponseMessage => r.getId -> r }.toMap
      assertEquals(responses.get("1").map(_.getResult), Some("ok"))
    }
  }
}
