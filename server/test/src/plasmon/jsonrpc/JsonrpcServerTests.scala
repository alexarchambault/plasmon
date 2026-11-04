package plasmon.jsonrpc

import org.eclipse.lsp4j.jsonrpc.{MessageConsumer, RemoteEndpoint}
import org.eclipse.lsp4j.jsonrpc.messages.{
  Message,
  NotificationMessage,
  RequestMessage,
  ResponseMessage
}
import plasmon.TestLogger

import java.util.concurrent.{CompletableFuture, ConcurrentLinkedQueue}

import scala.jdk.CollectionConverters.*

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
