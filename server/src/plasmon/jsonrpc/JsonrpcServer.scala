package plasmon.jsonrpc

import org.eclipse.lsp4j.jsonrpc.Endpoint
import org.eclipse.lsp4j.jsonrpc.json.JsonRpcMethod
import plasmon.jsonrpc.{Handlers, NotificationHandler, RequestHandler}
import plasmon.Logger

import java.util.concurrent.{CancellationException, CompletableFuture => JCFuture}
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.AtomicInteger

final class JsonrpcServer(
  handlers: Handlers,
  requestsLogger: Logger
) extends Endpoint {

  private val notificationHandlersMap = handlers.notificationHandlers.map(r => r.name -> r).toMap
  private val requestHandlersMap      = handlers.requestHandlers.map(r => r.name -> r).toMap

  scribe.info("request handlers: " + requestHandlersMap.keys.toVector.sorted)

  val ongoingRequests      = new ConcurrentHashMap[(String, Object, JCFuture[Object]), Unit]
  val ongoingNotifications = new ConcurrentHashMap[(String, Object), Unit]

  def supportedMethods: Map[String, JsonRpcMethod] = {
    val fromRequests = requestHandlersMap.map {
      case (method, handler) =>
        method -> JsonRpcMethod.request(method, handler.returnType, handler.inputType)
    }
    val fromNotifications = notificationHandlersMap.map {
      case (method, handler) =>
        method -> JsonRpcMethod.notification(method, handler.inputType)
    }
    fromRequests ++ fromNotifications
  }

  private val requestCount = new AtomicInteger

  override def request(method: String, parameter: Object): JCFuture[Object] = {
    val logger = requestsLogger.addPrefix(s"[${requestCount.incrementAndGet()}-$method] ")
    logger.log(s"Parameter $parameter")
    val handler = requestHandlersMap
      .getOrElse(
        method,
        RequestHandler.fail(method, new Exception(s"No handler for request $method"))
      )
      .as[Object, Object]
    val futureResp =
      try handler.call(parameter, logger)
      catch {
        case t: Throwable =>
          logger.log("Exception when starting to process request")
          logger.log(t)
          throw t
      }
    ongoingRequests.put((method, parameter, futureResp), ())
    futureResp.handle { (resOrNull, exOrNull) =>
      ongoingRequests.remove((method, parameter, futureResp))
      if (exOrNull == null)
        logger.log(s"Response ${resOrNull.toString.take(100)}")
      else
        exOrNull match {
          case _: CancellationException =>
            logger.log("Cancelled")
          case _ =>
            logger.log("Exception:")
            logger.log(exOrNull)
        }
      ()
    }
    futureResp
  }
  override def notify(method: String, parameter: Object): Unit = {
    val logger = requestsLogger.addPrefix(s"[*${requestCount.incrementAndGet()}-$method] ")
    logger.log(s"Parameter $parameter")
    val handler = notificationHandlersMap
      .getOrElse(
        method,
        NotificationHandler.fail(method, new Exception(s"No handler for notification $method"))
      )
      .as[Object]
    try {
      ongoingNotifications.put((method, parameter), ())
      try handler.call(parameter, logger)
      catch {
        case t: Throwable =>
          logger.log("Exception when starting to process notification")
          logger.log(t)
          throw t
      }
    }
    finally
      ongoingNotifications.remove((method, parameter))
  }
}
