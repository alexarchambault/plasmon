package plasmon.jsonrpc

import plasmon.Logger

import java.lang.reflect.Type
import java.util.concurrent.CompletableFuture

import scala.reflect.ClassTag

final case class RequestHandler[A, B](
  name: String,
  call: (A, Logger) => CompletableFuture[B],
  inputClass: Class[A],
  outputClass: Class[B]
) {
  def as[A0, B0]: RequestHandler[A0, B0] =
    this.asInstanceOf[RequestHandler[A0, B0]]

  def inputType: Type  = inputClass
  def returnType: Type = outputClass

  def ifNull(other: RequestHandler[A, B]): RequestHandler[A, B] =
    copy(
      call = (a, logger) =>
        call(a, logger).thenCompose { bOrNull =>
          if (bOrNull == null) other.call(a, logger)
          else CompletableFuture.completedFuture(bOrNull)
        }
    )
}

object RequestHandler {

  def of[A: ClassTag, B: ClassTag](name: String)(call: (A, Logger) => CompletableFuture[B])
    : RequestHandler[A, B] =
    RequestHandler(
      name,
      call,
      implicitly[ClassTag[A]].runtimeClass.asInstanceOf[Class[A]],
      implicitly[ClassTag[B]].runtimeClass.asInstanceOf[Class[B]]
    )

  def fail(name: String, cause: Throwable = null): RequestHandler[Object, Object] =
    RequestHandler[Object, Object](
      name,
      (params, logger) => {
        logger.log("Unhandled request")
        CompletableFuture.failedFuture(new Exception(s"Request $name", cause))
      },
      classOf[Object],
      classOf[Object]
    )
}
