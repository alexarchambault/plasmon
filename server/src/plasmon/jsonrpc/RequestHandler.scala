package plasmon.jsonrpc

import plasmon.Logger

import java.lang.reflect.Type
import java.util.concurrent.CompletableFuture

import scala.reflect.ClassTag

/** Handles a JSON-RPC request
  *
  * lsp4j cancels the future `call` returns when the client sends `$/cancelRequest`. Only that very
  * future gets cancelled: futures it was derived from (via `thenApply`, `thenCompose`, `asJava`, …)
  * are left alone. Handlers whose work can be interrupted should return the future of
  * [[plasmon.ide.CancelTokens.future]], and do all their work inside it.
  */
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
      (_, logger) => {
        logger.log("Unhandled request")
        CompletableFuture.failedFuture(new Exception(s"Request $name", cause))
      },
      classOf[Object],
      classOf[Object]
    )
}
