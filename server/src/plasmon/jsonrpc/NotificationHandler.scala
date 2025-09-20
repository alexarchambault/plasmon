package plasmon.jsonrpc

import java.lang.reflect.Type

import scala.reflect.ClassTag
import plasmon.Logger

final case class NotificationHandler[A](
  name: String,
  call: (A, Logger) => Unit,
  inputClass: Class[A]
) {
  def as[A0]: NotificationHandler[A0] =
    this.asInstanceOf[NotificationHandler[A0]]

  def inputType: Type = inputClass
}

object NotificationHandler {

  def of[A: ClassTag](name: String)(call: (A, Logger) => Unit): NotificationHandler[A] =
    NotificationHandler(name, call, implicitly[ClassTag[A]].runtimeClass.asInstanceOf[Class[A]])

  def fail(name: String, cause: Throwable = null): NotificationHandler[Object] =
    NotificationHandler[Object](
      name,
      (_, logger) => {
        logger.log("Unhandled notification")
        throw new Exception(s"Notification $name", cause)
      },
      classOf[Object]
    )
}
