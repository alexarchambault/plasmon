// Based on https://github.com/scalameta/metals/blob/0ad0bc184f82dbd178d01f76913ea6bdfa98db14/metals/src/main/scala/scala/meta/internal/metals/FutureCancelToken.scala

package plasmon.ide

import java.lang
import java.util.concurrent.CompletionStage

import scala.compat.java8.FutureConverters._
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.util.Failure
import scala.util.Success

import scala.meta.pc.CancelToken

/** A cancel token backed by a Scala future.
  */
case class FutureCancelToken(f: Future[Boolean])(implicit ec: ExecutionContext)
    extends CancelToken {
  var isCancelled: Boolean = false
  f.onComplete {
    case Failure(_) =>
      isCancelled = true
    case Success(cancel) =>
      isCancelled = cancel
  }

  override def checkCanceled(): Unit = {
    if (isCancelled)
      throw new InterruptedException()
  }

  override def onCancel(): CompletionStage[lang.Boolean] =
    f.map(cancel => java.lang.Boolean.valueOf(cancel)).toJava
}

object FutureCancelToken {
  def fromUnit(
    f: Future[Unit]
  )(implicit ec: ExecutionContext): FutureCancelToken =
    FutureCancelToken(f.map(_ => true))
}
