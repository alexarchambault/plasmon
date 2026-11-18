// Based on https://github.com/scalameta/metals/blob/0ad0bc184f82dbd178d01f76913ea6bdfa98db14/metals/src/main/scala/scala/meta/internal/metals/FutureCancelToken.scala

package plasmon.ide

import java.lang
import java.util.concurrent.CompletionStage

import scala.compat.java8.FutureConverters.*
import scala.concurrent.{ExecutionContext, Future}
import scala.meta.pc.CancelToken
import scala.util.{Failure, Success}

/** A cancel token backed by a Scala future.
  */
case class FutureCancelToken(f: Future[Boolean])(implicit ec: ExecutionContext)
    extends CancelToken {
  // Read from f rather than set by a callback of it, which runs some time after f completes:
  // checkCanceled throws as soon as the request is cancelled
  def isCancelled: Boolean =
    f.value match {
      case Some(Success(cancel)) => cancel
      case Some(Failure(_))      => true
      case None                  => false
    }

  // InterruptedException rather than the CancellationException of the CancelChecker contract: the
  // presentation compilers handle the former as an interruption, but report the latter as a crash
  override def checkCanceled(): Unit = {
    if (isCancelled)
      throw new InterruptedException()
  }

  override def onCancel(): CompletionStage[lang.Boolean] =
    f.map(cancel => java.lang.Boolean.valueOf(cancel)).toJava
}
