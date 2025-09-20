package plasmon

import scala.collection.mutable

abstract class PlasmonActor[T](implicit ctx: castor.Context) extends castor.BatchActor[T] {
  val awaitingMessages = new mutable.Queue[T]
  override def send(
    t: T
  )(implicit
    fileName: sourcecode.FileName,
    line: sourcecode.Line
  ): Unit = {
    awaitingMessages.enqueue(t)
    super.send(t)
  }
  override def runBatch(msgs: Seq[T]): Unit =
    for (msg <- msgs) {
      val idx = awaitingMessages.indexOf(msg)
      if (idx < 0)
        scribe.error(s"Inconsistent actor queue for $this (message not found: $msg)")
      else
        awaitingMessages.remove(idx)
    }
}
