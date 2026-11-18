package plasmon

import scala.collection.mutable

abstract class PlasmonActor[T](implicit ctx: castor.Context) extends castor.BatchActor[T] {

  // Senders add to it from any thread, the actor removes from it, and others (like indexing
  // threads, to know if they should stop) read it: only accessed with its lock held
  private val awaitingMessages0 = new mutable.Queue[T]

  /** Messages sent to this actor that it hasn't processed yet */
  def awaitingMessages: Vector[T] =
    awaitingMessages0.synchronized(awaitingMessages0.toVector)

  /** Whether a message matching `f` is waiting to be processed */
  def isAwaiting(f: T => Boolean): Boolean =
    awaitingMessages0.synchronized(awaitingMessages0.exists(f))

  override def send(
    t: T
  )(implicit
    fileName: sourcecode.FileName,
    line: sourcecode.Line
  ): Unit = {
    awaitingMessages0.synchronized(awaitingMessages0.enqueue(t))
    super.send(t)
  }
  override def runBatch(msgs: Seq[T]): Unit =
    awaitingMessages0.synchronized {
      for (msg <- msgs) {
        val idx = awaitingMessages0.indexOf(msg)
        if (idx < 0)
          scribe.error(s"Inconsistent actor queue for $this (message not found: $msg)")
        else
          awaitingMessages0.remove(idx)
      }
    }
}
