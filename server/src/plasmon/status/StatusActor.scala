package plasmon.status

import plasmon.Status
import plasmon.languageclient.PlasmonLanguageClient

import scala.jdk.CollectionConverters.*
import scala.util.Try
import scala.concurrent.duration.FiniteDuration
import java.util.concurrent.ScheduledExecutorService
import java.util.concurrent.TimeUnit
import java.util.concurrent.ScheduledFuture

class StatusActor(
  languageClient: PlasmonLanguageClient,
  status: Status,
  minDelay: FiniteDuration,
  ses: ScheduledExecutorService
)(implicit ctx: castor.Context) extends plasmon.PlasmonActor[StatusActor.Message]
    with AutoCloseable {
  import StatusActor.*

  private var lastUpdateInstant = -1L
  private val minDelayMs        = minDelay.toMillis

  private var unprocessed = Seq.empty[Message.RefreshStatus]

  private var closed             = false
  private var scheduledFutureOpt = Option.empty[ScheduledFuture[?]]

  override def runBatch(msgs: Seq[Message]): Unit = {
    super.runBatch(msgs)
    val refreshMessages = msgs.collect {
      case r: Message.RefreshStatus => r
    }
    if (!closed && refreshMessages.nonEmpty) {
      val priority = refreshMessages.exists(_.priority.contains(lastUpdateInstant))
      val now      = System.currentTimeMillis()
      val proceed  = priority || (now - lastUpdateInstant >= minDelayMs)
      if (proceed) {
        val res = Try {
          val plasmonFileStatus = status.plasmonFileStatus()
          plasmonFileStatus match {
            case Some((path, status0)) =>
              languageClient.statusUpdate(path.toNIO.toUri.normalize.toASCIIString, status0.asJava)
            case None =>
              languageClient.statusUpdate("", Nil.asJava)
          }
          lastUpdateInstant = now
          plasmonFileStatus
        }
        for (msg <- refreshMessages.iterator ++ unprocessed.iterator; f <- msg.onDone.iterator)
          f(res)
        unprocessed = Nil
      }
      else {
        unprocessed = unprocessed ++ refreshMessages.filter(_.onDone.nonEmpty)
        if (scheduledFutureOpt.isEmpty)
          scheduledFutureOpt = Some {
            ses.schedule(
              new Runnable {
                def run(): Unit =
                  try
                    if (!closed)
                      send(
                        Message.RefreshStatus(
                          priority = Some(lastUpdateInstant),
                          onDone = Some { _ =>
                            scheduledFutureOpt = None
                          }
                        )
                      )
                  catch {
                    case t: Throwable =>
                      if (closed)
                        scribe.info("Status update after delay interrupted")
                      else
                        scribe.error("Error updating status after fixed delay", t)
                  }
              },
              lastUpdateInstant + minDelayMs - now,
              TimeUnit.MILLISECONDS
            )
          }
      }
    }
  }

  def close(): Unit = {
    closed = true
    for (f <- scheduledFutureOpt) {
      f.cancel(true)
      scheduledFutureOpt = None
    }
  }
}

object StatusActor {
  sealed abstract class Message extends Product with Serializable
  object Message {
    final case class RefreshStatus(
      priority: Option[Long] = None,
      onDone: Option[Try[Option[(os.Path, Seq[PlasmonLanguageClient.StatusUpdate])]] => Unit] = None
    ) extends Message
  }
}
