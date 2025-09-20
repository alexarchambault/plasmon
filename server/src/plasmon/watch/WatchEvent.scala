package plasmon.watch

import scala.collection.mutable

sealed abstract class WatchEvent extends Product with Serializable

object WatchEvent {
  sealed abstract class FileWatchEvent extends WatchEvent {
    def path: os.Path
  }
  case class CreateOrModify(path: os.Path) extends FileWatchEvent
  case class Delete(path: os.Path)         extends FileWatchEvent
  // indicates that file watching events may have been lost
  // for the given path, or for an unknown path if path is null
  case class Overflow(path: os.Path) extends FileWatchEvent

  case object Reindex extends WatchEvent

  def normalize(events: Seq[WatchEvent]): Seq[WatchEvent] =
    if (events.contains(Reindex)) Seq(Reindex)
    else
      normalizeFileEvents(
        events.collect {
          case f: FileWatchEvent => f
        }
      )

  def normalizeFileEvents(events: Seq[FileWatchEvent]): Seq[FileWatchEvent] = {
    val seen = new mutable.HashSet[os.Path]
    // keep only last event related to each file
    events
      .reverseIterator
      .flatMap { ev =>
        if (seen.contains(ev.path)) Iterator.empty
        else {
          seen += ev.path
          Iterator(ev)
        }
      }
      .toVector
      .reverse
  }
}
