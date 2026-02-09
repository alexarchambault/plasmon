package plasmon.watch

import scala.collection.mutable
import scala.meta.internal.mtags.GlobalSymbolIndex

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

  sealed abstract class TargetWatchEvent extends WatchEvent {
    def targetId: GlobalSymbolIndex.BuildTarget
  }
  case class Compile(targetId: GlobalSymbolIndex.BuildTarget) extends TargetWatchEvent
  case class ComputeInteractiveSemanticdb(targetId: GlobalSymbolIndex.BuildTarget, path: os.Path)
      extends WatchEvent

  def normalize(events: Seq[WatchEvent]): Seq[WatchEvent] =
    if (events.contains(Reindex)) Seq(Reindex)
    else {
      val fileEvents = normalizeFileEvents(
        events.collect {
          case f: FileWatchEvent => f
        }
      )
      val targetEvents = normalizeTargetEvents(
        events.collect {
          case t: TargetWatchEvent => t
        }
      )
      val computeSemdbEvents = normalizeComputeInteractiveSemanticdbEvents(
        events.collect {
          case c: ComputeInteractiveSemanticdb => c
        }
      )
      fileEvents ++ targetEvents ++ computeSemdbEvents
    }

  private def normalizeFileEvents(events: Seq[FileWatchEvent]): Seq[FileWatchEvent] = {
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
  private def normalizeTargetEvents(events: Seq[TargetWatchEvent]): Seq[TargetWatchEvent] = {
    val seen = new mutable.HashSet[GlobalSymbolIndex.BuildTarget]
    // keep only last event related to each target
    events
      .reverseIterator
      .flatMap { ev =>
        if (seen.contains(ev.targetId)) Iterator.empty
        else {
          seen += ev.targetId
          Iterator(ev)
        }
      }
      .toVector
      .reverse
  }
  private def normalizeComputeInteractiveSemanticdbEvents(events: Seq[ComputeInteractiveSemanticdb])
    : Seq[ComputeInteractiveSemanticdb] =
    events.distinct
}
