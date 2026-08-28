package plasmon.servercommand

import org.eclipse.lsp4j as l
import plasmon.PlasmonEnrichments.*

import scala.jdk.CollectionConverters.*
import scala.meta.Input

/** Carrying out a workspace edit, the way the editor on the other end of an LSP connection would.
  *
  * The commands that stand in for an editor - [[LspDidOpen]] and friends - have to do this
  * themselves: over LSP the server asks the client to apply an edit and the client obliges, and
  * there is no client here.
  */
object WorkspaceEdits {

  /** Applies an edit to the files on disk. Returns the files it changed. */
  def applyToDisk(edit: l.WorkspaceEdit): Seq[os.Path] =
    perFile(edit).map {
      case (path, edits) =>
        val content = if (os.exists(path)) os.read(path) else ""
        os.write.over(path, applyTo(content, edits), createFolders = true)
        path
    }

  /** The edits of a workspace edit, grouped by the file they apply to.
    *
    * A workspace edit carries them either as plain per-URI changes or as versioned document
    * changes; the latter can also ask for files to be created, renamed or deleted, which nothing
    * here produces and which this refuses rather than silently dropping.
    */
  private def perFile(edit: l.WorkspaceEdit): Seq[(os.Path, Seq[l.TextEdit])] = {
    val fromChanges = Option(edit.getChanges)
      .toSeq
      .flatMap(_.asScala.toSeq)
      .map {
        case (uri, edits) => (uri.osPathFromUri, edits.asScala.toSeq)
      }
    val fromDocumentChanges = Option(edit.getDocumentChanges)
      .toSeq
      .flatMap(_.asScala.toSeq)
      .map { documentChange =>
        if (!documentChange.isLeft)
          sys.error(s"Unsupported resource operation in workspace edit: ${documentChange.getRight}")
        val documentEdit = documentChange.getLeft
        val edits = documentEdit.getEdits.asScala.toSeq.map { edit0 =>
          if (edit0.isLeft) edit0.getLeft
          else
            // Its new text carries snippet placeholders for the editor to put a cursor at, so
            // writing it out as-is would leave a literal `$0` in someone's source file
            sys.error(s"Unsupported snippet edit in workspace edit: ${edit0.getRight}")
        }
        (documentEdit.getTextDocument.getUri.osPathFromUri, edits)
      }
    (fromChanges ++ fromDocumentChanges)
      .groupBy(_._1)
      .toVector
      .sortBy(_._1)
      .map {
        case (path, grouped) => (path, grouped.flatMap(_._2))
      }
  }

  /** Applies text edits to a document, last one first so that earlier offsets stay valid. */
  private def applyTo(content: String, edits: Seq[l.TextEdit]): String = {
    val input = Input.String(content)
    def offset(pos: l.Position): Int =
      math.min(content.length, input.toOffset(pos.getLine, pos.getCharacter))
    edits
      .sortBy(edit => (edit.getRange.getStart.getLine, edit.getRange.getStart.getCharacter))
      .reverse
      .foldLeft(content) { (content0, edit) =>
        val from = offset(edit.getRange.getStart)
        val to   = math.max(from, offset(edit.getRange.getEnd))
        content0.take(from) + edit.getNewText + content0.drop(to)
      }
  }
}
