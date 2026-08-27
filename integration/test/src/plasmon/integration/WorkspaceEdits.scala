package plasmon.integration

import org.eclipse.lsp4j as l

import java.net.URI
import java.nio.file.Paths

import scala.jdk.CollectionConverters.*

/** Carrying out a workspace edit, the way an editor would.
  *
  * The test's language client uses this when the server asks it to apply an edit. The CLI has its
  * own copy of this on the server side (`plasmon.servercommand.WorkspaceEdits`), since there is no
  * client there to ask - having the two written separately is what makes the tests worth running
  * both ways: they meet at the files on disk, and a test that reads those back catches a
  * disagreement.
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

  private def pathOf(uri: String): os.Path =
    os.Path(Paths.get(new URI(uri)))

  private def perFile(edit: l.WorkspaceEdit): Seq[(os.Path, Seq[l.TextEdit])] = {
    val fromChanges = Option(edit.getChanges)
      .toSeq
      .flatMap(_.asScala.toSeq)
      .map {
        case (uri, edits) => (pathOf(uri), edits.asScala.toSeq)
      }
    val fromDocumentChanges = Option(edit.getDocumentChanges)
      .toSeq
      .flatMap(_.asScala.toSeq)
      .filter(_.isLeft)
      .map(_.getLeft)
      .map { documentEdit =>
        val edits = documentEdit.getEdits.asScala.toSeq.filter(_.isLeft).map(_.getLeft)
        (pathOf(documentEdit.getTextDocument.getUri), edits)
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
    val lineStarts = content
      .linesWithSeparators
      .scanLeft(0)(_ + _.length)
      .toVector
    def offset(pos: l.Position): Int =
      math.min(
        content.length,
        lineStarts.lift(pos.getLine).getOrElse(content.length) + pos.getCharacter
      )
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
