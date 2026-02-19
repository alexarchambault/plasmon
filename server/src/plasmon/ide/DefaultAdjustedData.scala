package plasmon.ide

import java.util.{List => JList}

import scala.meta.pc.AutoImportsResult

import org.eclipse.{lsp4j => l}

// Based on https://github.com/scalameta/metals/blob/0ad0bc184f82dbd178d01f76913ea6bdfa98db14/metals/src/main/scala/scala/meta/internal/metals/AdjustLspData.scala#L136-L164
object DefaultAdjustedData extends AdjustLspData {

  def paths: Option[(appliesTo: os.Path, userPath: os.Path)] =
    None

  override def adjustPos(
    pos: l.Position,
    adjustToZero: Boolean = true
  ): l.Position = identity(pos)

  override def adjustRange(range: l.Range): l.Range = identity(range)

  override def adjustTextEdits(
    edits: JList[l.TextEdit]
  ): JList[l.TextEdit] = identity(edits)

  override def adjustLocations(
    locations: JList[l.Location]
  ): JList[l.Location] = identity(locations)

  override def adjustHoverResp(hover: l.Hover): l.Hover = identity(hover)

  override def adjustCompletionListInPlace(list: l.CompletionList): Unit = {}

  override def adjustImportResult(
    autoImportResult: AutoImportsResult
  ): Unit = {}

  override def adjustDiagnostic(
    diag: l.Diagnostic
  ): l.Diagnostic = identity(diag)
}
