package plasmon.ide

import java.util.{List => JList}

import plasmon.PlasmonEnrichments.*
import scala.jdk.CollectionConverters.*
import scala.meta.pc
import scala.meta.pc.AutoImportsResult
import scala.meta.pc.HoverSignature

import org.eclipse.{lsp4j => l}

// Originally based on https://github.com/scalameta/metals/blob/2a6f8a437a1ce7c44140673edfe34bb74dfd33be/metals/src/main/scala/scala/meta/internal/metals/AdjustLspData.scala#L19-L108 or an earlier version of that file

trait AdjustLspData {

  def paths: Option[(appliesTo: os.Path, userPath: os.Path)]

  def adjustPos(pos: l.Position, adjustToZero: Boolean = true): l.Position

  def adjustRange(range: l.Range): l.Range =
    new l.Range(
      adjustPos(range.getStart),
      adjustPos(range.getEnd)
    )

  def adjustTextEdits(
    edits: JList[l.TextEdit]
  ): java.util.List[l.TextEdit] =
    edits.asScala.map { loc =>
      loc.setRange(adjustRange(loc.getRange))
      loc
    }.asJava

  def adjustDocumentHighlight(
    highlights: JList[l.DocumentHighlight]
  ): java.util.List[l.DocumentHighlight] =
    highlights.asScala.map { loc =>
      loc.setRange(adjustRange(loc.getRange))
      loc
    }.asJava

  def adjustDiagnostic(
    diag: l.Diagnostic
  ): l.Diagnostic = {
    diag.setRange(adjustRange(diag.getRange))
    diag
  }

  def adjustLocation(location: l.Location): l.Location =
    new l.Location(location.getUri, adjustRange(location.getRange))

  def adjustReferencesResult(
    referencesResult: pc.ReferencesResult,
    additionalAdjust: AdjustRange,
    text: String
  ): ReferencesResult =
    new ReferencesResult(
      referencesResult.symbol,
      referencesResult
        .locations()
        .asScala
        .flatMap(loc =>
          additionalAdjust(loc, text, referencesResult.symbol)
            .map(adjustLocation)
        )
        .toList
    )

  def adjustLocations(
    locations: java.util.List[l.Location]
  ): JList[l.Location]

  def adjustHoverResp(hover: l.Hover): l.Hover =
    if (hover.getRange == null)
      hover
    else {
      val newRange = adjustRange(hover.getRange)
      val newHover = new l.Hover
      newHover.setContents(hover.getContents)
      newHover.setRange(newRange)
      newHover
    }

  def adjustHoverResp(hover: HoverSignature): HoverSignature =
    hover.getRange.map(rng => hover.withRange(adjustRange(rng))).orElse(hover)

  def adjustCompletionListInPlace(list: l.CompletionList): Unit = {
    for (item <- list.getItems.asScala) {
      for (textEdit <- item.getLeftTextEdit)
        textEdit.setRange(adjustRange(textEdit.getRange))
      for (l <- Option(item.getAdditionalTextEdits); textEdit <- l.asScala)
        textEdit.setRange(adjustRange(textEdit.getRange))
    }
  }

  def adjustImportResult(
    autoImportResult: AutoImportsResult
  ): Unit = {
    for (textEdit <- autoImportResult.edits.asScala)
      textEdit.setRange(adjustRange(textEdit.getRange))
  }
}
