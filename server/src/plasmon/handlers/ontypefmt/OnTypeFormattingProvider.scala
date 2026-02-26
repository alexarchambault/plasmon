package plasmon.handlers.ontypefmt

import org.eclipse.lsp4j as l
import plasmon.PlasmonEnrichments.*
import plasmon.ide.{Buffers, Trees}
import plasmon.index.BspData

import scala.meta.inputs.Input
import scala.meta.internal.mtags.GlobalSymbolIndex

// Originally based on https://github.com/scalameta/metals/blob/2a6f8a437a1ce7c44140673edfe34bb74dfd33be/metals/src/main/scala/scala/meta/internal/metals/formatting/OnTypeFormattingProvider.scala#L33-L74
private class OnTypeFormattingProvider(
  buffers: Buffers,
  trees: Trees,
  bspData: BspData
) {

  // The order of which this is important to know which will first return the Edits
  private val formatters: List[OnTypeFormatter] = List(
    MultilineString,
    InterpolateStringContext
  )

  def format(params: l.DocumentOnTypeFormattingParams): List[l.TextEdit] = {

    val path        = params.getTextDocument.getUri.osPathFromUri
    val range       = new l.Range(params.getPosition, params.getPosition)
    val triggerChar = params.getCh
    val position    = params.getPosition

    val edits = for {
      module     <- bspData.inverseSources(path).map(_.getUri).map(GlobalSymbolIndex.BuildTarget(_))
      sourceText <- buffers.get(path)
      virtualFile = Input.VirtualFile(path.toNIO.toUri.toASCIIString, sourceText)
      startPos <- range.getStart.toMeta(virtualFile)
      endPos   <- range.getEnd.toMeta(virtualFile)
    } yield {
      val onTypeformatterParams =
        OnTypeFormatterParams(
          sourceText,
          position,
          triggerChar,
          startPos,
          endPos,
          trees.tokenized(module, path)
        )
      formatters
        .iterator
        .flatMap(_.contribute(onTypeformatterParams).iterator)
        .find(_ => true)
        .getOrElse(Nil)
    }
    edits.getOrElse(Nil)
  }
}
