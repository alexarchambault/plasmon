package plasmon.handlers.ontypefmt

import org.eclipse.lsp4j as l

// Originally based on https://github.com/scalameta/metals/blob/2a6f8a437a1ce7c44140673edfe34bb74dfd33be/metals/src/main/scala/scala/meta/internal/metals/formatting/RangeFormattingProvider.scala#L26-L30
trait RangeFormatter {
  def contribute(
    rangeFormatterParams: RangeFormatterParams
  ): Option[List[l.TextEdit]]
}
