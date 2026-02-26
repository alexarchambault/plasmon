package plasmon.handlers.ontypefmt

import org.eclipse.lsp4j as l

import scala.meta.tokens.Tokens

// Originally based on https://github.com/scalameta/metals/blob/2a6f8a437a1ce7c44140673edfe34bb74dfd33be/metals/src/main/scala/scala/meta/internal/metals/formatting/RangeFormattingProvider.scala#L15-L24
case class RangeFormatterParams(
  sourceText: String,
  range: l.Range,
  formattingOptions: l.FormattingOptions,
  startPos: meta.Position,
  endPos: meta.Position,
  tokens: Option[Tokens]
) extends FormatterParams {
  lazy val splitLines: Array[String] = sourceText.split("\\r?\\n")
}
