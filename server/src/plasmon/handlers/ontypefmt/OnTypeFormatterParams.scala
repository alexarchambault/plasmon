package plasmon.handlers.ontypefmt

import org.eclipse.lsp4j as l

import scala.meta.Position
import scala.meta.tokens.Tokens

// Originally based on https://github.com/scalameta/metals/blob/2a6f8a437a1ce7c44140673edfe34bb74dfd33be/metals/src/main/scala/scala/meta/internal/metals/formatting/OnTypeFormattingProvider.scala#L15-L25
private case class OnTypeFormatterParams(
  sourceText: String,
  position: l.Position,
  triggerChar: String,
  startPos: Position,
  endPos: Position,
  tokens: Option[Tokens]
) extends FormatterParams {
  lazy val splitLines: Array[String] = sourceText.split("\\r?\\n")
  val range                          = new l.Range(position, position)
}
