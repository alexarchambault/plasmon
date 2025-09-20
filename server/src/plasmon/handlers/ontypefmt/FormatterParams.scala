// Originally based on https://github.com/scalameta/metals/blob/2a6f8a437a1ce7c44140673edfe34bb74dfd33be/metals/src/main/scala/scala/meta/internal/metals/formatting/FormatterParams.scala#L4

package plasmon.handlers.ontypefmt

trait FormatterParams {
  def startPos: meta.Position
  def endPos: meta.Position
  def sourceText: String
}
