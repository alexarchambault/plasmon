package plasmon.ide

import scala.meta.inputs.Input
import scala.meta.inputs.Position

// Based on https://github.com/scalameta/metals/blob/0ad0bc184f82dbd178d01f76913ea6bdfa98db14/metals/src/main/scala/scala/meta/internal/parsing/JavaTokens.scala#L12-L21
private case class JavaToken(
  id: Int,
  text: String,
  start: Int,
  end: Int,
  input: Input,
  isLF: Boolean = false
) {
  lazy val pos: Position = Position.Range(input, start, end)
}
