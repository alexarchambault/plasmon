// Based on https://github.com/scalameta/metals/blob/0ad0bc184f82dbd178d01f76913ea6bdfa98db14/metals/src/main/scala/scala/meta/internal/parsing/EmptyResult.scala#L9

package plasmon.ide

import scala.meta.Position

object EmptyResult {
  case object Unchanged extends EmptyResult
  case object NoMatch   extends EmptyResult
  def unchanged: Either[EmptyResult, Position] = Left(Unchanged)
  def noMatch: Either[EmptyResult, Position]   = Left(NoMatch)
}

sealed trait EmptyResult
