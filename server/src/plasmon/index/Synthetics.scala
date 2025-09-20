// Based on https://github.com/scalameta/metals/blob/0ad0bc184f82dbd178d01f76913ea6bdfa98db14/metals/src/main/scala/scala/meta/internal/metals/Synthetics.scala

package plasmon.index

import scala.meta.internal.semanticdb.Synthetic

/** Utilities to work with SemanticDB synthetics.
  */
private object Synthetics {
  def existsSymbol(synthetic: Synthetic)(fn: String => Boolean): Boolean =
    foreachSymbol(synthetic) { symbol =>
      if (fn(symbol)) Stop
      else Continue
    }.isStop

  sealed abstract class ForeachResult {
    def isStop: Boolean = this == Stop
  }
  case object Continue extends ForeachResult
  case object Stop     extends ForeachResult

  def foreachSymbol(
    synthetic: Synthetic
  )(fn: String => ForeachResult): ForeachResult = {
    import scala.meta.internal.semanticdb._
    def isStop(t: Tree): Boolean =
      t match {
        case ApplyTree(function, arguments) =>
          isStop(function) || arguments.exists(isStop)
        case SelectTree(_, id) =>
          id.exists(isStop)
        case IdTree(symbol) =>
          fn(symbol).isStop
        case TypeApplyTree(function, _) =>
          isStop(function)
        case FunctionTree(_, body) =>
          isStop(body)
        case LiteralTree(_) =>
          false
        case MacroExpansionTree(_, _) =>
          false
        case OriginalTree(_) => false
        case Tree.Empty      => false
      }
    if (isStop(synthetic.tree)) Stop
    else Continue
  }
}
