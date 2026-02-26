package plasmon.ide

import org.eclipse.lsp4j as l
import plasmon.PlasmonEnrichments.*

import scala.meta.internal.semanticdb as s

// Originally based on https://github.com/scalameta/metals/blob/2a6f8a437a1ce7c44140673edfe34bb74dfd33be/metals/src/main/scala/scala/meta/internal/metals/ReferenceProvider.scala#L851-L883 or an earlier version of that file

trait AdjustRange {
  def apply(range: s.Range, text: String, symbol: String): Option[s.Range]
  def apply(loc: l.Location, text: String, symbol: String): Option[l.Location] = {
    val semRange = s.Range(
      loc.getRange.getStart.getLine,
      loc.getRange.getStart.getCharacter,
      loc.getRange.getEnd.getLine,
      loc.getRange.getEnd.getCharacter
    )
    for (adjusted <- apply(semRange, text, symbol)) yield {
      loc.setRange(adjusted.toLsp)
      loc
    }
  }
}

object AdjustRange {
  def apply(adjust: (s.Range, String, String) => Option[s.Range]): AdjustRange =
    new AdjustRange {
      def apply(range: s.Range, text: String, symbol: String): Option[s.Range] =
        adjust(range, text, symbol)
    }

  object none extends AdjustRange {
    def apply(range: s.Range, text: String, symbol: String): Option[s.Range] =
      Some(range)
    override def apply(
      loc: l.Location,
      text: String,
      symbol: String
    ): Option[l.Location] = Some(loc)
  }
}
