// Originally based on https://github.com/scalameta/metals/blob/2a6f8a437a1ce7c44140673edfe34bb74dfd33be/metals/src/main/scala/scala/meta/internal/metals/formatting/ScalaCliDependencyRangeFormatter.scala or an earlier version of that file

package plasmon.handlers.ontypefmt

import org.eclipse.{lsp4j => l}

object ScalaCliDependencyRangeFormatter extends RangeFormatter {

  override def contribute(
    range: RangeFormatterParams
  ): Option[List[l.TextEdit]] = {

    val line = range.sourceText.substring(
      range.startPos.start - range.startPos.startColumn,
      range.endPos.end
    )
    DependencyConverter
      .convertSbtToMillStyleIfPossible(line)
      .map(converted =>
        new l.TextEdit(
          new l.Range(
            new l.Position(range.startPos.startLine, 0),
            new l.Position(range.startPos.startLine, line.length)
          ),
          converted.replacementDirective.replace("\"", "")
        )
      )
      .map(List(_))

  }

}
