// Originally based on https://github.com/scalameta/metals/blob/2a6f8a437a1ce7c44140673edfe34bb74dfd33be/metals/src/main/scala/scala/meta/internal/metals/formatting/InterpolateStringContext.scala or an earlier version of that file

package plasmon.handlers.ontypefmt

import org.eclipse.lsp4j as l
import plasmon.PlasmonEnrichments.*

import scala.meta
import scala.meta.inputs.Position
import scala.meta.tokens.Token

/** Formatter that automatically converts a string literal to an interpolation when an interpolation
  * `${}` marker is added in the literal.
  */
private object InterpolateStringContext extends OnTypeFormatter {

  override def contribute(
    params: OnTypeFormatterParams
  ): Option[List[l.TextEdit]] =
    params.triggerChar.head match {
      case '{' => contributeInterpolationContext(params)
      case _   => None
    }

  private def contributeInterpolationContext(
    params: OnTypeFormatterParams
  ): Option[List[l.TextEdit]] = {
    val range = new l.Range(params.position, params.position)
    params.tokens
      .getOrElse(Nil)
      .collectFirst {
        case lit: Token.Constant.String if lit.pos.toLsp.encloses(range) =>
          convertStringToInterpolation(
            lit.pos,
            params.sourceText,
            params.startPos.start
          )
      }
      .flatten
  }

  private def convertStringToInterpolation(
    stringLitPos: meta.Position,
    sourceText: String,
    cursorPos: Int
  ): Option[List[l.TextEdit]] = {
    val expectedDollarPos = cursorPos - 2
    val input             = stringLitPos.input
    if (
      expectedDollarPos >= 0 && cursorPos < sourceText.length() && sourceText
        .substring(expectedDollarPos, cursorPos + 1) == "${}"
    ) {
      def insertTextEdit(offset: Int, text: String): l.TextEdit =
        new l.TextEdit(Position.Range(input, offset, offset).toLsp, text)

      val interpolationContextEdit = insertTextEdit(stringLitPos.start, "s")
      val dollarEdits = (stringLitPos.start to stringLitPos.end)
        .filter(i => sourceText.charAt(i) == '$' && i != expectedDollarPos)
        .map(insertTextEdit(_, "$"))
        .toList

      Some(interpolationContextEdit :: dollarEdits.toList)
    }
    else
      None
  }
}
