package plasmon.semdb

import scala.util.matching.Regex

// Based on https://github.com/scalameta/metals/blob/e071d44324016086e32bfafde3d2da448c9e244c/mtags/src/main/scala/scala/meta/internal/mtags/Semanticdbs.scala#L154-L178

object Shebang {
  val shebang                     = "#!"
  private val sheBangRegex: Regex = s"""(^(#!.*(\\r\\n?|\\n)?)+(\\s*!#.*)?)""".r

  /** This function adjusts file content changing all not-newline characters in the shebang header
    * into spaces. This is the same as done in the Scala 3 compiler, so for the same input, m5d from
    * semanticdb and the one calculated from adjusted content will match.
    */
  def adjustContent(content: String): String = {
    val regexMatch = sheBangRegex.findFirstMatchIn(content)
    regexMatch match {
      case Some(firstMatch) =>
        val shebangContent = firstMatch.toString()
        val substitution =
          shebangContent.map {
            case c @ ('\n' | '\r') => c
            case _                 => ' '
          }
        substitution ++ content.drop(shebangContent.length())
      case None => content
    }
  }
}
