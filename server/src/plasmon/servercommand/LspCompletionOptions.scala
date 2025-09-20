package plasmon.servercommand

import caseapp.core.help.Help
import caseapp.core.parser.Parser

final case class LspCompletionOptions(
  line: Int,
  col: Option[Int] = None
)

object LspCompletionOptions {
  implicit lazy val parser: Parser[LspCompletionOptions] = Parser.derive
  implicit lazy val help: Help[LspCompletionOptions]     = Help.derive
}
