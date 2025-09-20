package plasmon.servercommand

import caseapp.core.help.Help
import caseapp.core.parser.Parser

final case class LspHoverOptions(
  uri: Option[String] = None,
  line: Int,
  col: Int
)

object LspHoverOptions {
  implicit lazy val parser: Parser[LspHoverOptions] = Parser.derive
  implicit lazy val help: Help[LspHoverOptions]     = Help.derive
}
