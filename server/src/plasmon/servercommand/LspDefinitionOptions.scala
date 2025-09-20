package plasmon.servercommand

import caseapp.core.help.Help
import caseapp.core.parser.Parser

final case class LspDefinitionOptions(
  uri: Option[String] = None,
  line: Int,
  col: Int
)

object LspDefinitionOptions {
  implicit lazy val parser: Parser[LspDefinitionOptions] = Parser.derive
  implicit lazy val help: Help[LspDefinitionOptions]     = Help.derive
}
