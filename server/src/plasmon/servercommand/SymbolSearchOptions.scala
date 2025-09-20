package plasmon.servercommand

import caseapp.core.help.Help
import caseapp.core.parser.Parser

final case class SymbolSearchOptions(
  color: Boolean = true
)

object SymbolSearchOptions {
  implicit lazy val parser: Parser[SymbolSearchOptions] = Parser.derive
  implicit lazy val help: Help[SymbolSearchOptions]     = Help.derive
}
