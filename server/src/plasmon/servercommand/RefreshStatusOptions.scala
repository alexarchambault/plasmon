package plasmon.servercommand

import caseapp.core.help.Help
import caseapp.core.parser.Parser

final case class RefreshStatusOptions()

object RefreshStatusOptions {
  implicit lazy val parser: Parser[RefreshStatusOptions] = Parser.derive
  implicit lazy val help: Help[RefreshStatusOptions]     = Help.derive
}
