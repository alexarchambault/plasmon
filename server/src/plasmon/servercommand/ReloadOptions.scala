package plasmon.servercommand

import caseapp.core.help.Help
import caseapp.core.parser.Parser

final case class ReloadOptions()

object ReloadOptions {
  implicit lazy val parser: Parser[ReloadOptions] = Parser.derive
  implicit lazy val help: Help[ReloadOptions]     = Help.derive
}
