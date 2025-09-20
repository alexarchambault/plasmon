package plasmon.servercommand

import caseapp.core.help.Help
import caseapp.core.parser.Parser

final case class ExitOptions()

object ExitOptions {
  implicit lazy val parser: Parser[ExitOptions] = Parser.derive
  implicit lazy val help: Help[ExitOptions]     = Help.derive
}
