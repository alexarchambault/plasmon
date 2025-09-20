package plasmon.servercommand

import caseapp.core.help.Help
import caseapp.core.parser.Parser

final case class DiagnosticsOptions(
  adjust: Boolean = true,
  json: Boolean = false
)

object DiagnosticsOptions {
  implicit lazy val parser: Parser[DiagnosticsOptions] = Parser.derive
  implicit lazy val help: Help[DiagnosticsOptions]     = Help.derive
}
