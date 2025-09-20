package plasmon.servercommand

import caseapp.Name
import caseapp.core.help.Help
import caseapp.core.parser.Parser

final case class InspectOptions(
  @Name("v")
  verbose: Boolean = false,
  workspace: Option[String] = None,
  list: Boolean = false,
  listUseShortIds: Boolean = true,
  diagnostics: Boolean = false,
  @Name("readonly")
  readOnly: Boolean = false,
  compilations: Boolean = false
)

object InspectOptions {
  implicit lazy val parser: Parser[InspectOptions] = Parser.derive
  implicit lazy val help: Help[InspectOptions]     = Help.derive
}
