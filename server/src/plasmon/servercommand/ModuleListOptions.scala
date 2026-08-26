package plasmon.servercommand

import caseapp.HelpMessage
import caseapp.core.help.Help
import caseapp.core.parser.Parser

// format: off
@HelpMessage("List the modules a source file belongs to and could be loaded from")
final case class ModuleListOptions(
  @HelpMessage("URI of the file to list the modules of, instead of passing it as an argument")
    uri: Option[String] = None,
  @HelpMessage("Print the modules as JSON")
    json: Boolean = false
)
// format: on

object ModuleListOptions {
  implicit lazy val parser: Parser[ModuleListOptions] = Parser.derive
  implicit lazy val help: Help[ModuleListOptions]     = Help.derive
}
