package plasmon.servercommand

import caseapp.HelpMessage
import caseapp.core.help.Help
import caseapp.core.parser.Parser

// format: off
@HelpMessage("List the code lenses of a source file")
final case class LspCodeLensOptions(
  @HelpMessage("URI of the file to look at, instead of passing it as an argument")
    uri: Option[String] = None,
  @HelpMessage("Load whatever the file needs before answering: start a server if none is running, then load a build tool and a module for the file if it has none")
    auto: Boolean = false,
  @HelpMessage("Print the raw LSP response as JSON")
    json: Boolean = false
)
// format: on

object LspCodeLensOptions {
  implicit lazy val parser: Parser[LspCodeLensOptions] = Parser.derive
  implicit lazy val help: Help[LspCodeLensOptions]     = Help.derive
}
