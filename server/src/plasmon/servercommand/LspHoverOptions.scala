package plasmon.servercommand

import caseapp.HelpMessage
import caseapp.core.help.Help
import caseapp.core.parser.Parser

// format: off
@HelpMessage("Hover at a position in a source file")
final case class LspHoverOptions(
  @HelpMessage("URI of the file to hover in, instead of passing it as an argument")
    uri: Option[String] = None,
  @HelpMessage("Line to hover at, zero-based")
    line: Int,
  @HelpMessage("Column to hover at, zero-based")
    col: Int,
  @HelpMessage("Load whatever the file needs before answering: start a server if none is running, then load a build tool and a module for the file if it has none")
    auto: Boolean = false,
  @HelpMessage("Print the raw LSP response as JSON")
    json: Boolean = false
)
// format: on

object LspHoverOptions {
  implicit lazy val parser: Parser[LspHoverOptions] = Parser.derive
  implicit lazy val help: Help[LspHoverOptions]     = Help.derive
}
