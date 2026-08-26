package plasmon.servercommand

import caseapp.HelpMessage
import caseapp.core.help.Help
import caseapp.core.parser.Parser

// format: off
@HelpMessage("Get signature help at a position in a source file")
final case class LspSignatureHelpOptions(
  @HelpMessage("URI of the file to look at, instead of passing it as an argument")
    uri: Option[String] = None,
  @HelpMessage("Line to look at, zero-based")
    line: Int,
  @HelpMessage("Column to look at, zero-based")
    col: Int,
  @HelpMessage("Print the raw LSP response as JSON")
    json: Boolean = false
)
// format: on

object LspSignatureHelpOptions {
  implicit lazy val parser: Parser[LspSignatureHelpOptions] = Parser.derive
  implicit lazy val help: Help[LspSignatureHelpOptions]     = Help.derive
}
