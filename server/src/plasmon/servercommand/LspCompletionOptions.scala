package plasmon.servercommand

import caseapp.HelpMessage
import caseapp.core.help.Help
import caseapp.core.parser.Parser

// format: off
@HelpMessage("Complete at a position in a source file")
final case class LspCompletionOptions(
  @HelpMessage("URI of the file to complete in, instead of passing it as an argument")
    uri: Option[String] = None,
  @HelpMessage("Line to complete at, zero-based")
    line: Int,
  @HelpMessage("Column to complete at, zero-based (default: the end of the line)")
    col: Option[Int] = None,
  @HelpMessage("Print the raw LSP response as JSON")
    json: Boolean = false
)
// format: on

object LspCompletionOptions {
  implicit lazy val parser: Parser[LspCompletionOptions] = Parser.derive
  implicit lazy val help: Help[LspCompletionOptions]     = Help.derive
}
