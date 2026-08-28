package plasmon.servercommand

import caseapp.HelpMessage
import caseapp.core.help.Help
import caseapp.core.parser.Parser

// format: off
@HelpMessage("Tell the server the editor's copy of a source file changed")
final case class LspDidChangeOptions(
  @HelpMessage("URI of the file that changed, instead of passing it as an argument")
    uri: Option[String] = None,
  @HelpMessage("File holding the editor's new copy of the content (default: the file itself)")
    contentFile: Option[String] = None,
  @HelpMessage("Carry out the edits the server asks for, as an editor would")
    applyEdits: Boolean = true,
  @HelpMessage("Print the edits the server asks for as JSON")
    json: Boolean = false
)
// format: on

object LspDidChangeOptions {
  implicit lazy val parser: Parser[LspDidChangeOptions] = Parser.derive
  implicit lazy val help: Help[LspDidChangeOptions]     = Help.derive
}
