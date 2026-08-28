package plasmon.servercommand

import caseapp.HelpMessage
import caseapp.core.help.Help
import caseapp.core.parser.Parser

// format: off
@HelpMessage("Tell the server a source file was opened in the editor")
final case class LspDidOpenOptions(
  @HelpMessage("URI of the file that was opened, instead of passing it as an argument")
    uri: Option[String] = None,
  @HelpMessage("File holding the editor's copy of the content, if it differs from the file on disk")
    contentFile: Option[String] = None,
  @HelpMessage("Version of the editor's copy of the content")
    version: Int = 0,
  @HelpMessage("Carry out the edits the server asks for, as an editor would")
    applyEdits: Boolean = true,
  @HelpMessage("Print the edits the server asks for as JSON")
    json: Boolean = false
)
// format: on

object LspDidOpenOptions {
  implicit lazy val parser: Parser[LspDidOpenOptions] = Parser.derive
  implicit lazy val help: Help[LspDidOpenOptions]     = Help.derive
}
