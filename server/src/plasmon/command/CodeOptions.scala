package plasmon.command

import caseapp.HelpMessage
import caseapp.core.help.Help
import caseapp.core.parser.Parser

// format: off
final case class CodeOptions(
  global: Boolean = false,
  codeCommand: Option[String] = None,
  @HelpMessage("Have the Plasmon server start in automatic mode, where build tools and modules get loaded for the files you open, rather than picked by hand (--auto=false: start in manual mode). Applies to the first server the editor starts, switch modes from the editor afterwards (default: the mode the workspace was last left in, else manual)")
    auto: Option[Boolean] = None
)
// format: on

object CodeOptions {
  implicit lazy val parser: Parser[CodeOptions] = Parser.derive
  implicit lazy val help: Help[CodeOptions]     = Help.derive
}
