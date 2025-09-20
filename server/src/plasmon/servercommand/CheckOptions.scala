package plasmon.servercommand

import caseapp.core.help.Help
import caseapp.core.parser.Parser
import caseapp.{HelpMessage, Name}

@HelpMessage("Run health checks on a source file")
final case class CheckOptions(
  require: List[String] = Nil,
  target: Option[String] = None,
  fullTree: Boolean = false,
  verbose: Boolean = false,
  metap: Boolean = false,
  @Name("metapInteractive")
  interactiveMetap: Boolean = false
)

object CheckOptions {
  implicit lazy val parser: Parser[CheckOptions] = Parser.derive
  implicit lazy val help: Help[CheckOptions]     = Help.derive
}
