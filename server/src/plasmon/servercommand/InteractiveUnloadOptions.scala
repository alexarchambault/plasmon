package plasmon.servercommand

import caseapp.core.help.Help
import caseapp.core.parser.Parser

final case class InteractiveUnloadOptions(
  target: List[String] = Nil,
  main: Option[Boolean] = None,
  completion: Option[Boolean] = None
)

object InteractiveUnloadOptions {
  implicit lazy val parser: Parser[InteractiveUnloadOptions] = Parser.derive
  implicit lazy val help: Help[InteractiveUnloadOptions]     = Help.derive
}
