package plasmon.servercommand

import caseapp.core.help.Help
import caseapp.core.parser.Parser

final case class SharedBspOptions(
  require: List[String] = Nil,
  target: List[String] = Nil,
  workspace: Option[String] = None
)

object SharedBspOptions {
  implicit lazy val parser: Parser[SharedBspOptions] = Parser.derive
  implicit lazy val help: Help[SharedBspOptions]     = Help.derive
}
