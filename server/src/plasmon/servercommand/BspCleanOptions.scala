package plasmon.servercommand

import caseapp.Recurse
import caseapp.core.help.Help
import caseapp.core.parser.Parser

final case class BspCleanOptions(
  @Recurse
  sharedBsp: SharedBspOptions = SharedBspOptions(),
  workspace: Option[String] = None
)

object BspCleanOptions {
  implicit lazy val parser: Parser[BspCleanOptions] = Parser.derive
  implicit lazy val help: Help[BspCleanOptions]     = Help.derive
}
