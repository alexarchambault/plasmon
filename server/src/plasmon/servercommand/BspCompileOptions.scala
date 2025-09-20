package plasmon.servercommand

import caseapp.Recurse
import caseapp.core.help.Help
import caseapp.core.parser.Parser

// format: off
final case class BspCompileOptions(
  @Recurse
    sharedBsp: SharedBspOptions = SharedBspOptions(),
  dumbBuildToolHacks: Boolean = false
)
// format: on

object BspCompileOptions {
  implicit lazy val parser: Parser[BspCompileOptions] = Parser.derive
  implicit lazy val help: Help[BspCompileOptions]     = Help.derive
}
