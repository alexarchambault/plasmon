package plasmon.servercommand

import caseapp.Recurse
import caseapp.core.help.Help
import caseapp.core.parser.Parser

// format: off
final case class BspAddOptions(
  @Recurse
    shared: SharedBspAddRemoveOptions = SharedBspAddRemoveOptions()
)
// format: on

object BspAddOptions {
  implicit lazy val parser: Parser[BspAddOptions] = Parser.derive
  implicit lazy val help: Help[BspAddOptions]     = Help.derive
}
