package plasmon.servercommand

import caseapp.Recurse
import caseapp.core.help.Help
import caseapp.core.parser.Parser

// format: off
final case class BuildToolAddOptions(
  @Recurse
    shared: SharedBspAddRemoveOptions = SharedBspAddRemoveOptions(),
  sbt: Option[Boolean] = None
)
// format: on

object BuildToolAddOptions {
  implicit lazy val parser: Parser[BuildToolAddOptions] = Parser.derive
  implicit lazy val help: Help[BuildToolAddOptions]     = Help.derive
}
