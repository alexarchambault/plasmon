package plasmon.servercommand

import caseapp.Name
import caseapp.core.help.Help
import caseapp.core.parser.Parser

final case class SharedBspAddRemoveOptions(
  mill: Option[Boolean] = None,
  millViaBloop: Option[Boolean] = None,
  bloop: Option[Boolean] = None,
  scalaCli: Option[Boolean] = None,
  bspFile: Option[String] = None,
  @Name("onlyTargets")
  onlyTarget: List[String] = Nil
)

object SharedBspAddRemoveOptions {
  implicit lazy val parser: Parser[SharedBspAddRemoveOptions] = Parser.derive
  implicit lazy val help: Help[SharedBspAddRemoveOptions]     = Help.derive
}
