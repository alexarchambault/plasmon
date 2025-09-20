package plasmon.servercommand

import caseapp.Name
import caseapp.core.help.Help
import caseapp.core.parser.Parser

final case class BspBuildTargetsOptions(
  require: List[String] = Nil,
  @Name("rawId")
  @Name("raw")
  rawIds: Boolean = false,
  details: Boolean = false
)

object BspBuildTargetsOptions {
  implicit lazy val parser: Parser[BspBuildTargetsOptions] = Parser.derive
  implicit lazy val help: Help[BspBuildTargetsOptions]     = Help.derive
}
