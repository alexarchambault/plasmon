package plasmon.servercommand

import caseapp.core.help.Help
import caseapp.core.parser.Parser

final case class BspListOptions()

object BspListOptions {
  implicit lazy val parser: Parser[BspListOptions] = Parser.derive
  implicit lazy val help: Help[BspListOptions]     = Help.derive
}
