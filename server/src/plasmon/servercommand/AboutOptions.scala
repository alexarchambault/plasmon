package plasmon.servercommand

import caseapp.core.help.Help
import caseapp.core.parser.Parser

final case class AboutOptions()

object AboutOptions {
  implicit lazy val parser: Parser[AboutOptions] = Parser.derive
  implicit lazy val help: Help[AboutOptions]     = Help.derive
}
