package plasmon.servercommand

import caseapp.core.help.Help
import caseapp.core.parser.Parser

final case class ShowIndexOptions(
  topLevel: Boolean = true,
  all: Boolean = false
)

object ShowIndexOptions {
  implicit lazy val parser: Parser[ShowIndexOptions] = Parser.derive
  implicit lazy val help: Help[ShowIndexOptions]     = Help.derive
}
