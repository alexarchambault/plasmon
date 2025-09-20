package plasmon.servercommand

import caseapp.core.help.Help
import caseapp.core.parser.Parser

final case class IndexOptions(
  await: Boolean = false
)

object IndexOptions {
  implicit lazy val parser: Parser[IndexOptions] = Parser.derive
  implicit lazy val help: Help[IndexOptions]     = Help.derive
}
