package plasmon.command

import caseapp.Name
import caseapp.core.help.Help
import caseapp.core.parser.Parser

final case class VersionOptions(
  @Name("commit")
  commitHash: Boolean = false
)

object VersionOptions {
  implicit lazy val parser: Parser[VersionOptions] = Parser.derive
  implicit lazy val help: Help[VersionOptions]     = Help.derive
}
