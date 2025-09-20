package plasmon.command

import caseapp.core.help.Help
import caseapp.core.parser.Parser

final case class CodeOptions(
  global: Boolean = false,
  codeCommand: Option[String] = None
)

object CodeOptions {
  implicit lazy val parser: Parser[CodeOptions] = Parser.derive
  implicit lazy val help: Help[CodeOptions]     = Help.derive
}
