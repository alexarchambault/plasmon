package plasmon.servercommand

import caseapp.core.help.Help
import caseapp.core.parser.Parser

final case class FallbackCompilerOptions(
  enable: Option[Boolean] = None
)

object FallbackCompilerOptions {
  implicit lazy val parser: Parser[FallbackCompilerOptions] = Parser.derive
  implicit lazy val help: Help[FallbackCompilerOptions]     = Help.derive
}
