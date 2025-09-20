package plasmon.servercommand

import caseapp.core.help.Help
import caseapp.core.parser.Parser

final case class TemplateOptions()

object TemplateOptions {
  implicit lazy val parser: Parser[TemplateOptions] = Parser.derive
  implicit lazy val help: Help[TemplateOptions]     = Help.derive
}
