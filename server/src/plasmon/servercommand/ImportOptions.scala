package plasmon.servercommand

import caseapp.core.help.Help
import caseapp.core.parser.Parser

final case class ImportOptions(
  target: List[String] = Nil,
  connection: Option[String] = None,
  toplevelCacheOnly: Boolean = false,
  ignoreToplevelSymbolsErrors: Boolean = true,
  keep: Boolean = false
)

object ImportOptions {
  implicit lazy val parser: Parser[ImportOptions] = Parser.derive
  implicit lazy val help: Help[ImportOptions]     = Help.derive
}
