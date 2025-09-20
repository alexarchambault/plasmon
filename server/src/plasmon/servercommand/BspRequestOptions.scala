package plasmon.servercommand

import caseapp.core.help.Help
import caseapp.core.parser.Parser

final case class BspRequestOptions(
  path: Option[String] = None,
  buildServer: Option[String] = None
)

object BspRequestOptions {
  implicit lazy val parser: Parser[BspRequestOptions] = Parser.derive
  implicit lazy val help: Help[BspRequestOptions]     = Help.derive
}
