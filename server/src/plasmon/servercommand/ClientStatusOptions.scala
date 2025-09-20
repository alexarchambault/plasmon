package plasmon.servercommand

import caseapp.ValueDescription
import caseapp.core.help.Help
import caseapp.core.parser.Parser

final case class ClientStatusOptions(
  @ValueDescription("info|warn|error")
  level: Option[String] = None,
  error: Option[Boolean] = None,
  warn: Option[Boolean] = None,
  show: Option[Boolean] = None,
  hide: Option[Boolean] = None,
  tooltip: Option[String] = None,
  markdownTooltip: Option[String] = None,
  command: Option[String] = None,
  commandTooltip: Option[String] = None,
  markdownCommandTooltip: Option[String] = None,
  @ValueDescription("metals|bsp|\"\"")
  statusType: Option[String] = None,
  direct: Boolean = true
)

object ClientStatusOptions {
  implicit lazy val parser: Parser[ClientStatusOptions] = Parser.derive
  implicit lazy val help: Help[ClientStatusOptions]     = Help.derive
}
