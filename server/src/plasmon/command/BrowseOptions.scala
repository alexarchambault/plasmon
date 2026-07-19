package plasmon.command

import caseapp.{HelpMessage, ValueDescription}
import caseapp.core.help.Help
import caseapp.core.parser.Parser

final case class BrowseOptions(
  @HelpMessage("Address to listen on")
  host: String = "127.0.0.1",
  @HelpMessage("Port to listen on")
  port: Int = 8080,
  @HelpMessage("PEM encoded TLS certificate (requires --key)")
  @ValueDescription("path")
  cert: Option[String] = None,
  @HelpMessage("PEM encoded PKCS#8 TLS private key (requires --cert)")
  @ValueDescription("path")
  key: Option[String] = None
)

object BrowseOptions {
  implicit lazy val parser: Parser[BrowseOptions] = Parser.derive
  implicit lazy val help: Help[BrowseOptions]     = Help.derive
}
