package plasmon.servercommand

import caseapp.{HelpMessage, Name}
import caseapp.core.help.Help
import caseapp.core.parser.Parser
import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker

// format: off
@HelpMessage("Load a build tool, discovering it the way the editor extension does")
final case class BuildToolLoadOptions(
  @HelpMessage("Id of the build tool to load, such as mill, sbt, scala-cli or bloop (default: the only discovered one)")
  @Name("id")
    toolId: Option[String] = None,
  @HelpMessage("Id to discover the build tool under, if it differs from the build tool id")
    discoverId: Option[String] = None,
  @HelpMessage("URI of the file to discover build tools from, instead of passing it as an argument")
    uri: Option[String] = None
)
// format: on

object BuildToolLoadOptions {
  implicit lazy val parser: Parser[BuildToolLoadOptions]        = Parser.derive
  implicit lazy val help: Help[BuildToolLoadOptions]            = Help.derive
  implicit lazy val codec: JsonValueCodec[BuildToolLoadOptions] = JsonCodecMaker.make
}
