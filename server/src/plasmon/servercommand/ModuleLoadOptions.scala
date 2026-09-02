package plasmon.servercommand

import caseapp.HelpMessage
import caseapp.core.help.Help
import caseapp.core.parser.Parser
import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker

// format: off
@HelpMessage("Load the module a source file belongs to")
final case class ModuleLoadOptions(
  @HelpMessage("Load every module the file belongs to, rather than the recommended one only")
    all: Boolean = false,
  @HelpMessage("URI of the file to load the module of, instead of passing it as an argument")
    uri: Option[String] = None
)
// format: on

object ModuleLoadOptions {
  implicit lazy val parser: Parser[ModuleLoadOptions]        = Parser.derive
  implicit lazy val help: Help[ModuleLoadOptions]            = Help.derive
  implicit lazy val codec: JsonValueCodec[ModuleLoadOptions] = JsonCodecMaker.make
}
