package plasmon.servercommand

import caseapp.HelpMessage
import caseapp.core.help.Help
import caseapp.core.parser.Parser
import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker

// format: off
@HelpMessage("Load every module of every loaded build tool")
final case class ModuleLoadAllOptions(
  @HelpMessage("Only fill the top-level symbol cache, rather than indexing the modules fully")
    toplevelCacheOnly: Boolean = false
)
// format: on

object ModuleLoadAllOptions {
  implicit lazy val parser: Parser[ModuleLoadAllOptions]        = Parser.derive
  implicit lazy val help: Help[ModuleLoadAllOptions]            = Help.derive
  implicit lazy val codec: JsonValueCodec[ModuleLoadAllOptions] = JsonCodecMaker.make
}
