package plasmon.servercommand

import caseapp.Recurse
import caseapp.core.help.Help
import caseapp.core.parser.Parser
import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker

// format: off
final case class BspCompileOptions(
  @Recurse
    sharedBsp: SharedBspOptions = SharedBspOptions(),
  dumbBuildToolHacks: Boolean = false
)
// format: on

object BspCompileOptions {
  implicit lazy val parser: Parser[BspCompileOptions]        = Parser.derive
  implicit lazy val help: Help[BspCompileOptions]            = Help.derive
  implicit lazy val codec: JsonValueCodec[BspCompileOptions] = JsonCodecMaker.make
}
