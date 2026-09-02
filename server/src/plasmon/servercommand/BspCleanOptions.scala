package plasmon.servercommand

import caseapp.Recurse
import caseapp.core.help.Help
import caseapp.core.parser.Parser
import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker

final case class BspCleanOptions(
  @Recurse
  sharedBsp: SharedBspOptions = SharedBspOptions(),
  workspace: Option[String] = None
)

object BspCleanOptions {
  implicit lazy val parser: Parser[BspCleanOptions]        = Parser.derive
  implicit lazy val help: Help[BspCleanOptions]            = Help.derive
  implicit lazy val codec: JsonValueCodec[BspCleanOptions] = JsonCodecMaker.make
}
