package plasmon.servercommand

import caseapp.Recurse
import caseapp.core.help.Help
import caseapp.core.parser.Parser
import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker

final case class BspRemoveOptions(
  @Recurse
  shared: SharedBspAddRemoveOptions = SharedBspAddRemoveOptions()
)

object BspRemoveOptions {
  implicit lazy val parser: Parser[BspRemoveOptions]        = Parser.derive
  implicit lazy val help: Help[BspRemoveOptions]            = Help.derive
  implicit lazy val codec: JsonValueCodec[BspRemoveOptions] = JsonCodecMaker.make
}
