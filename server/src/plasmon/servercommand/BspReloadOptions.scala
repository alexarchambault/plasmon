package plasmon.servercommand

import caseapp.Recurse
import caseapp.core.help.Help
import caseapp.core.parser.Parser
import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker

final case class BspReloadOptions(
  @Recurse
  shared: SharedBspAddRemoveOptions = SharedBspAddRemoveOptions(),
  strict: Boolean = false
)

object BspReloadOptions {
  implicit lazy val parser: Parser[BspReloadOptions]        = Parser.derive
  implicit lazy val help: Help[BspReloadOptions]            = Help.derive
  implicit lazy val codec: JsonValueCodec[BspReloadOptions] = JsonCodecMaker.make
}
