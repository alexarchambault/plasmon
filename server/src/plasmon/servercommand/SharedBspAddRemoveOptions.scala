package plasmon.servercommand

import caseapp.core.help.Help
import caseapp.core.parser.Parser
import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker

final case class SharedBspAddRemoveOptions(
  mill: Option[Boolean] = None,
  millViaBloop: Option[Boolean] = None,
  bloop: Option[Boolean] = None,
  scalaCli: Option[Boolean] = None,
  bspFile: Option[String] = None
)

object SharedBspAddRemoveOptions {
  implicit lazy val parser: Parser[SharedBspAddRemoveOptions]        = Parser.derive
  implicit lazy val help: Help[SharedBspAddRemoveOptions]            = Help.derive
  implicit lazy val codec: JsonValueCodec[SharedBspAddRemoveOptions] = JsonCodecMaker.make
}
