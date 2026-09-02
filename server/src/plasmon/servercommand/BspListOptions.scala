package plasmon.servercommand

import caseapp.core.help.Help
import caseapp.core.parser.Parser
import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker

final case class BspListOptions()

object BspListOptions {
  implicit lazy val parser: Parser[BspListOptions]        = Parser.derive
  implicit lazy val help: Help[BspListOptions]            = Help.derive
  implicit lazy val codec: JsonValueCodec[BspListOptions] = JsonCodecMaker.make
}
