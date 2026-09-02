package plasmon.servercommand

import caseapp.core.help.Help
import caseapp.core.parser.Parser
import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker

final case class BspRequestOptions(
  path: Option[String] = None,
  buildServer: Option[String] = None
)

object BspRequestOptions {
  implicit lazy val parser: Parser[BspRequestOptions]        = Parser.derive
  implicit lazy val help: Help[BspRequestOptions]            = Help.derive
  implicit lazy val codec: JsonValueCodec[BspRequestOptions] = JsonCodecMaker.make
}
