package plasmon.servercommand

import caseapp.core.help.Help
import caseapp.core.parser.Parser
import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker

final case class RefreshStatusOptions()

object RefreshStatusOptions {
  implicit lazy val parser: Parser[RefreshStatusOptions]        = Parser.derive
  implicit lazy val help: Help[RefreshStatusOptions]            = Help.derive
  implicit lazy val codec: JsonValueCodec[RefreshStatusOptions] = JsonCodecMaker.make
}
