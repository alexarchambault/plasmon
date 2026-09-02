package plasmon.servercommand

import caseapp.core.help.Help
import caseapp.core.parser.Parser
import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker

final case class AboutOptions()

object AboutOptions {
  implicit lazy val parser: Parser[AboutOptions]        = Parser.derive
  implicit lazy val help: Help[AboutOptions]            = Help.derive
  implicit lazy val codec: JsonValueCodec[AboutOptions] = JsonCodecMaker.make
}
