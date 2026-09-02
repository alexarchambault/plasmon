package plasmon.servercommand

import caseapp.core.help.Help
import caseapp.core.parser.Parser
import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker

final case class ReloadOptions()

object ReloadOptions {
  implicit lazy val parser: Parser[ReloadOptions]        = Parser.derive
  implicit lazy val help: Help[ReloadOptions]            = Help.derive
  implicit lazy val codec: JsonValueCodec[ReloadOptions] = JsonCodecMaker.make
}
