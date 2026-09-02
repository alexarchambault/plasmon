package plasmon.servercommand

import caseapp.core.help.Help
import caseapp.core.parser.Parser
import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker

final case class ExitOptions()

object ExitOptions {
  implicit lazy val parser: Parser[ExitOptions]        = Parser.derive
  implicit lazy val help: Help[ExitOptions]            = Help.derive
  implicit lazy val codec: JsonValueCodec[ExitOptions] = JsonCodecMaker.make
}
