package plasmon.servercommand

import caseapp.core.help.Help
import caseapp.core.parser.Parser
import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker

final case class DiagnosticsOptions(
  adjust: Boolean = true,
  json: Boolean = false
)

object DiagnosticsOptions {
  implicit lazy val parser: Parser[DiagnosticsOptions]        = Parser.derive
  implicit lazy val help: Help[DiagnosticsOptions]            = Help.derive
  implicit lazy val codec: JsonValueCodec[DiagnosticsOptions] = JsonCodecMaker.make
}
