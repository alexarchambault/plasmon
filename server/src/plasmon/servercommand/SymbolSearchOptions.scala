package plasmon.servercommand

import caseapp.core.help.Help
import caseapp.core.parser.Parser
import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker

final case class SymbolSearchOptions(
  color: Boolean = true
)

object SymbolSearchOptions {
  implicit lazy val parser: Parser[SymbolSearchOptions]        = Parser.derive
  implicit lazy val help: Help[SymbolSearchOptions]            = Help.derive
  implicit lazy val codec: JsonValueCodec[SymbolSearchOptions] = JsonCodecMaker.make
}
