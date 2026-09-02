package plasmon.servercommand

import caseapp.core.help.Help
import caseapp.core.parser.Parser
import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker

final case class IndexOptions(
  await: Boolean = false
)

object IndexOptions {
  implicit lazy val parser: Parser[IndexOptions]        = Parser.derive
  implicit lazy val help: Help[IndexOptions]            = Help.derive
  implicit lazy val codec: JsonValueCodec[IndexOptions] = JsonCodecMaker.make
}
