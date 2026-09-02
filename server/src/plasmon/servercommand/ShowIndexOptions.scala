package plasmon.servercommand

import caseapp.core.help.Help
import caseapp.core.parser.Parser
import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker

final case class ShowIndexOptions(
  topLevel: Boolean = true,
  all: Boolean = false
)

object ShowIndexOptions {
  implicit lazy val parser: Parser[ShowIndexOptions]        = Parser.derive
  implicit lazy val help: Help[ShowIndexOptions]            = Help.derive
  implicit lazy val codec: JsonValueCodec[ShowIndexOptions] = JsonCodecMaker.make
}
