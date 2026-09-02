package plasmon.servercommand

import caseapp.core.help.Help
import caseapp.core.parser.Parser
import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker

final case class FallbackCompilerOptions(
  enable: Option[Boolean] = None
)

object FallbackCompilerOptions {
  implicit lazy val parser: Parser[FallbackCompilerOptions]        = Parser.derive
  implicit lazy val help: Help[FallbackCompilerOptions]            = Help.derive
  implicit lazy val codec: JsonValueCodec[FallbackCompilerOptions] = JsonCodecMaker.make
}
