package plasmon.servercommand

import caseapp.core.help.Help
import caseapp.core.parser.Parser
import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker

final case class TemplateOptions()

object TemplateOptions {
  implicit lazy val parser: Parser[TemplateOptions]        = Parser.derive
  implicit lazy val help: Help[TemplateOptions]            = Help.derive
  implicit lazy val codec: JsonValueCodec[TemplateOptions] = JsonCodecMaker.make
}
