package plasmon.servercommand

import caseapp.core.help.Help
import caseapp.core.parser.Parser
import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker

final case class ImportOptions(
  connection: Option[String] = None,
  toplevelCacheOnly: Boolean = false,
  ignoreToplevelSymbolsErrors: Boolean = true,
  keep: Boolean = false
)

object ImportOptions {
  implicit lazy val parser: Parser[ImportOptions]        = Parser.derive
  implicit lazy val help: Help[ImportOptions]            = Help.derive
  implicit lazy val codec: JsonValueCodec[ImportOptions] = JsonCodecMaker.make
}
