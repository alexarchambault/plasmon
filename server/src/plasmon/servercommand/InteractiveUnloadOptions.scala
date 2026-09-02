package plasmon.servercommand

import caseapp.core.help.Help
import caseapp.core.parser.Parser
import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker

final case class InteractiveUnloadOptions(
  target: List[String] = Nil,
  main: Option[Boolean] = None,
  completion: Option[Boolean] = None
)

object InteractiveUnloadOptions {
  implicit lazy val parser: Parser[InteractiveUnloadOptions]        = Parser.derive
  implicit lazy val help: Help[InteractiveUnloadOptions]            = Help.derive
  implicit lazy val codec: JsonValueCodec[InteractiveUnloadOptions] = JsonCodecMaker.make
}
