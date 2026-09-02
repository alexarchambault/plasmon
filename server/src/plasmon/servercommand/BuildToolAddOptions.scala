package plasmon.servercommand

import caseapp.Recurse
import caseapp.core.help.Help
import caseapp.core.parser.Parser
import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker

// format: off
final case class BuildToolAddOptions(
  @Recurse
    shared: SharedBspAddRemoveOptions = SharedBspAddRemoveOptions(),
  sbt: Option[Boolean] = None
)
// format: on

object BuildToolAddOptions {
  implicit lazy val parser: Parser[BuildToolAddOptions]        = Parser.derive
  implicit lazy val help: Help[BuildToolAddOptions]            = Help.derive
  implicit lazy val codec: JsonValueCodec[BuildToolAddOptions] = JsonCodecMaker.make
}
