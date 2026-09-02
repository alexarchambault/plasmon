package plasmon.servercommand

import caseapp.HelpMessage
import caseapp.core.help.Help
import caseapp.core.parser.Parser
import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker

// format: off
@HelpMessage("Compile the modules the given source files belong to")
final case class CompileOptions(
  @HelpMessage("URI of a file to compile the module of, instead of passing files as arguments")
    uri: List[String] = Nil,
  @HelpMessage("Exit with a non-zero code if compilation fails")
    failOnError: Boolean = true
)
// format: on

object CompileOptions {
  implicit lazy val parser: Parser[CompileOptions]        = Parser.derive
  implicit lazy val help: Help[CompileOptions]            = Help.derive
  implicit lazy val codec: JsonValueCodec[CompileOptions] = JsonCodecMaker.make
}
