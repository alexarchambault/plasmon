package plasmon.servercommand

import caseapp.{HelpMessage, Name}
import caseapp.core.help.Help
import caseapp.core.parser.Parser
import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker

// format: off
@HelpMessage("Compile a source file with the presentation compiler, and print its diagnostics\n\nNothing goes through the build tool: these are the diagnostics of the presentation compiler alone. --json prints them as LSP diagnostics, with zero-based positions.")
final case class LspDiagnosticsOptions(
  @HelpMessage("URI of the file to look at, instead of passing it as an argument")
    uri: Option[String] = None,
  @HelpMessage("Load whatever the file needs before answering: start a server if none is running, then load a build tool and a module for the file if it has none")
    auto: Boolean = false,
  @HelpMessage("Don't print what is being loaded, nor the output of a build tool being started, nor the logs of the request (printed on stderr otherwise)")
  @Name("q")
    quiet: Boolean = false,
  @HelpMessage("Print the diagnostics as JSON, as LSP diagnostics")
    json: Boolean = false
) extends HasAutoOption
// format: on

object LspDiagnosticsOptions {
  implicit lazy val parser: Parser[LspDiagnosticsOptions]        = Parser.derive
  implicit lazy val help: Help[LspDiagnosticsOptions]            = Help.derive
  implicit lazy val codec: JsonValueCodec[LspDiagnosticsOptions] = JsonCodecMaker.make
}
