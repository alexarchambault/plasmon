package plasmon.command

import caseapp.{ArgsName, HelpMessage}
import caseapp.core.help.Help
import caseapp.core.parser.Parser

// format: off
@ArgsName("directory] [nvim arguments…")
@HelpMessage("Run Neovim, with Plasmon, on the current directory or the one passed, isolated from your own Neovim set-up")
final case class NvimOptions(
  @HelpMessage("Neovim to run: 'system' (the nvim on PATH), the path of an nvim binary, or a version to download (default: the version Plasmon was tested with, downloaded)")
    neovim: Option[String] = None,
  @HelpMessage("Run Neovim with LazyVim (default: true). With --lazyvim=false, Neovim runs with a minimal config, with no other plugin than Plasmon's")
    lazyvim: Boolean = true,
  @HelpMessage("Have the Plasmon server start in automatic mode, where build tools and modules get loaded for the files you open, rather than picked by hand (--auto=false: start in manual mode). Applies to the first server Neovim starts for a workspace, switch modes with :Plasmon mode afterwards (default: the mode the workspace was last left in, else manual)")
    auto: Option[Boolean] = None
)
// format: on

object NvimOptions {
  implicit lazy val parser: Parser[NvimOptions] = Parser.derive
  implicit lazy val help: Help[NvimOptions]     = Help.derive
}
