package plasmon.servercommand

import caseapp.HelpMessage
import caseapp.core.help.Help
import caseapp.core.parser.Parser

final case class WithdrawOptions(
  @HelpMessage(
    "Ensure all passed targets were previously loaded and that some targets are passed on the command-line"
  )
  strict: Boolean = false
)

object WithdrawOptions {
  implicit lazy val parser: Parser[WithdrawOptions] = Parser.derive
  implicit lazy val help: Help[WithdrawOptions]     = Help.derive
}
