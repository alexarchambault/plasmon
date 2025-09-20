package plasmon.command

import caseapp.core.help.Help
import caseapp.core.parser.Parser
import caseapp.{@@, Counter, Name, Tag}

// format: off
final case class SetupOptions(
  @Name("v")
    verbose: Int @@ Counter = Tag.of(0)
) {
  // format: on
  lazy val verbosity = Tag.unwrap(verbose)
}

object SetupOptions {
  implicit lazy val parser: Parser[SetupOptions] = Parser.derive
  implicit lazy val help: Help[SetupOptions]     = Help.derive
}
