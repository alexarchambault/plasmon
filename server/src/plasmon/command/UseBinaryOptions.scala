package plasmon.command

import caseapp.{@@, Counter, Name, Tag}
import caseapp.core.help.Help
import caseapp.core.parser.Parser

// format: off
final case class UseBinaryOptions(
  @Name("v")
    verbose: Int @@ Counter = Tag.of(0)
) {
  // format: on
  lazy val verbosity = Tag.unwrap(verbose)
}

object UseBinaryOptions {
  implicit lazy val parser: Parser[UseBinaryOptions] = Parser.derive
  implicit lazy val help: Help[UseBinaryOptions]     = Help.derive
}
