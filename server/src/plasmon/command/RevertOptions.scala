package plasmon.command

import caseapp.{@@, Counter, Name, Tag}
import caseapp.core.help.Help
import caseapp.core.parser.Parser

final case class RevertOptions(
  @Name("v")
  verbose: Int @@ Counter = Tag.of(0)
) {
  lazy val verbosity = Tag.unwrap(verbose)
}

object RevertOptions {
  implicit lazy val parser: Parser[RevertOptions] = Parser.derive
  implicit lazy val help: Help[RevertOptions]     = Help.derive
}
