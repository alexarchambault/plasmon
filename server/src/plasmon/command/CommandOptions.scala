package plasmon.command

import caseapp.{@@, Counter, Name, Tag}
import caseapp.core.help.Help
import caseapp.core.parser.Parser

// format: off
final case class CommandOptions(
  socket: Option[String] = None,
  workingDir: Option[String] = None,
  @Name("v")
    verbose: Int @@ Counter = Tag.of(0),
  logJsonrpcInput: Option[Boolean] = None
) {
  // format: on
  lazy val verbosity = Tag.unwrap(verbose)
}

object CommandOptions {
  implicit lazy val parser: Parser[CommandOptions] = Parser.derive
  implicit lazy val help: Help[CommandOptions]     = Help.derive
}
