package plasmon

import caseapp.core.{Indexed, RemainingArgs}
import caseapp.core.app.CommandsEntryPoint
import caseapp.core.help.Help
import caseapp.core.parser.Parser
import plasmon.command.*

object Plasmon extends CommandsEntryPoint {
  def progName = "plasmon"

  private final case class NoArgs()
  private object NoArgs {
    implicit lazy val parser: Parser[NoArgs] = Parser.derive
    implicit lazy val help: Help[NoArgs]     = Help.derive
  }

  private def remoteCommands: Seq[caseapp.Command[?]] =
    Server.remoteCommands.map { command =>
      val commandNames = command.names
      new caseapp.Command[NoArgs]()(Parser[NoArgs], command.help.as[NoArgs]) {
        override def hasHelp: Boolean        = false
        override def hasFullHelp: Boolean    = false
        override def names                   = commandNames
        override def stopAtFirstUnrecognized = true
        def run(options: NoArgs, remainingArgs: RemainingArgs) =
          plasmon.command.Command.run(
            CommandOptions(),
            remainingArgs.copy(
              indexedRemaining =
                commandNames
                  .head
                  .zipWithIndex
                  .map { case (s, idx) => Indexed(idx, 1, s) } ++
                  remainingArgs.indexedRemaining
            )
          )
      }
    }

  lazy val commands: Seq[caseapp.Command[?]] = {
    val all = Seq(
      Code,
      Command,
      Revert,
      Server,
      Setup,
      UseBinary,
      UseJvm
    ) ++
      remoteCommands
    all.sortBy(_.name)
  }

  private implicit class HelpOps[T](private val help: Help[T]) extends AnyVal {
    def as[U]: Help[U] =
      help.asInstanceOf[Help[U]]
  }
}
