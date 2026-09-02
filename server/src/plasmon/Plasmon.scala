package plasmon

import caseapp.core.app.CommandsEntryPoint
import plasmon.command.*

object Plasmon extends CommandsEntryPoint {
  def progName = "plasmon"

  lazy val commands: Seq[caseapp.Command[?]] = {
    val all = Seq(
      Code,
      Command,
      Revert,
      Server,
      Setup,
      UseBinary,
      UseJvm,
      Version
    ) ++
      // The commands that run in the server. They parse themselves here, and only then is a
      // server needed - see RemoteCommand.
      RemoteCommands.commands(CommandOptions())
    all.sortBy(_.name)
  }
}
