package plasmon.command

import caseapp.core.app.CommandsEntryPoint
import plasmon.servercommand.ServerCommand

/** The commands that run in the server, as the client offers them.
  *
  * The same list [[Server.remoteCommands]] gives the server, one [[RemoteCommand]] per entry, so
  * that a command the server can run is a command the client can parse - and the other way round.
  */
object RemoteCommands {

  def commands(connection: CommandOptions): Seq[caseapp.Command[?]] =
    Server.remoteCommands.map(remoteCommand(_, connection))

  /** Its own method so that the type each [[ServerCommand]] is parameterized on is named. */
  private def remoteCommand[T](
    serverCommand: ServerCommand[T],
    connection: CommandOptions
  ): RemoteCommand[T] =
    new RemoteCommand(serverCommand, connection)

  /** Dispatches to them by name, the way the top-level `plasmon` entry point does. */
  def entryPoint(connection: CommandOptions): CommandsEntryPoint =
    new CommandsEntryPoint {
      def progName                          = "plasmon"
      def commands: Seq[caseapp.Command[?]] = RemoteCommands.commands(connection)
    }
}
