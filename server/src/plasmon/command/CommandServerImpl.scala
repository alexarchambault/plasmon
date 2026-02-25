package plasmon.command

import caseapp.core.app.CommandsEntryPoint
import plasmon.protocol
import plasmon.protocol.{CommandClient, CommandServer}
import plasmon.servercommand.ServerCommandInstance

import java.util.concurrent.{CompletableFuture, ExecutorService}

class CommandServerImpl(
  remoteCommands: CommandClient => Seq[ServerCommandInstance[?]],
  pool: ExecutorService
) extends CommandServer {

  val entryPoint: CommandsEntryPoint = new CommandsEntryPoint {
    def progName = "plasmon"
    def commands: Seq[caseapp.Command[?]] =
      remoteCommands(client)
    override def exit(code: Int): Nothing =
      ServerCommandInstance.exit(code)
  }

  def runCommand(params: protocol.Command): CompletableFuture[protocol.CommandResult] =
    CompletableFuture.supplyAsync(
      () => {
        scribe.info(s"Run command $params")
        val res = new protocol.CommandResult
        try entryPoint.main(params.getArgs)
        catch {
          case e: ServerCommandInstance.Exit =>
            res.setExitCode(e.code)
          case e: Throwable =>
            scribe.warn(
              s"Caught exception when running command ${Option(params).fold("[no params]")(_.getArgs.mkString(" "))}",
              e
            )
            res.setExitCode(1)
        }
        res
      },
      pool
    )
}
