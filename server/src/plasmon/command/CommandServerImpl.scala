package plasmon.command

import com.github.plokhotnyuk.jsoniter_scala.core.readFromString
import plasmon.protocol
import plasmon.protocol.{CommandClient, CommandServer, RemainingArgsJson}
import plasmon.protocol.CommandClient.ops.*
import plasmon.servercommand.ServerCommandInstance

import java.util.concurrent.{CompletableFuture, ExecutorService}

class CommandServerImpl(
  remoteCommands: CommandClient => Seq[ServerCommandInstance[?]],
  pool: ExecutorService
) extends CommandServer {

  def runCommand(params: protocol.Command): CompletableFuture[protocol.CommandResult] =
    CompletableFuture.supplyAsync(
      () => {
        val name = Option(params).flatMap(p => Option(p.getName)).map(_.toList).getOrElse(Nil)
        scribe.info(s"Run command ${name.mkString(" ")}")
        val res = new protocol.CommandResult
        try
          // The client parsed this out of the very same command list, so a name we don't know
          // means the two sides disagree on what commands there are - a client from another
          // version, most likely
          remoteCommands(client).find(_.names.contains(name)) match {
            case Some(instance) =>
              instance.runFromJson(optionsJson(params), remainingArgs(params))
            case None =>
              val message = s"No command named '${name.mkString(" ")}' in this server"
              scribe.error(message)
              client.printLine(message, toStderr = true)
              res.setExitCode(1)
          }
        catch {
          case e: ServerCommandInstance.Exit =>
            res.setExitCode(e.code)
          case e: Throwable =>
            scribe.warn(s"Caught exception when running command ${name.mkString(" ")}", e)
            res.setExitCode(1)
        }
        res
      },
      pool
    )

  private def optionsJson(params: protocol.Command): String =
    Option(params.getOptions).map(_.toString).getOrElse("{}")

  private def remainingArgs(params: protocol.Command): caseapp.core.RemainingArgs =
    Option(params.getRemainingArgs)
      .map(json => readFromString[RemainingArgsJson](json.toString))
      .getOrElse(RemainingArgsJson.empty)
      .toRemainingArgs
}
