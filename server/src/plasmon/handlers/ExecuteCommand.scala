package plasmon.handlers

import org.eclipse.{lsp4j => l}
import plasmon.jsonrpc.Handlers
import plasmon.Server
import plasmon.jsonrpc.{CommandHandler, RequestHandler}

import java.util.concurrent.CompletableFuture

import scala.jdk.CollectionConverters.*

object ExecuteCommand {

  def handler(server: Server, commandHandlers: Seq[CommandHandler]) = {
    val map = commandHandlers
      .groupBy(_.commandName)
      .map {
        case (k, Seq(v)) => k -> v
        case (k, other) =>
          scribe.error(s"Ignoring ${other.length - 1} handler(s) for command $k")
          k -> other.head
      }
    RequestHandler.of[l.ExecuteCommandParams, Object]("workspace/executeCommand") {
      (params, logger) =>
        val argsOpt    = Option(params.getArguments).map(_.asScala.toVector)
        val commandStr = params.getCommand + argsOpt.fold("")(_.mkString("(", ", ", ")"))
        logger.log(s"Running command $commandStr")
        map.get(params.getCommand) match {
          case Some(handler) =>
            logger.log("Found handler")
            if (handler.refreshStatus)
              server.refreshStatus()
            try handler.call(params, logger)
            finally
              if (handler.refreshStatus)
                server.refreshStatus()
          case None =>
            logger.log("No handler for command")
            // FIXME Trapped error… (for users)
            CompletableFuture.completedFuture(null)
        }
    }
  }

  def handlers(server: Server, commandHandlers: Seq[CommandHandler]): Handlers =
    Handlers(Nil, Seq(handler(server, commandHandlers)), Nil)
}
