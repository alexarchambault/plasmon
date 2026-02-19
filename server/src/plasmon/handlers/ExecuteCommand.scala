package plasmon.handlers

import org.eclipse.{lsp4j => l}
import plasmon.jsonrpc.Handlers
import plasmon.Server
import plasmon.jsonrpc.{CommandHandler, RequestHandler}

import java.util.concurrent.CompletableFuture

import scala.jdk.CollectionConverters.*
import plasmon.languageclient.PlasmonLanguageClient
import java.util.UUID

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

        def progress(f: CompletableFuture[Object]): CompletableFuture[Object] = {
          val reqId    = UUID.randomUUID().toString
          val reqName0 = params.getCommand.stripPrefix("plasmon/")
          server.languageClient.progress(
            PlasmonLanguageClient.ProgressDetails(
              "command",
              "Running command",
              reqId,
              reqName0,
              done = false
            )
          )
          def done(): Unit =
            server.languageClient.progress(
              PlasmonLanguageClient.ProgressDetails(
                "command",
                "Running command",
                reqId,
                reqName0,
                done = true
              )
            )
          val f0 =
            try f
            catch {
              case t: Throwable =>
                done()
                throw t
            }
          f0.whenComplete { (_, _) =>
            try done()
            catch {
              case t: Throwable =>
                scribe.warn("Error sending progress done notification", t)
            }
          }
          f0
        }

        map.get(params.getCommand) match {
          case Some(handler) =>
            logger.log("Found handler")
            if (handler.refreshStatus)
              server.refreshStatus()
            try progress(handler.call(params, logger))
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
