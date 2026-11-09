package plasmon.protocol

import org.eclipse.lsp4j.jsonrpc.services.JsonRequest

import java.util.concurrent.{CompletableFuture, ExecutionException}

trait CommandClient {
  @JsonRequest("print")
  def print(data: PrintData): CompletableFuture[Unit]
}

object CommandClient {
  object ops {
    implicit class CommandClientOps(private val client: CommandClient) extends AnyVal {
      def printLine(line: String): Unit =
        printLine(line, toStderr = false)
      def printLine(line: String, toStderr: Boolean): Unit =
        // Throw what failed the request (the client going away, say), rather than a wrapper
        try client.print(new PrintData(line, toStderr)).get()
        catch {
          case e: ExecutionException if e.getCause != null =>
            throw e.getCause
        }
    }
  }
}
