package plasmon.protocol

import org.eclipse.lsp4j.jsonrpc.services.JsonRequest

import java.util.concurrent.CompletableFuture

trait CommandClient {
  @JsonRequest("print")
  def print(data: PrintData): CompletableFuture[Unit]
}

object CommandClient {
  object ops {
    implicit class CommandClientOps(private val client: CommandClient) extends AnyVal {
      def printLine(line: String): Unit =
        client.print(new PrintData(line, false)).get()
      def printLine(line: String, toStderr: Boolean): Unit =
        client.print(new PrintData(line, toStderr)).get()
    }
  }
}
