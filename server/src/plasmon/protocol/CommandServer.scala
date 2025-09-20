package plasmon.protocol

import org.eclipse.lsp4j.jsonrpc.services.JsonRequest

import java.util.concurrent.CompletableFuture

trait CommandServer {
  @JsonRequest("runCommand")
  def runCommand(params: Command): CompletableFuture[CommandResult]

  protected var client: CommandClient = null
  def setClient(client: CommandClient): Unit =
    this.client = client
}
