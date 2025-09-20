package plasmon.servercommand

import caseapp.core.RemainingArgs
import plasmon.command.ServerCommandThreadPools
import plasmon.protocol.CommandClient
import plasmon.index.Indexer
import plasmon.Server

import scala.concurrent.Await
import scala.concurrent.duration.Duration

final case class Index(
  server: Server,
  indexer: Indexer,
  client: CommandClient
) extends ServerCommandInstance[IndexOptions](client) {
  def run(options: IndexOptions, args: RemainingArgs): Unit = {
    if (options.await)
      while (indexer.actor.awaitingMessages.nonEmpty)
        Thread.sleep(100L)
    else
      Await.result(indexer.reIndex(), Duration.Inf)
  }
}

object Index extends ServerCommand[IndexOptions] {
  def instance(
    server: Server,
    indexer: Indexer,
    client: CommandClient,
    lspServer: plasmon.jsonrpc.JsonrpcServer,
    pool: ServerCommandThreadPools
  ): ServerCommandInstance[IndexOptions] =
    Index(server, indexer, client)
}
