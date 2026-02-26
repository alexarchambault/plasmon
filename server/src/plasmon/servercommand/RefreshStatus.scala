package plasmon.servercommand

import caseapp.core.RemainingArgs
import plasmon.Server
import plasmon.index.Indexer
import plasmon.protocol.CommandClient

import scala.concurrent.Await
import scala.concurrent.duration.Duration

final case class RefreshStatus(
  server: Server,
  client: CommandClient
) extends ServerCommandInstance[RefreshStatusOptions](client) {
  override def names = RefreshStatus.names
  def run(options: RefreshStatusOptions, args: RemainingArgs): Unit = {
    val res = Await.result(server.refreshStatusDetails(), Duration.Inf)
    res match {
      case None =>
        printLine("No status to refresh", toStderr = true)
      case Some((path, status)) =>
        printLine(s"Refreshed status for $path", toStderr = true)
        printLine(pprint.apply(status).toString, toStderr = true)
    }
  }
}

object RefreshStatus extends ServerCommand[RefreshStatusOptions] {
  override def names = List(
    List("refresh-status"),
    List("refresh", "status")
  )
  def instance(
    server: Server,
    indexer: Indexer,
    client: CommandClient,
    lspServer: plasmon.jsonrpc.JsonrpcServer,
    pool: plasmon.command.ServerCommandThreadPools
  ): ServerCommandInstance[RefreshStatusOptions] =
    RefreshStatus(server, client)
}
