package plasmon.servercommand

import caseapp.core.RemainingArgs
import plasmon.protocol.CommandClient
import plasmon.index.Indexer
import plasmon.Server

final case class BspList(
  server: Server,
  client: CommandClient
) extends ServerCommandInstance[BspListOptions](client) {
  override def names = List(
    List("bsp", "list"),
    List("bsp-list")
  )
  def run(options: BspListOptions, args: RemainingArgs): Unit =
    for (conn <- server.bspServers.list.flatMap(_._2))
      printLine(s"${conn.name}: ${conn.info}")
}

object BspList extends ServerCommand[BspListOptions] {
  def instance(
    server: Server,
    indexer: Indexer,
    client: CommandClient,
    lspServer: plasmon.jsonrpc.JsonrpcServer,
    pool: plasmon.command.ServerCommandThreadPools
  ): ServerCommandInstance[BspListOptions] =
    BspList(server, client)
  override def names = List(
    List("bsp", "list"),
    List("bsp-list")
  )
}
