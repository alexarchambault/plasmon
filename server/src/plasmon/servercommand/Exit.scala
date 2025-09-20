package plasmon.servercommand

import caseapp.core.RemainingArgs
import plasmon.protocol.CommandClient
import plasmon.index.Indexer
import plasmon.Server

final case class Exit(
  server: Server,
  indexer: Indexer,
  client: CommandClient,
  pool: plasmon.command.ServerCommandThreadPools
) extends ServerCommandInstance[ExitOptions](client) {
  def run(options: ExitOptions, args: RemainingArgs): Unit = {
    printLine("Scheduling server exit", toStderr = true)
    val t: Thread =
      new Thread("server-exit") {
        setDaemon(true)
        override def run(): Unit = {
          // FIXME Ideally, we'd like to wait until this remote command (exit)
          // returned its result to users, so that we know the exit command
          // exited fine before we initiate a shutdown, that stops the command
          // server.
          Thread.sleep(1000L)
          scribe.info("Scheduled server shutdown per user request")
          server.shutdown().get()
          scribe.info("Scheduled server exit per user request")
          server.exit()
          scribe.info("Scheduled server exit per user request done")
        }
      }
    t.start()
  }
}

object Exit extends ServerCommand[ExitOptions] {
  def instance(
    server: Server,
    indexer: Indexer,
    client: CommandClient,
    lspServer: plasmon.jsonrpc.JsonrpcServer,
    pool: plasmon.command.ServerCommandThreadPools
  ): ServerCommandInstance[ExitOptions] =
    Exit(server, indexer, client, pool)
}
