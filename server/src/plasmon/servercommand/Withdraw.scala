package plasmon.servercommand

import caseapp.core.RemainingArgs
import plasmon.Server
import plasmon.index.Indexer
import plasmon.protocol.CommandClient

final case class Withdraw(
  server: Server,
  indexer: Indexer,
  client: CommandClient
) extends ServerCommandInstance[WithdrawOptions](client) {
  def run(options: WithdrawOptions, args: RemainingArgs): Unit = {
    val toWithdraw    = args.all.map(BspUtil.targetFullId(server.workingDir, _))
    val toWithdrawSet = toWithdraw.toSet
    val indexed       = indexer.cachedIndexed()
    val remove = indexed.map {
      case (info, targets) =>
        targets.filter(toWithdrawSet).map(info -> _)
    }
    val remove0 = remove.flatten

    val notFound = toWithdraw.filterNot(remove0.map(_._2).toSet)
    if (notFound.nonEmpty)
      if (options.strict) {
        for (notFoundTarget <- notFound)
          printLine(s"Target $notFoundTarget not found", toStderr = true)
        exit(1)
      }
      else
        for (notFoundTarget <- notFound)
          printLine(s"Warning: target $notFoundTarget not found", toStderr = true)

    if (remove0.isEmpty)
      if (options.strict) {
        printLine("Nothing to remove")
        exit(1)
      }
      else
        printLine("Warning: nothing to remove")

    for ((info, id) <- remove0)
      indexer.removeTarget(info, id)

    indexer.index(
      toplevelCacheOnly = indexer.cachedToplevelCacheOnly(),
      ignoreToplevelSymbolsErrors = indexer.cachedIgnoreToplevelSymbolsErrors(),
      mayReadFromBspCache = false
    )
  }
}

object Withdraw extends ServerCommand[WithdrawOptions] {
  def instance(
    server: Server,
    indexer: Indexer,
    client: CommandClient,
    lspServer: plasmon.jsonrpc.JsonrpcServer,
    pool: plasmon.command.ServerCommandThreadPools
  ): ServerCommandInstance[WithdrawOptions] =
    Withdraw(server, indexer, client)
}
