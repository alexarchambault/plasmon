package plasmon.servercommand

import caseapp.core.RemainingArgs
import plasmon.index.Indexer
import plasmon.protocol.CommandClient
import plasmon.Server
import scala.meta.internal.mtags.GlobalSymbolIndex

final case class ShowIndex(
  server: Server,
  client: CommandClient
) extends ServerCommandInstance[ShowIndexOptions](client) {
  def run(options: ShowIndexOptions, args: RemainingArgs): Unit = {
    val args0 = args.all
    if (args0.isEmpty) {
      printLine(
        "No build target specified, run bsp build-targets --raw to list available build targets",
        toStderr = true
      )
      exit(1)
    }
    else if (!options.topLevel && !options.all) {
      printLine("Either --top-level or --all must be specified", toStderr = true)
      exit(1)
    }
    else {
      val bucketsIt = args0
        .iterator
        .map(GlobalSymbolIndex.BuildTarget(_))
        .flatMap { module =>
          server
            .symbolIndex
            .dialectBuckets
            .iterator
            .filter {
              case ((_, mod), _) => mod == module
            }
        }

      for (((dialect, module), bucket) <- bucketsIt) {
        printLine(s"  ${module.asString} (dialect $dialect)")
        if (options.topLevel) {
          printLine("  Top level:")
          for (elem <- bucket.toplevels.trieMap.keysIterator.toVector.sorted)
            printLine(elem)
        }
        if (options.all) {
          printLine("  All (lazy):")
          for (elem <- bucket.definitions.trieMap.keysIterator.toVector.sorted)
            printLine(elem)
        }
        printLine("")
      }
    }
  }
}

object ShowIndex extends ServerCommand[ShowIndexOptions] {
  def instance(
    server: Server,
    indexer: Indexer,
    client: CommandClient,
    lspServer: plasmon.jsonrpc.JsonrpcServer,
    pool: plasmon.command.ServerCommandThreadPools
  ): ServerCommandInstance[ShowIndexOptions] =
    ShowIndex(server, client)
}
