package plasmon.servercommand

import caseapp.core.RemainingArgs
import com.google.gson.Gson
import plasmon.protocol.CommandClient
import plasmon.util.PrintDiagnostic
import plasmon.index.Indexer
import plasmon.Server

import scala.jdk.CollectionConverters.*

final case class Diagnostics(
  server: Server,
  client: CommandClient
) extends ServerCommandInstance[DiagnosticsOptions](client) {
  def run(options: DiagnosticsOptions, args: RemainingArgs): Unit =
    for (strPath <- args.all) {
      val path = os.Path(strPath, server.workingDir)
      val allDiags = server.bspData.buildClients().flatMap { buildClient =>
        val diags = buildClient.diagnosticsFor(path)
        if (options.adjust)
          diags.flatMap { diag =>
            buildClient.toFreshDiagnostic(???, path, diag).toSeq
          }
        else
          diags
      }

      if (allDiags.isEmpty)
        scribe.info(s"No diagnostics for $path")
      else if (options.json) {
        val res: String = new Gson().toJson(allDiags.toList.asJava)
        printLine(res)
      }
      else
        for (diag <- allDiags)
          PrintDiagnostic.printFileDiagnostic(printLine(_), Right(path), diag, color = true)
    }
}

object Diagnostics extends ServerCommand[DiagnosticsOptions] {
  def instance(
    server: Server,
    indexer: Indexer,
    client: CommandClient,
    lspServer: plasmon.jsonrpc.JsonrpcServer,
    pool: plasmon.command.ServerCommandThreadPools
  ): ServerCommandInstance[DiagnosticsOptions] =
    Diagnostics(server, client)
}
