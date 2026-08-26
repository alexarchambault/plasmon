package plasmon.servercommand

import caseapp.core.RemainingArgs
import ch.epfl.scala.bsp4j as b
import plasmon.Server
import plasmon.PlasmonEnrichments.StringThingExtensions
import plasmon.command.ServerCommandThreadPools
import plasmon.index.Indexer
import plasmon.protocol.CommandClient

import scala.concurrent.Await
import scala.concurrent.duration.Duration

/** Compiles the module a file belongs to.
  *
  * The high-level counterpart of [[BspCompile]], which needs a workspace and target ids: here the
  * file is enough, and the server works out what to build - the same [[ProjectOps.compile]] the
  * `plasmon/compile` LSP command goes through.
  */
final case class Compile(
  server: Server,
  client: CommandClient
) extends ServerCommandInstance[CompileOptions](client) {
  def run(options: CompileOptions, args: RemainingArgs): Unit = {

    val files = args.all.map(os.Path(_, server.workingDir)) ++
      options.uri.map(_.osPathFromUri)

    if (files.isEmpty) {
      printLine("No file specified", toStderr = true)
      exit(1)
    }

    var failed = false
    for (file <- files)
      Await.result(ProjectOps.compile(server, file), Duration.Inf) match {
        case None =>
          printLine(s"No build target found for $file, nothing to compile", toStderr = true)
        case Some(res) =>
          res.getStatusCode match {
            case b.StatusCode.OK =>
              printLine(s"Compiled $file", toStderr = true)
            case b.StatusCode.ERROR =>
              printLine(s"Compilation error for $file", toStderr = true)
              failed = true
            case b.StatusCode.CANCELLED =>
              printLine(s"Compilation cancelled for $file", toStderr = true)
              failed = true
          }
      }

    if (failed && options.failOnError)
      exit(1)
  }
}

object Compile extends ServerCommand[CompileOptions] {
  def instance(
    server: Server,
    indexer: Indexer,
    client: CommandClient,
    lspServer: plasmon.jsonrpc.JsonrpcServer,
    pool: ServerCommandThreadPools
  ): ServerCommandInstance[CompileOptions] =
    Compile(server, client)
}
