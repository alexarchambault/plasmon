package plasmon.servercommand

import caseapp.core.RemainingArgs
import org.eclipse.lsp4j as l
import plasmon.Server
import plasmon.PlasmonEnrichments.*
import plasmon.ide.FutureCancelToken
import plasmon.index.Indexer
import plasmon.protocol.CommandClient
import plasmon.util.PrintDiagnostic

import scala.concurrent.{Await, Promise}
import scala.concurrent.duration.Duration
import scala.jdk.CollectionConverters.*

/** The diagnostics of the presentation compiler for a file.
  *
  * The file is compiled by the presentation compiler of its module, and what it reported while
  * doing so is printed. The build tool isn't involved, unlike with [[Diagnostics]], which prints
  * what the last BSP compilation reported.
  */
final case class LspDiagnostics(
  server: Server,
  indexer: Indexer,
  client: CommandClient,
  pools: plasmon.command.ServerCommandThreadPools
) extends ServerCommandInstance[LspDiagnosticsOptions](client) {
  override def names = LspDiagnostics.names
  def run(options: LspDiagnosticsOptions, args: RemainingArgs): Unit = {

    val (path, uri) = FileArg.single(args.all, options.uri, server.workingDir)

    ensureFileContent(server, path)

    if (options.auto)
      AutoLoad(server, indexer, pools, path, printLine(_, toStderr = true), options.quiet)

    val diagnostics =
      if (path.isDependencySource(server.workingDir)) {
        // Compiled on their own, sources of dependencies are mostly noise
        if (!options.quiet)
          printLine(s"$path is a dependency source, not compiling it", toStderr = true)
        Nil
      }
      else server.bspData.inverseSources(path) match {
        case None =>
          if (!options.quiet)
            printLine(s"No module found for $path", toStderr = true)
          Nil
        case Some(targetId) =>
          val token = {
            import scala.concurrent.ExecutionContext.Implicits.global
            FutureCancelToken(Promise[Boolean]().future)
          }
          server.presentationCompilers.compile(
            new l.TextDocumentIdentifier(uri),
            targetId,
            token
          ) match {
            case None =>
              if (!options.quiet)
                printLine(s"No presentation compiler for $path", toStderr = true)
              Nil
            case Some(f) =>
              Await.result(f, Duration.Inf)
              // The presentation compiler hands its diagnostics as LSP ones to the build client
              // while it compiles (see `emitDiagnostics` in PresentationCompilers) - none clears
              // the former ones. The result itself only has them as strings, and not always.
              server.bspData.buildClientOf(targetId).toSeq.flatMap(_.pcDiagnosticsFor(path))
          }
      }

    if (options.json)
      printJson(diagnostics.asJava)
    else
      for (diag <- diagnostics)
        PrintDiagnostic.printFileDiagnostic(printLine(_), Right(path), diag, color = true)
  }
}

object LspDiagnostics extends ServerCommand[LspDiagnosticsOptions] {
  def instance(
    server: Server,
    indexer: Indexer,
    client: CommandClient,
    lspServer: plasmon.jsonrpc.JsonrpcServer,
    pool: plasmon.command.ServerCommandThreadPools
  ): ServerCommandInstance[LspDiagnosticsOptions] =
    LspDiagnostics(server, indexer, client, pool)
  override def names = List(
    List("lsp", "diagnostics"),
    List("lsp-diagnostics")
  )
}
