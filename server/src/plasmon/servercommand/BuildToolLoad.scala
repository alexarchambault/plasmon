package plasmon.servercommand

import caseapp.core.RemainingArgs
import plasmon.Server
import plasmon.command.ServerCommandThreadPools
import plasmon.index.Indexer
import plasmon.protocol.CommandClient

import scala.concurrent.Await
import scala.concurrent.duration.Duration

/** Loads a build tool the way the editor extension's "Load build tool" does.
  *
  * Unlike [[BuildToolAdd]], which is told exactly which build tool to start, this discovers what
  * fits the workspace (and the file passed, if any) and starts that - the same
  * [[ProjectOps.loadBuildTool]] the `plasmon/loadBuildTool` LSP command goes through.
  */
final case class BuildToolLoad(
  server: Server,
  client: CommandClient,
  pools: ServerCommandThreadPools
) extends ServerCommandInstance[BuildToolLoadOptions](client) {
  override def names = BuildToolLoad.names
  def run(options: BuildToolLoadOptions, args: RemainingArgs): Unit = {

    val fileOpt = FileArg
      .optional(args.all, options.uri, server.workingDir)
      .orElse(server.editorState.focusedDocument)

    val (discoverId, toolId) = options.toolId.orElse(options.discoverId) match {
      case Some(toolId) =>
        (options.discoverId.getOrElse(toolId), toolId)
      case None =>
        ProjectOps.discoverBuildTools(server, fileOpt) match {
          case Seq(tool) =>
            (tool.discoverId, tool.buildTool.id)
          case Seq() =>
            printLine(
              s"No build tool found in ${server.workspace()}" +
                fileOpt.fold("")(file => s" for $file"),
              toStderr = true
            )
            exit(1)
          case several =>
            printLine("Several build tools found, pass one of these with --id:", toStderr = true)
            for (tool <- several)
              printLine(s"  ${tool.buildTool.id}", toStderr = true)
            exit(1)
        }
    }

    Await.result(
      ProjectOps.loadBuildTool(server, pools, discoverId, toolId, fileOpt),
      Duration.Inf
    ) match {
      case Left(err) =>
        printLine(s"Error loading build tool $toolId: $err", toStderr = true)
        exit(1)
      case Right(()) =>
        printLine(s"Loaded build tool $toolId", toStderr = true)
    }
  }
}

object BuildToolLoad extends ServerCommand[BuildToolLoadOptions] {
  def instance(
    server: Server,
    indexer: Indexer,
    client: CommandClient,
    lspServer: plasmon.jsonrpc.JsonrpcServer,
    pool: ServerCommandThreadPools
  ): ServerCommandInstance[BuildToolLoadOptions] =
    BuildToolLoad(server, client, pool)
  override def names = List(
    List("build", "tool", "load"),
    List("build-tool", "load"),
    List("build-tool-load")
  )
}
