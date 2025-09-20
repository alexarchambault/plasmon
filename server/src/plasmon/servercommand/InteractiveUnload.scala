package plasmon.servercommand

import caseapp.core.RemainingArgs
import ch.epfl.scala.{bsp4j => b}
import plasmon.protocol.CommandClient
import plasmon.index.Indexer
import plasmon.Server

final case class InteractiveUnload(
  server: Server,
  client: CommandClient
) extends ServerCommandInstance[InteractiveUnloadOptions](client) {
  def run(options: InteractiveUnloadOptions, args: RemainingArgs): Unit = {
    val fromArgs = args.all.flatMap { arg =>
      val path = os.Path(arg, server.workingDir)
      if (!os.exists(path))
        sys.error(s"$path not found")
      server.bspData.allTargetData.flatMap { targetData =>
        targetData.sourceBuildTargets(path)
          .map(_.toVector)
          .getOrElse(Nil)
      }
    }
    val fromTargetOptions = options.target.map { str =>
      BspUtil.targetFullId(server.workingDir, str)
    }
    val allTargetIds = fromArgs ++ fromTargetOptions
    def unload(targetId: b.BuildTargetIdentifier, isCompletion: Boolean = false): Unit = {
      val unloaded = server.presentationCompilers.unloadCompilerForTarget(targetId, isCompletion)
      printLine(
        if (unloaded)
          s"Unloaded interactive compiler for ${BspUtil.targetShortId(server.bspData, targetId)}"
        else
          "No interactive compiler to unload",
        toStderr = true
      )
    }
    val unloadCompletion = options.completion.getOrElse(false)
    val unloadMain       = options.main.getOrElse(!unloadCompletion)
    if (unloadMain)
      for (targetId <- allTargetIds)
        unload(targetId)
    if (unloadCompletion)
      for (targetId <- allTargetIds)
        unload(targetId, isCompletion = true)
  }
}

object InteractiveUnload extends ServerCommand[InteractiveUnloadOptions] {
  def instance(
    server: Server,
    indexer: Indexer,
    client: CommandClient,
    lspServer: plasmon.jsonrpc.JsonrpcServer,
    pool: plasmon.command.ServerCommandThreadPools
  ): ServerCommandInstance[InteractiveUnloadOptions] =
    InteractiveUnload(server, client)
}
