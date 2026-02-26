package plasmon.servercommand

import caseapp.core.RemainingArgs
import plasmon.Server
import plasmon.index.Indexer
import plasmon.internal.Constants
import plasmon.protocol.CommandClient
import plasmon.protocol.CommandClient.ops.*

final case class About(
  server: Server,
  client: CommandClient
) extends ServerCommandInstance[AboutOptions](client) {
  def run(options: AboutOptions, args: RemainingArgs): Unit = {
    val isGraalvmNativeImage = sys.props.contains("org.graalvm.nativeimage.imagecode")

    client.printLine(s"Plasmon ${Constants.version}")
    client.printLine(s"  - based on Metals (mtags) ${Constants.mtagsVersion}")
    client.printLine(s"  - relying on scalameta ${Constants.scalametaVersion}")
    client.printLine(s"  - using BSP ${Constants.bspVersion}")
    client.printLine(
      if (isGraalvmNativeImage)
        s"  - native binary"
      else
        s"  - running on ${sys.props.getOrElse("java.vm.vendor", "")} ${sys.props.getOrElse("java.vm.version", "")}"
    )
  }
}

object About extends ServerCommand[AboutOptions] {
  def instance(
    server: Server,
    indexer: Indexer,
    client: CommandClient,
    lspServer: plasmon.jsonrpc.JsonrpcServer,
    pool: plasmon.command.ServerCommandThreadPools
  ): ServerCommandInstance[AboutOptions] =
    About(server, client)
}
