package plasmon.bsp

import ch.epfl.scala.{bsp4j => b}
import org.eclipse.lsp4j.jsonrpc.RemoteEndpoint
import plasmon.Logger

final case class BspConnection(
  params: b.InitializeBuildResult,
  launcher: BuildServerLauncher,
  conn: PlasmonBuildServer,
  proc: BuildServerProcess,
  remoteEndpoint: RemoteEndpoint,
  client: PlasmonBuildClientImpl,
  logger: Logger
) {
  def name: String =
    params.getDisplayName
  def info: BuildServerInfo =
    launcher.info
}
