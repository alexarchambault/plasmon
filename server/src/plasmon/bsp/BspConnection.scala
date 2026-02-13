package plasmon.bsp

import ch.epfl.scala.{bsp4j => b}
import org.eclipse.lsp4j.jsonrpc.RemoteEndpoint
import plasmon.Logger
import plasmon.render.JsonCodecs.given
import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker

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
  def enhancedName: String =
    BspConnection.enhancedName(name)
  def info: BuildServerInfo =
    launcher.info

  def asJson: BspConnection.AsJson =
    BspConnection.AsJson(
      params = params,
      launcher = launcher.asJson,
      conn = conn.toString,
      proc = proc,
      remoteEndpoint = remoteEndpoint.toString,
      client = client.asJson,
      logger = logger.toString
    )
}

object BspConnection {
  final case class AsJson(
    params: b.InitializeBuildResult,
    launcher: BuildServerLauncher.AsJson,
    conn: String,
    proc: BuildServerProcess,
    remoteEndpoint: String,
    client: PlasmonBuildClientImpl.AsJson,
    logger: String
  )

  given JsonValueCodec[AsJson] =
    JsonCodecMaker.make

  def enhancedName(name: String): String =
    if (name == "mill-bsp" || name == "mill") "Mill"
    else if (name == "scala-cli") "Scala CLI"
    else name
}
