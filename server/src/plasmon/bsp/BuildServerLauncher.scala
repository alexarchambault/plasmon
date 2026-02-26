package plasmon.bsp

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import plasmon.Logger

final case class BuildServerLauncher(
  info: BuildServerInfo,
  preliminaryDisplayName: String,
  prepare: Option[(Logger, Boolean) => Unit]
) {
  def asJson: BuildServerLauncher.AsJson =
    BuildServerLauncher.AsJson(
      info = info,
      preliminaryDisplayName = preliminaryDisplayName,
      prepare = prepare.map(_.toString)
    )
}

object BuildServerLauncher {

  final case class AsJson(
    info: BuildServerInfo,
    preliminaryDisplayName: String,
    prepare: Option[String]
  )

  given JsonValueCodec[AsJson] =
    JsonCodecMaker.make

}
