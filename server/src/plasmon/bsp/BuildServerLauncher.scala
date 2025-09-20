package plasmon.bsp

import plasmon.Logger

final case class BuildServerLauncher(
  info: BuildServerInfo,
  preliminaryDisplayName: String,
  prepare: Option[(Logger, Boolean) => Unit]
)
