package plasmon.bsp

import plasmon.internal.Constants

import scala.util.Properties

object SbtDist {

  lazy val sbtLauncher: os.Path = {
    val archiveCache = coursierapi.ArchiveCache.create()
    val sbtVer = Constants.defaultSbtVersion
    val sbtArchiveUrl = s"https://github.com/sbt/sbt/releases/download/v$sbtVer/sbt-$sbtVer.zip"
    val archiveRoot = os.Path(archiveCache.get(coursierapi.Artifact.of(sbtArchiveUrl)))
    val scriptName = if (Properties.isWin) "sbt.bat" else "sbt"
    archiveRoot / "sbt/bin" / scriptName
  }

}
