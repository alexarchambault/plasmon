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

  /** Environment that makes [[sbtLauncher]], and anything it starts, run on `javaHome`.
    *
    * `sbt.bat` picks `%JAVA_HOME%\bin\java.exe` up on its own, but the Unix script hard-codes
    * `java_cmd=java` and never looks at `JAVA_HOME` - only its `--java-home` option changes that.
    * Passing the option is not enough anyway: sbt's BSP entry point is a thin client that forks the
    * actual server by re-running this same script (via `-Dsbt.script=…`) with arguments of its own,
    * so the JDK has to be somewhere that fork will find it too. Putting it first on `PATH` reaches
    * both.
    *
    * Without this, sbt runs on whatever `java` happens to be first on `PATH`, and reports that JDK
    * as the `javaHome` of every target it describes - so `--jvm` would mean something for Mill and
    * Scala CLI, and nothing for sbt.
    */
  def env(javaHome: os.Path): Map[String, String] = {
    val javaBinDir = (javaHome / "bin").toString
    val pathSep    = java.io.File.pathSeparator
    val path = sys.env.get("PATH").filter(_.nonEmpty) match {
      case Some(former) => javaBinDir + pathSep + former
      case None         => javaBinDir
    }
    Map(
      "JAVA_HOME" -> javaHome.toString,
      "PATH"      -> path
    )
  }

}
