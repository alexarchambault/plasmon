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
    * `sbt.bat` reads `JAVA_HOME` itself, and puts its `bin` first on the `PATH` it passes on, so on
    * Windows naming the JDK is enough. The Unix script hard-codes `java_cmd=java` and never looks
    * at `JAVA_HOME` - only its `--java-home` option changes that. Passing the option is not enough
    * anyway: sbt's BSP entry point is a thin client that forks the actual server by re-running this
    * same script (via `-Dsbt.script=…`) with arguments of its own, so the JDK has to be somewhere
    * that fork will find it too. Putting it first on `PATH` reaches both.
    *
    * `PATH` is left alone on Windows on purpose. Its real name there is `Path`, so replacing
    * `PATH` wholesale drops the system directories from the child's environment - `sbt.bat` then
    * can't find `findstr`, mis-detects the Java version and refuses to start.
    *
    * Without this, sbt runs on whatever `java` happens to be first on `PATH`, and reports that JDK
    * as the `javaHome` of every target it describes - so `--jvm` would mean something for Mill and
    * Scala CLI, and nothing for sbt.
    */
  def env(javaHome: os.Path): Map[String, String] = {
    val javaHomeEnv = Map("JAVA_HOME" -> javaHome.toString)
    if (Properties.isWin) javaHomeEnv
    else {
      val javaBinDir = (javaHome / "bin").toString
      val path = sys.env.get("PATH").filter(_.nonEmpty) match {
        case Some(former) => javaBinDir + java.io.File.pathSeparator + former
        case None         => javaBinDir
      }
      javaHomeEnv + ("PATH" -> path)
    }
  }

}
