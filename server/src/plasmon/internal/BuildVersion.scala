package plasmon.internal

import java.util.Properties

/** Version details, read at runtime from a resource written by the build.
  *
  * These live in a resource rather than in [[Constants]] on purpose: they change at each commit,
  * and having them in generated sources would recompile everything every time.
  */
object BuildVersion {

  private def resourcePath = "plasmon/version.properties"

  private lazy val properties: Properties = {
    val props = new Properties
    val isOpt = Seq(getClass.getClassLoader, Thread.currentThread().getContextClassLoader)
      .iterator
      .filter(_ != null)
      .flatMap(cl => Option(cl.getResourceAsStream(resourcePath)).iterator)
      .nextOption()
    for (is <- isOpt)
      try props.load(is)
      finally is.close()
    props
  }

  private def property(key: String): Option[String] =
    Option(properties.getProperty(key)).map(_.trim).filter(_.nonEmpty)

  lazy val version: String =
    property("version").getOrElse("unknown")

  lazy val commitHash: Option[String] =
    property("commit-hash")

  /** Whether [[version]] is a released version, rather than a snapshot one */
  def isStable: Boolean =
    version != "unknown" && !version.endsWith("-SNAPSHOT")

  /** [[version]], followed by the commit hash for non-stable versions */
  lazy val fullVersion: String =
    commitHash.filter(_ => !isStable) match {
      case Some(hash) => s"$version ($hash)"
      case None       => version
    }
}
