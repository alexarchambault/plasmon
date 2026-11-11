package plasmon.pc

import coursier.version.Version
import plasmon.internal.Constants

object SupportedScalaVersions {

  /** Whether we have a presentation compiler for a Scala version
    *
    * Our presentation compilers handle the versions of their Scala binary version up to their own.
    */
  def isSupported(scalaVersion: String): Boolean = {
    val latestOpt =
      if (scalaVersion.startsWith("3.")) Some(Constants.scalaVersion)
      else RemotePcHost.scala2Versions.get(RemotePcHost.scalaBinaryVersion(scalaVersion))
    latestOpt.exists(latest => Version(scalaVersion) <= Version(latest))
  }
}
