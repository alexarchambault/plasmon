// Based on https://github.com/scalameta/metals/blob/0ad0bc184f82dbd178d01f76913ea6bdfa98db14/metals/src/main/scala/scala/meta/internal/metals/ScalaTarget.scala

package plasmon.ide

import scala.util.Success
import scala.util.Try

import scala.meta.Dialect
import scala.meta.dialects._
import plasmon.PlasmonEnrichments.*
import scala.jdk.CollectionConverters.*
import scala.meta.internal.semver.SemVer

import ch.epfl.scala.{bsp4j => b}
import scala.meta.internal.metals.ScalaVersions
import scala.meta.internal.metals

case class ScalaTarget(
  info: b.BuildTarget,
  scalaInfo: b.ScalaBuildTarget,
  scalac: b.ScalacOptionsItem,
  autoImports: Option[Seq[String]],
  sbtVersion: Option[String]
) extends JvmTarget {

  def isSbt = sbtVersion.isDefined

  def dialect(path: os.Path): Dialect =
    dialect(path.isSbt)

  def dialect(pathIsSbt: Boolean): Dialect =
    scalaVersion match {
      case _ if info.isSbtBuild && pathIsSbt => Sbt
      case other =>
        val dialect =
          ScalaVersions.dialectForScalaVersion(other, includeSource3 = false)
        dialect match {
          case Scala213 if containsSource3 =>
            Scala213Source3
          case Scala212 if containsSource3 =>
            Scala212Source3
          case Scala3
              if scalac
                .getOptions
                .asScala
                .exists(_.startsWith("-Ykind-projector")) =>
            ScalaTarget.Scala3WithStarAsTypePlaceholder
          case Scala3 =>
            // without StarAsTypePlaceholder since this needs an additional compiler option
            ScalaTarget.Scala3WithCaptureCheckingWithoutStarAsTypePlaceholder
          case other => other
        }
    }

  def displayName: String = info.getName

  def dataKind: String = info.dataKind

  def baseDirectory: String = info.baseDirectory

  def options: List[String] = scalac.getOptions.asScala.toList

  def fmtDialect: metals.ScalafmtDialect =
    ScalaVersions.fmtDialectForScalaVersion(scalaVersion, containsSource3)

  def semanticdbFilesPresent(): Boolean =
    os.walk(targetroot / Directories.semanticdb)
      .exists(_.last.endsWith(".semanticdb"))

  def isSemanticdbEnabled: Boolean =
    scalac.isSemanticdbEnabled(scalaVersion)

  def isSourcerootDeclared: Boolean =
    scalac.isSourcerootDeclared(scalaVersion)

  /** If the build server supports lazy classpath resolution, we will not get any classpath data
    * eagerly and we should not use this endpoint. It should only be used as a fallback.
    *
    * This is due to the fact that we don't request classpath as it can be resonably expensive.
    *
    * @return
    *   non empty classpath only if it was resolved prior
    */
  def classpath: Option[List[String]] =
    if (scalac.getClasspath.isEmpty)
      None
    else
      Some(scalac.getClassDirectory :: scalac.getClasspath.asScala.toList)

  def bestEffortPath: os.Path =
    targetroot / Directories.bestEffort

  def isBestEffort: Boolean = {
    val minVersion = SemVer.Version.fromString("3.5.0")
    Try(SemVer.Version.fromString(scalaVersion)) match {
      case Success(version) =>
        // we compare only major and minor, as we still want RCs and nightlys to work as well
        version.major >= minVersion.major &&
        version.minor >= minVersion.minor
      case _ => false
    }
  }

  def classDirectory: os.Path = scalac.getClassDirectory.osPathFromUri

  def scalaVersion: String = scalaInfo.getScalaVersion

  def id: b.BuildTargetIdentifier = info.getId

  def scalaBinaryVersion: String = scalaInfo.getScalaBinaryVersion

  private def containsSource3 =
    scalac.getOptions.asScala.exists(opt => opt.startsWith("-Xsource:3"))

  def targetroot: os.Path = scalac.targetroot(scalaVersion)

  def scalaPlatform: b.ScalaPlatform = scalaInfo.getPlatform

  private def jvmBuildTarget: Option[b.JvmBuildTarget] = Option(
    scalaInfo.getJvmBuildTarget
  )

  def jvmVersion: Option[String] =
    jvmBuildTarget.flatMap(f => Option(f.getJavaVersion))

  def jvmHome: Option[String] =
    jvmBuildTarget.flatMap(f => Option(f.getJavaHome))
}

object ScalaTarget {
  lazy val Scala3WithStarAsTypePlaceholder = Scala3.withAllowStarAsTypePlaceholder(true)
  lazy val Scala3WithCaptureCheckingWithoutStarAsTypePlaceholder = Scala3
    .withAllowStarAsTypePlaceholder(false)
    .withAllowCaptureChecking(true)
}
