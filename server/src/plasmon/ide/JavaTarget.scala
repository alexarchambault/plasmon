// Based on https://github.com/scalameta/metals/blob/0ad0bc184f82dbd178d01f76913ea6bdfa98db14/metals/src/main/scala/scala/meta/internal/metals/JavaTarget.scala

package plasmon.ide

import ch.epfl.scala.bsp4j as b
import plasmon.PlasmonEnrichments.*

import scala.jdk.CollectionConverters.*

case class JavaTarget(
  info: b.BuildTarget,
  javac: b.JavacOptionsItem
) extends JvmTarget {
  def displayName: String = info.getName

  def dataKind: String = info.dataKind

  def baseDirectory: String = info.baseDirectory

  def options: List[String] = javac.getOptions.asScala.toList

  def isSemanticdbEnabled: Boolean =
    javac.isSemanticdbEnabled

  def isSourcerootDeclared: Boolean =
    javac.isSourcerootDeclared

  def isTargetrootDeclared: Boolean =
    javac.isTargetrootDeclared

  def classDirectory: os.Path = javac.getClassDirectory.osPathFromUri

  def id: b.BuildTargetIdentifier = info.getId

  def releaseVersion: Option[String] = javac.releaseVersion

  def targetVersion: Option[String] = javac.targetVersion

  def sourceVersion: Option[String] = javac.sourceVersion

  def targetroot: Option[os.Path] = javac.targetroot.map(_.resolveIfJar)

  /** If the build server supports lazy classpath resolution, we will not get any classpath data
    * eagerly and we should not use this endpoint. It should only be used as a fallback.
    *
    * This is due to the fact that we don't request classpath as it can be resonably expensive.
    *
    * @return
    *   non empty classpath only if it was resolved prior
    */
  def classpath: Option[List[String]] =
    if (javac.getClasspath.isEmpty)
      None
    else
      Some(javac.getClasspath.asScala.toList)
}
