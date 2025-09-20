// Based on https://github.com/scalameta/metals/blob/0ad0bc184f82dbd178d01f76913ea6bdfa98db14/metals/src/main/scala/scala/meta/internal/metals/JvmTarget.scala

package plasmon.ide

import plasmon.PlasmonEnrichments.*

import ch.epfl.scala.{bsp4j => b}

trait JvmTarget {

  def displayName: String

  def id: b.BuildTargetIdentifier

  /** If the build server supports lazy classpath resolution, we will not get any classpath data
    * eagerly and we should not use this endpoint. It should only be used as a fallback.
    *
    * This is due to the fact that we don't request classpath as it can be resonably expensive.
    *
    * @return
    *   non empty classpath only if it was resolved prior
    */
  def classpath: Option[List[String]]

  def classDirectory: os.Path

  /** This method collects jars from classpath defined in scalacOptions.
    *
    * If the build server supports lazy classpath resolution, we will not get any classpath data
    * eagerly and we should not use this endpoint. It should only be used as a fallback.
    *
    * This is due to the fact that we don't request classpath as it can be resonably expensive.
    *
    * We should use the buildTargetDependencyModules information from the indexer instead.
    *
    * @return
    *   non empty classpath jar list if it was resolved prior
    */
  def jarClasspath: Option[List[os.Path]] =
    classpath.map(collectJars)

  private def collectJars(paths: List[String]) =
    paths
      .filter(_.endsWith(".jar"))
      .map(_.osPathFromUri)
}
