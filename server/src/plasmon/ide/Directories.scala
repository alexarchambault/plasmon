// Originally based on https://github.com/scalameta/metals/blob/0ad0bc184f82dbd178d01f76913ea6bdfa98db14/metals/src/main/scala/scala/meta/internal/metals/Directories.scala

package plasmon.ide

import scala.meta.io.RelativePath

object Directories {
  def readonly0: os.SubPath =
    os.sub / ".plasmon/readonly"
  def readonly: os.SubPath =
    os.sub / ".plasmon/readonly"
  def dependencies: os.SubPath =
    readonly0 / dependenciesName
  def semanticdb: os.SubPath =
    os.sub / "META-INF/semanticdb"
  def bestEffort: os.SubPath =
    os.sub / "META-INF/best-effort"

  val dependenciesName = "dependencies"
}
