// Originally based on https://github.com/scalameta/metals/blob/0ad0bc184f82dbd178d01f76913ea6bdfa98db14/metals/src/main/scala/scala/meta/internal/metals/SourceMapper.scala

package plasmon.ide

import scala.meta.inputs.Input

import org.eclipse.{lsp4j => l}
import scala.meta.internal.mtags.SourcePath
import plasmon.ide.SbtBuildTool
import plasmon.index.BspData

import plasmon.PlasmonEnrichments._

final case class SourceMapper(
  bspData: BspData,
  buffers: Buffers
) {
  def mappedFrom(path: os.Path): Option[os.Path] =
    bspData.mappedFrom(path)

  def mappedTo(path: os.Path): Option[os.Path] =
    bspData.mappedTo(path).map(_.path)
  def mappedTo(path: SourcePath): Option[SourcePath] =
    path match {
      case s: SourcePath.Standard =>
        mappedTo(os.Path(s.path))
          .map(_.toNIO)
          .map(SourcePath.Standard(_))
      case _ => None
    }
  def mappedLineForServer(path: os.Path, line: Int): Int =
    bspData.mappedLineForServer(path, line).getOrElse(line)
  def mappedLineForClient(path: os.Path, line: Int): Int =
    bspData.mappedLineForClient(path, line).getOrElse(line)

  def pcMapping(
    path: os.Path
  ): (Input.VirtualFile, l.Position => l.Position, AdjustLspData) = {

    def input = path.toInputFromBuffers(buffers)
    def default: (Input.VirtualFile, l.Position => l.Position, AdjustLspData) = {
      val viaBuildTargets: Option[(Input.VirtualFile, l.Position => l.Position, AdjustLspData)] =
        bspData.mappedTo(path).map(_.update(input.value))
      viaBuildTargets.getOrElse(
        (input, identity[l.Position], AdjustedLspData.default)
      )
    }

    val forScripts: Option[(Input.VirtualFile, l.Position => l.Position, AdjustLspData)] =
      if (path.isSbt)
        bspData
          .sbtAutoImports(path)
          .map(
            SbtBuildTool.sbtInputPosAdjustment(input, _)
          )
      else None

    forScripts.getOrElse(default)
  }
}
