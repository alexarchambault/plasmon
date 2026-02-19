// Originally based on https://github.com/scalameta/metals/blob/0ad0bc184f82dbd178d01f76913ea6bdfa98db14/metals/src/main/scala/scala/meta/internal/metals/SourceMapper.scala

package plasmon.ide

import scala.meta.inputs.Input

import ch.epfl.scala.{bsp4j => b}
import org.eclipse.{lsp4j => l}
import scala.meta.internal.mtags.SourcePath
import plasmon.ide.SbtBuildTool
import plasmon.index.BspData

import plasmon.PlasmonEnrichments._
import plasmon.index.TargetData.MappedSource

final case class SourceMapper(
  bspData: BspData,
  buffers: Buffers
) {
  def mappedTo(targetId: b.BuildTargetIdentifier, path: os.Path): Option[MappedSource] =
    bspData.mappedTo(targetId, path)
  def mappedTo(targetId: b.BuildTargetIdentifier, path: SourcePath): Option[SourcePath] =
    path match {
      case s: SourcePath.Standard =>
        mappedTo(targetId, os.Path(s.path))
          .map(_.path.toNIO)
          .map(SourcePath.Standard(_))
      case _ => None
    }

  def mappedTo0(targetId: b.BuildTargetIdentifier, path: SourcePath): Option[MappedSource] =
    path match {
      case s: SourcePath.Standard =>
        mappedTo(targetId, os.Path(s.path))
      case _ => None
    }

  def pcMapping(
    targetId: b.BuildTargetIdentifier,
    path: os.Path
  ): (Input.VirtualFile, l.Position => l.Position, AdjustLspData) = {

    def input = path.toInputFromBuffers(buffers)
    def default: (Input.VirtualFile, l.Position => l.Position, AdjustLspData) = {
      val viaBuildTargets: Option[(Input.VirtualFile, l.Position => l.Position, AdjustLspData)] =
        bspData.mappedTo(targetId, path).map(_.update(input.value))
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
