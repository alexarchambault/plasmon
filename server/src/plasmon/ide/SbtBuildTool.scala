package plasmon.ide

import scala.meta.inputs.Input

import org.eclipse.{lsp4j => l}

import plasmon.PlasmonEnrichments._

// Based on https://github.com/scalameta/metals/blob/0ad0bc184f82dbd178d01f76913ea6bdfa98db14/metals/src/main/scala/scala/meta/internal/builds/SbtBuildTool.scala#L472-L499
object SbtBuildTool {
  def sbtInputPosAdjustment(
    originInput: Input.VirtualFile,
    autoImports: Seq[String]
  ): (Input.VirtualFile, l.Position => l.Position, AdjustLspData) = {

    val appendLineSize = autoImports.size

    val modifiedInput =
      originInput.copy(value =
        prependAutoImports(originInput.value, autoImports)
      )
    def adjustRequest(position: l.Position) = new l.Position(
      appendLineSize + position.getLine,
      position.getCharacter
    )
    val adjustLspData = AdjustedLspData.create(
      pos =>
        new l.Position(pos.getLine - appendLineSize, pos.getCharacter),
      filterOutLocations = { loc => !loc.getUri.isSbt }
    )
    (modifiedInput, adjustRequest, adjustLspData)
  }

  def prependAutoImports(text: String, autoImports: Seq[String]): String = {
    val prepend = autoImports.mkString("", "\n", "\n")
    prepend + text
  }
}
