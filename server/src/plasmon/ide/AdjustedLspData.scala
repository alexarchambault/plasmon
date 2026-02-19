package plasmon.ide

import java.util.{List => JList}

import scala.jdk.CollectionConverters.*

import org.eclipse.{lsp4j => l}
import plasmon.PlasmonEnrichments.StringThingExtensions

// Originally based on https://github.com/scalameta/metals/blob/2a6f8a437a1ce7c44140673edfe34bb74dfd33be/metals/src/main/scala/scala/meta/internal/metals/AdjustLspData.scala#L110-L134 or an earlier version of that file

case class AdjustedLspData(
  appliesTo: os.Path,
  userPath: os.Path,
  adjustPosition: l.Position => l.Position,
  filterOutLocations: l.Location => Boolean
) extends AdjustLspData {

  def paths: Option[(appliesTo: os.Path, userPath: os.Path)] =
    Some((appliesTo, userPath))

  override def adjustLocations(
    locations: JList[l.Location]
  ): JList[l.Location] =
    locations.asScala
      .collect {
        case loc if !filterOutLocations(loc) =>
          paths match {
            case Some((appliesTo, userPath)) =>
              if (loc.getUri.osPathFromUri == appliesTo) {
                loc.setUri(userPath.toNIO.toUri.toASCIIString)
                loc.setRange(adjustRange(loc.getRange))
                Seq(loc)
              }
              else
                Nil
            case None =>
              loc.setRange(adjustRange(loc.getRange))
              Seq(loc)
          }
      }
      .flatten
      .asJava
  override def adjustPos(
    pos: l.Position,
    adjustToZero: Boolean = true
  ): l.Position = {
    val adjusted = adjustPosition(pos)
    if (adjustToZero && adjusted.getCharacter < 0) adjusted.setCharacter(0)
    if (adjustToZero && adjusted.getLine < 0) adjusted.setLine(0)
    adjusted
  }

}

object AdjustedLspData {

  def create(
    appliesTo: os.Path,
    userPath: os.Path,
    f: l.Position => l.Position,
    filterOutLocations: l.Location => Boolean = _ => false
  ): AdjustLspData =
    AdjustedLspData(
      appliesTo,
      userPath,
      pos => f(pos),
      filterOutLocations
    )

  val default: AdjustLspData = DefaultAdjustedData
}
