package plasmon.integration

import org.eclipse.lsp4j as l

final case class Positions(
  content: Map[os.SubPath, String],
  positions: Map[(os.SubPath, Int), (Int, Int)]
) {
  def +(other: Positions): Positions =
    Positions(
      content ++ other.content,
      positions ++ other.positions
    )

  def pos(path: os.SubPath, idx: Int): (Int, Int) =
    positions.getOrElse(
      (path, idx),
      sys.error(s"Position $idx in $path not found")
    )
  def lspPos(path: os.SubPath, idx: Int): l.Position = {
    val (lineIdx, colIdx) = pos(path, idx)
    new l.Position(lineIdx, colIdx)
  }
  def lspRange(path: os.SubPath, startIdx: Int, endIdx: Int): l.Range =
    new l.Range(lspPos(path, startIdx), lspPos(path, endIdx))

  def update(path: os.SubPath, source: String): Positions =
    copy(
      content = content + (path -> Positions.regex.pattern.matcher(source).replaceAll("")),
      positions = (positions.filter(_._1._1 != path)) ++ Positions.mapOf(source).map {
        case (idx, pos) =>
          ((path, idx), pos)
      }
    )
}

object Positions {

  def empty: Positions =
    Positions(Map.empty, Map.empty)

  private val regex = "<[0-9]+>".r

  def mapOf(content: String): Map[Int, (Int, Int)] =
    content
      .linesIterator
      .zipWithIndex
      .map {
        case (line, lineIdx) =>
          val (_, values) = regex.findAllMatchIn(line).foldLeft((0, Map.empty[Int, (Int, Int)])) {
            case ((shift, acc), m) =>
              val idx      = m.matched.toString.stripPrefix("<").stripSuffix(">").toInt
              val newShift = shift + (m.end - m.start)
              val newEntry = (idx, (lineIdx, m.start - shift))
              (newShift, acc + newEntry)
          }
          values
      }
      .foldLeft(Map.empty[Int, (Int, Int)])(_ ++ _)

  def of(content: (os.SubPath, String)*): Positions =
    content
      .map {
        case (path, content0) =>
          val updatedContent = regex.pattern.matcher(content0).replaceAll("")
          val posMap = mapOf(content0).map {
            case (idx, pos) =>
              ((path, idx), pos)
          }
          Positions(Map(path -> updatedContent), posMap)
      }
      .foldLeft(empty)(_ + _)
}
