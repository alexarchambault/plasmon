package plasmon.pc

import java.util.List as JList

import scala.meta.pc.{OutlineFiles as JOutlineFiles, VirtualFileParams}

// Based on https://github.com/scalameta/metals/blob/0ad0bc184f82dbd178d01f76913ea6bdfa98db14/metals/src/main/scala/scala/meta/internal/metals/OutlineFilesProvider.scala#L191-L194

private case class OutlineFiles(
  files: JList[VirtualFileParams],
  isFirstCompileSubstitute: Boolean = false
) extends JOutlineFiles
