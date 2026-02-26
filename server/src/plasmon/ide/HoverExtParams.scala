// Based on https://github.com/scalameta/metals/blob/0ad0bc184f82dbd178d01f76913ea6bdfa98db14/metals/src/main/scala/scala/meta/internal/metals/HoverExtension.scala#L9

package plasmon.ide

import org.eclipse.lsp4j as l

import javax.annotation.Nullable

case class HoverExtParams(
  textDocument: l.TextDocumentIdentifier,
  @Nullable position: l.Position = null,
  @Nullable range: l.Range = null
) {
  def getPosition: l.Position =
    if (position != null) position
    else range.getStart
}
