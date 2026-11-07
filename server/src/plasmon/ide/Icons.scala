// Originally based on https://github.com/scalameta/metals/blob/0ad0bc184f82dbd178d01f76913ea6bdfa98db14/metals/src/main/scala/scala/meta/internal/metals/Icons.scala

package plasmon.ide

final case class Icons(
  findsuper: String
)

object Icons {

  private val codicon = """\$\([\w~-]+\)\s*""".r

  /** Removes VS Code `$(name)` icon markers, for clients that would show them verbatim. */
  def stripCodicons(text: String): String =
    codicon.replaceAllIn(text, "").trim

  // icons for vscode can be found here("Icons in Labels"):
  // https://code.visualstudio.com/api/references/icons-in-labels
  val vscode = Icons(
    findsuper = "$(arrow-up)"
  )
}
