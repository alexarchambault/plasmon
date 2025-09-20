package plasmon.settingsedit

import scala.scalajs.js
import scala.scalajs.js.annotation.{JSExportTopLevel, JSImport}

@JSExportTopLevel("SettingsEdit")
object SettingsEdit {

  @js.native
  @JSImport("./node_modules/jsonc-parser/lib/esm/main.js", "modify")
  def modify(content: String, jsonPath: js.Array[String], value: js.Any, options: js.Any): js.Any =
    js.native

  @js.native
  @JSImport("./node_modules/jsonc-parser/lib/esm/main.js", "applyEdits")
  def applyEdits(content: String, edits: js.Any): String =
    js.native

  @js.native
  @JSImport("./node_modules/jsonc-parser/lib/esm/main.js", "stripComments")
  def stripComments(content: String): String =
    js.native

  @js.native
  @JSImport("fs", "readFileSync")
  def readFileSync(path: String, options: js.Dictionary[String]): String =
    js.native

  @js.native
  @JSImport("process", "exit")
  def exit(code: Int): Unit =
    js.native

  @js.native
  @JSImport("process", "argv")
  def argv: js.Array[String] =
    js.native

  def actualArgs(args: Array[String]): Array[String] =
    if (args.isEmpty)
      argv.toArray.drop(2) // drop "node" and "/path/to/app.js"
    else
      args

  def encoding = "utf8"

  def patchContent(content: String): String =
    if (content.endsWith("\n")) content
    else content + "\n"

  def main(args: Array[String]): Unit =
    try
      actualArgs(args) match {
        case Array("about") => println("Plasmon settingsEdit")
        case Array("strip-comments", inputPath) =>
          val content        = readFileSync(inputPath, js.Dictionary("encoding" -> encoding))
          val updatedContent = stripComments(patchContent(content))
          print(updatedContent)
        case Array("update", inputPath, updates @ _*) =>
          val content = readFileSync(inputPath, js.Dictionary("encoding" -> encoding))
          val formattingOptions = js.Dictionary(
            "tabSize"      -> 2,
            "insertSpaces" -> true,
            "eol"          -> "\n"
          )
          val modificationOptions = js.Dictionary(
            "formattingOptions" -> formattingOptions
          )
          val updatedContent = updates
            .map { update =>
              update.split(":", 2) match {
                case Array(k, v) => (k, v)
                case _           => ???
              }
            }
            .foldLeft(patchContent(content)) {
              case (content0, (k, v)) =>
                val edits = modify(
                  content0,
                  js.Array(k),
                  if (v == "undefined") js.undefined else v,
                  modificationOptions
                )
                applyEdits(content0, edits)
            }
          print(updatedContent)
        case other =>
          System.err.println(s"Wrong args: ${other.toSeq}")
          exit(1)
      }
    finally {
      System.out.flush()
      System.err.flush()
    }
}
