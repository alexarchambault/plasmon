package plasmon.ide

import org.eclipse.lsp4j as l

object ClientCommands {

  // Based on https://github.com/scalameta/metals/blob/0ad0bc184f82dbd178d01f76913ea6bdfa98db14/metals/src/main/scala/scala/meta/internal/metals/ClientCommands.scala#L312-L316
  case class WindowLocation(
    uri: String,
    range: l.Range,
    otherWindow: Boolean = false
  )

  // Based on https://github.com/scalameta/metals/blob/0ad0bc184f82dbd178d01f76913ea6bdfa98db14/metals/src/main/scala/scala/meta/internal/metals/ClientCommands.scala#L317-L320
  object GotoLocation extends ParametrizedCommand[WindowLocation](
        "plasmon-goto-location",
        "Goto location"
      )
}
