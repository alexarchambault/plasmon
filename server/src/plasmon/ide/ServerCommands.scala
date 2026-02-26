package plasmon.ide

import org.eclipse.lsp4j as l

/** LSP commands supported by the Metals language server.
  */
object ServerCommands {

  final case class GotoSymbolParams(
    symbol: String,
    module: String
  )

  // Originally based on https://github.com/scalameta/metals/blob/0ad0bc184f82dbd178d01f76913ea6bdfa98db14/metals/src/main/scala/scala/meta/internal/metals/ServerCommands.scala#L497-L514

  val GotoSymbol = new ParametrizedCommand[GotoSymbolParams](
    "goto",
    "Goto location for symbol"
  )

  val GotoPosition = new ParametrizedCommand[l.Location](
    "goto-position",
    "Goto location for position"
  )
}
