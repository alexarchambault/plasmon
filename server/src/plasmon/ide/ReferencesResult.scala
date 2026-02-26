package plasmon.ide

import org.eclipse.lsp4j as l

// Based on https://github.com/scalameta/metals/blob/0ad0bc184f82dbd178d01f76913ea6bdfa98db14/metals/src/main/scala/scala/meta/internal/metals/ReferencesResult.scala#L5
case class ReferencesResult(symbol: String, locations: Seq[l.Location])
