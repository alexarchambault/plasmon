// Originally based on https://github.com/scalameta/metals/blob/2a6f8a437a1ce7c44140673edfe34bb74dfd33be/metals/src/main/scala/scala/meta/internal/implementation/SuperMethodProvider.scala or an earlier version of that file

package plasmon.handlers.codelens

import scala.meta.internal.semanticdb.SymbolInformation

object SuperMethodProvider {

  def findSuperForMethodOrField(methodSymbolInformation: SymbolInformation): Option[String] =
    methodSymbolInformation.overriddenSymbols.headOption.filter(isNonStopSymbol)

  private def isNonStopSymbol(symbol: String): Boolean =
    !stopSymbols.exists(stop => symbol.startsWith(stop))

  private final val stopSymbols: Set[String] = Set(
    "scala/AnyRef#",
    "scala/Serializable#",
    "java/io/Serializable#",
    "java/lang/Object#",
    "scala/AnyVal#",
    "scala/Any#"
  )
}
