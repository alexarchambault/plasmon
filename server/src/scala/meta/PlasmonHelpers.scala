package scala.meta

import scala.meta.internal.semanticdb.Scala

object PlasmonHelpers {
  def inputLineToOffset(input: Input, line: Int): Int =
    input.lineToOffset(line)
  def DescriptorParser(symbol: String) =
    Scala.DescriptorParser(symbol)
  def Tokens(tokens: Array[Token]): scala.meta.tokens.Tokens =
    scala.meta.tokens.Tokens(tokens)
}
