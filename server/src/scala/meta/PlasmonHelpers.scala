package scala.meta

import scala.meta.internal.semanticdb.Scala

object PlasmonHelpers {
  def inputLineToOffset(input: Input, line: Int): Int =
    input.lineToOffsetAndLength(line)._1
  def DescriptorParser(symbol: String) =
    Scala.DescriptorParser(symbol)
  def Tokens(tokens: Array[Token]): scala.meta.tokens.Tokens =
    scala.meta.tokens.Tokens(tokens)
}
