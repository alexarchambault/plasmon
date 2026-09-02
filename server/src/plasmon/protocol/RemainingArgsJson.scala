package plasmon.protocol

import caseapp.core.{Indexed, RemainingArgs}
import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker

/** [[caseapp.core.RemainingArgs]] on the wire.
  *
  * What a command is passed besides its options: the arguments, and separately whatever followed a
  * `--`. case-app remembers where on the command line each of them came from, and that is kept here
  * too - a command that reports a position back to the user would otherwise point at nothing.
  */
final case class RemainingArgsJson(
  remaining: List[RemainingArgsJson.Arg] = Nil,
  unparsed: List[RemainingArgsJson.Arg] = Nil
) {
  def toRemainingArgs: RemainingArgs =
    RemainingArgs(remaining.map(_.toIndexed), unparsed.map(_.toIndexed))
}

object RemainingArgsJson {

  final case class Arg(index: Int, length: Int, value: String) {
    def toIndexed: Indexed[String] =
      Indexed(index, length, value)
  }

  object Arg {
    def of(indexed: Indexed[String]): Arg =
      Arg(indexed.index, indexed.length, indexed.value)
  }

  def of(args: RemainingArgs): RemainingArgsJson =
    RemainingArgsJson(
      args.indexedRemaining.toList.map(Arg.of),
      args.indexedUnparsed.toList.map(Arg.of)
    )

  def empty: RemainingArgsJson =
    RemainingArgsJson()

  implicit lazy val codec: JsonValueCodec[RemainingArgsJson] = JsonCodecMaker.make
}
