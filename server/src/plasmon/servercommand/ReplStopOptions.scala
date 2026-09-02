package plasmon.servercommand

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker

final case class ReplStopOptions()

object ReplStopOptions {
  implicit lazy val codec: JsonValueCodec[ReplStopOptions] = JsonCodecMaker.make
}
