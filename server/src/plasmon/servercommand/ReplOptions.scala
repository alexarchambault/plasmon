package plasmon.servercommand

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker

final case class ReplOptions()

object ReplOptions {
  implicit lazy val codec: JsonValueCodec[ReplOptions] = JsonCodecMaker.make
}
