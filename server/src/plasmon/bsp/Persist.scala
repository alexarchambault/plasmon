package plasmon.bsp

import com.github.plokhotnyuk.jsoniter_scala.core.*

object Persist {

  def load(path: os.Path, tools: BuildTool.Tools): Seq[BuildTool] = {
    val bytes = os.read.bytes(path)
    val res =
      try readFromArray(bytes)(BuildTool.BuildToolJson.seqCodec)
      catch {
        case e: JsonReaderException =>
          throw new Exception(e)
      }
    res.map(_.toBuildTool(tools))
  }
}
