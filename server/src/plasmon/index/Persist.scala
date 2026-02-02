package plasmon.index

import ch.epfl.scala.{bsp4j => b}
import com.github.plokhotnyuk.jsoniter_scala.core._
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import plasmon.bsp.ConnectionInfoJson
import plasmon.bsp.BuildServerInfo
import plasmon.bsp.BuildTool

object Persist {

  def loadFromDisk(
    readFrom: os.Path,
    tools: BuildTool.Tools
  ): Option[(Seq[BuildServerInfo], Seq[(BuildServerInfo, Seq[b.BuildTargetIdentifier])])] =
    if (os.isFile(readFrom)) {
      val entries =
        try readFromArray(os.read.bytes(readFrom))(using Entry.seqCodec)
        catch {
          case e: JsonReaderException =>
            throw new Exception(e)
        }
      val addAllTargets =
        entries.iterator.filter(_.all).map(_.server.toConnectionInfo(tools)).toVector
      val targets = entries.iterator.filter(!_.all).toVector.map(e =>
        e.server.toConnectionInfo(tools) -> e.targets.map(new b.BuildTargetIdentifier(_))
      )
      Some((addAllTargets, targets))
    }
    else
      None

  def persistTargets(
    addAllTargets: Set[BuildServerInfo],
    targets: Map[BuildServerInfo, Seq[b.BuildTargetIdentifier]],
    persistTo: os.Path
  ): Unit = {
    val fromAll = addAllTargets.toVector.map { info =>
      Entry(ConnectionInfoJson(info), Nil, all = true)
    }
    val fromTargets = targets.map {
      case (info, ids) =>
        Entry(ConnectionInfoJson(info), ids.map(_.getUri), all = false)
    }
    val b = writeToArray[Seq[Entry]](fromAll ++ fromTargets)(using Entry.seqCodec)
    scribe.info(s"Writing targets to $persistTo")
    os.write.over(persistTo, b, createFolders = true)
  }

  private final case class Entry(
    server: ConnectionInfoJson,
    targets: Seq[String],
    all: Boolean
  )
  private object Entry {
    implicit lazy val codec: JsonValueCodec[Entry] = JsonCodecMaker.make
    lazy val seqCodec: JsonValueCodec[Seq[Entry]]  = JsonCodecMaker.make
  }

}
