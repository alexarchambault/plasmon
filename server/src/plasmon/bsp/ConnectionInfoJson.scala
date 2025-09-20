package plasmon.bsp

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker

sealed abstract class ConnectionInfoJson extends Product with Serializable {
  def toConnectionInfo(tools: BuildTool.Tools): BuildServerInfo
}

object ConnectionInfoJson {
  final case class Bsp(
    workspace: String,
    bspFile: String,
    onlyTargets: Option[List[String]]
  ) extends ConnectionInfoJson {
    def toConnectionInfo(tools: BuildTool.Tools): BuildServerInfo.Bsp =
      BuildServerInfo.Bsp(os.Path(workspace), os.Path(bspFile), onlyTargets.map(_.toSet))
  }
  final case class Bloop(workspace: String) extends ConnectionInfoJson {
    def toConnectionInfo(tools: BuildTool.Tools): BuildServerInfo.Bloop =
      BuildServerInfo.Bloop(os.Path(workspace))
  }
  final case class Mill(workspace: String, onlyTargets: Option[List[String]])
      extends ConnectionInfoJson {
    def toConnectionInfo(tools: BuildTool.Tools): BuildServerInfo.Mill =
      BuildServerInfo.Mill(os.Path(workspace), onlyTargets.map(_.toSet))
  }
  final case class Sbt(workspace: String, onlyTargets: Option[List[String]])
      extends ConnectionInfoJson {
    def toConnectionInfo(tools: BuildTool.Tools): BuildServerInfo.Sbt =
      BuildServerInfo.Sbt(os.Path(workspace), onlyTargets.map(_.toSet))
  }
  final case class ScalaCli(workspace: String, paths: Seq[String])
      extends ConnectionInfoJson {
    def toConnectionInfo(tools: BuildTool.Tools): BuildServerInfo.ScalaCli =
      BuildServerInfo.ScalaCli(
        os.Path(workspace),
        paths.map(os.Path(_)),
        tools.tools.getOrElse("scala-cli", sys.error("scala-cli not found in tools"))
      )
  }
  implicit lazy val codec: JsonValueCodec[ConnectionInfoJson] = JsonCodecMaker.make
  lazy val seqCodec: JsonValueCodec[Seq[ConnectionInfoJson]]  = JsonCodecMaker.make
  def apply(info: BuildServerInfo): ConnectionInfoJson =
    info match {
      case b: BuildServerInfo.Bsp =>
        Bsp(
          b.workspace.toString,
          b.bspFile.map(b.workspace / _).merge.toString,
          b.onlyTargets.map(_.toList.sorted)
        )
      case b: BuildServerInfo.Bloop =>
        Bloop(b.workspace.toString)
      case m: BuildServerInfo.Mill =>
        Mill(
          m.workspace.toString,
          m.onlyTargets.map(_.toList.sorted)
        )
      case m: BuildServerInfo.Sbt =>
        Sbt(
          m.workspace.toString,
          m.onlyTargets.map(_.toList.sorted)
        )
      case s: BuildServerInfo.ScalaCli =>
        ScalaCli(s.workspace.toString, s.paths.map(_.toString))
    }
}
