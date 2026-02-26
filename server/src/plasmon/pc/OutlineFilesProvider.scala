// Originally based on https://github.com/scalameta/metals/blob/0ad0bc184f82dbd178d01f76913ea6bdfa98db14/metals/src/main/scala/scala/meta/internal/metals/OutlineFilesProvider.scala

package plasmon.pc

import ch.epfl.scala.bsp4j as b
import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import plasmon.PlasmonEnrichments.*
import plasmon.ide.Buffers
import plasmon.index.BspData

import java.util.Optional

import scala.collection.concurrent.TrieMap
import scala.meta.internal.metals.ScalaVersions
import scala.meta.pc.OutlineFiles as JOutlineFiles

private class OutlineFilesProvider(
  bspData: BspData,
  buffers: Buffers
) {
  import OutlineFilesProvider._

  private val outlineFiles =
    new TrieMap[b.BuildTargetIdentifier, BuildTargetOutlineFilesProvider]()

  def shouldRestartPc(
    id: b.BuildTargetIdentifier,
    reason: PcRestartReason
  ): Boolean =
    reason match {
      case DidCompile(true) => true
      case _ =>
        outlineFiles.get(id) match {
          case Some(provider) =>
            // if it was never compiled successfully by the build server
            // we don't restart pc not to lose information from outline compile
            provider.wasSuccessfullyCompiledByBuildServer
          case None => true
        }
    }

  def onDidCompile(id: b.BuildTargetIdentifier, wasSuccessful: Boolean): Unit =
    outlineFiles.get(id) match {
      case Some(provider) =>
        if (wasSuccessful) provider.successfulCompilation()
      case None =>
        for {
          scalaTarget <- bspData.scalaTarget(id)
          // we don't perform outline compilation for Scala 3
          if !ScalaVersions.isScala3Version(scalaTarget.scalaVersion)
        } outlineFiles.putIfAbsent(
          id,
          new BuildTargetOutlineFilesProvider(
            bspData,
            buffers,
            id,
            wasSuccessful
          )
        )
    }

  def didChange(id: String, path: os.Path): Unit =
    buildTargetId(id).foreach(didChange(_, path))

  private def didChange(id: b.BuildTargetIdentifier, path: os.Path): Unit =
    for (provider <- outlineFiles.get(id))
      provider.didChange(path)

  def getOutlineFiles(id: String): Optional[JOutlineFiles] =
    getOutlineFiles(buildTargetId(id))

  private def getOutlineFiles(
    buildTargetId: Option[b.BuildTargetIdentifier]
  ): Optional[JOutlineFiles] = {
    val res: Option[JOutlineFiles] =
      for {
        id           <- buildTargetId
        provider     <- outlineFiles.get(id)
        outlineFiles <- provider.outlineFiles()
      } yield outlineFiles
    res.asJava
  }

  def clear(): Unit = {
    outlineFiles.clear()
  }

  private def buildTargetId(id: String): Option[b.BuildTargetIdentifier] =
    Option(id).filter(_.nonEmpty).map(new b.BuildTargetIdentifier(_))

  def asJson: OutlineFilesProvider.AsJson =
    OutlineFilesProvider.AsJson()
}

private object OutlineFilesProvider {
  sealed trait PcRestartReason
  case object InverseDependency              extends PcRestartReason
  case class DidCompile(wasSuccess: Boolean) extends PcRestartReason

  final case class AsJson()

  given JsonValueCodec[AsJson] =
    JsonCodecMaker.make
}
