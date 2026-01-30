// Based on https://github.com/scalameta/metals/blob/0ad0bc184f82dbd178d01f76913ea6bdfa98db14/metals/src/main/scala/scala/meta/internal/metals/MutableMd5Fingerprints.scala

package plasmon.ide

import java.nio.charset.Charset
import java.nio.charset.StandardCharsets
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.ConcurrentLinkedQueue

import scala.meta.internal.io.FileIO
import scala.meta.internal.jdk.CollectionConverters._
import scala.meta.internal.mtags.MD5
import scala.meta.internal.mtags.Md5Fingerprints
import scala.meta.io.AbsolutePath

import plasmon.PlasmonEnrichments.*
import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker

final class MutableMd5Fingerprints extends Md5Fingerprints {
  private val fingerprints = new ConcurrentHashMap[os.Path, ConcurrentLinkedQueue[Fingerprint]]

  def getAllFingerprints: Map[os.Path, List[Fingerprint]] =
    fingerprints.asScala.toMap.map {
      case (path, queue) =>
        path -> queue.asScala.toList
    }

  def addAll(fingerprints: Map[os.Path, List[Fingerprint]]): Unit =
    for {
      (path, fingerprints) <- fingerprints
      fingerprint          <- fingerprints
    } add(path, fingerprint)

  def add(
    path: os.Path,
    text: String,
    md5: Option[String] = None
  ): Fingerprint = {
    val fingerprint = Fingerprint(text, md5.getOrElse(MD5.compute(text)))
    add(path, fingerprint)
    fingerprint
  }

  private def add(
    path: os.Path,
    fingerprint: Fingerprint
  ): Unit =
    fingerprints
      .computeIfAbsent(path, _ => new ConcurrentLinkedQueue)
      .add(fingerprint)

  override def lookupText(path: AbsolutePath, md5: String): Option[String] = {
    val currentLookup = for {
      prints      <- Option(fingerprints.get(path))
      fingerprint <- prints.asScala.find(_.md5 == md5)
    } yield {
      // clear older fingerprints that no longer correspond to the semanticDB hash
      prints.clear()
      prints.add(fingerprint)
      fingerprint.text
    }

    currentLookup.orElse {
      val text       = os.read(path.toOs)
      val currentMD5 = MD5.compute(text)
      if (md5 == currentMD5) {
        add(path.toOs, text, Some(md5))
        Some(text)
      }
      else None
    }
  }

  override def loadLastValid(
    path: AbsolutePath,
    soughtMd5: String,
    charset: Charset
  ): Option[String] = {
    val text = os.read(path.toOs, charset)
    val md5  = MD5.compute(text)
    if (soughtMd5 == md5)
      Some(text)
    else
      lookupText(path, soughtMd5)
  }

  override def toString: String = s"Md5FingerprintProvider($fingerprints)"

  def asJson: MutableMd5Fingerprints.AsJson =
    MutableMd5Fingerprints.AsJson(
      fingerprints = fingerprints.asScala.toMap.map {
        case (k, v) =>
          (k.toString, v.asScala.toSeq)
      }
    )
}

object MutableMd5Fingerprints {
  final case class AsJson(
    fingerprints: Map[String, Seq[Fingerprint]]
  )

  given JsonValueCodec[AsJson] =
    JsonCodecMaker.make
}
