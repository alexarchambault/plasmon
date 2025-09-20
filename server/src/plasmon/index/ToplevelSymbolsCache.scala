package plasmon.index

import com.github.plokhotnyuk.jsoniter_scala.core.*
import com.github.plokhotnyuk.jsoniter_scala.macros.{CodecMakerConfig, JsonCodecMaker}
import net.jpountz.lz4.{LZ4FrameInputStream, LZ4FrameOutputStream}

import java.io.{ByteArrayOutputStream, FileInputStream, FileOutputStream}
import java.nio.file.{AtomicMoveNotSupportedException, FileAlreadyExistsException}
import java.time.format.DateTimeFormatter
import java.time.{Instant, LocalDateTime, ZoneOffset}
import java.util.UUID

import scala.annotation.tailrec

private final class ToplevelSymbolsCache(
  root: os.Path,
  coursierCache: os.Path,
  coursierArchiveCache: os.Path,
  readOnly: Boolean = false
) {

  private val prefixes = Seq[(os.Path, String, os.SubPath => Option[String])](
    // FIXME We might need to un-escape things when computing URIs here
    (
      coursierCache,
      "v1",
      subPath => Some(subPath.segments.head + "://" + subPath.segments.tail.mkString("/"))
    ),
    (
      coursierArchiveCache,
      "arc",
      subPath => Some(subPath.segments.head + "://" + subPath.segments.tail.mkString("/"))
    ),
    (os.home, "home", _ => None),
    (os.root, "root", _ => None)
  )

  private def cacheFileOpt(file: os.Path): Option[(os.Path, Option[String])] = {

    val baseFileOpt = prefixes.collectFirst {
      case (prefix, name, getLooseUri) if file.startsWith(prefix) =>
        val subPath     = file.relativeTo(prefix).asSubPath
        val looseUriOpt = getLooseUri(subPath)
        (root / name / subPath, looseUriOpt)
    }

    baseFileOpt.map {
      case (baseFile, looseUriOpt) =>
        (baseFile / os.up / s"${baseFile.last}.symbols", looseUriOpt)
    }
  }

  private def readOrCompute(
    file: os.Path,
    cacheFile: os.Path,
    looseUriOpt: Option[String],
    compute: () => Map[String, Seq[os.SubPath]],
    previousValueOpt: Option[(Map[String, Seq[os.SubPath]], Long)]
  ): Either[(Map[String, Seq[os.SubPath]], Long), Map[String, Seq[os.SubPath]]] = {
    val lastModifiedOnDisk = os.mtime(file)
    val fromDiskOpt = Some(cacheFile)
      .filter(os.exists)
      .flatMap { _ =>
        if (!os.isFile(cacheFile))
          throw new Exception(s"$cacheFile: invalid file in cache, expected a file")
        val inStream = new LZ4FrameInputStream(new FileInputStream(cacheFile.toIO))
        val baos     = new ByteArrayOutputStream
        inStream.transferTo(baos)
        inStream.close()
        // val compressedBytes = os.read.bytes(cacheFile)
        // val bytes = {
        //   val decompressor = ToplevelSymbolsCache.lz4Factory.safeDecompressor()
        //   @tailrec
        //   def decompress(maxSize: Int): Array[Byte] =
        //     try decompressor.decompress(compressedBytes, maxSize)
        //     catch {
        //       case e: LZ4Exception =>
        //         if (2 * maxSize > 1024*1024*1024)
        //           throw new Exception(e)
        //         else
        //           decompress(maxSize * 2)
        //     }
        //   decompress(3 * compressedBytes.length)
        // }
        val bytes = baos.toByteArray
        val format =
          try readFromArray(bytes)(ToplevelSymbolsCache.formatCodec)
          catch {
            case e: JsonReaderException =>
              throw new ToplevelSymbolsCache.JsonException(cacheFile, e)
          }
        val lastModifiedFromCache = format.sourceLastModifiedMillis
        if (lastModifiedOnDisk > 0 && lastModifiedOnDisk == lastModifiedFromCache)
          Some(format.symbolsAsPaths)
        else
          None
      }
    fromDiskOpt.map(Right(_)).getOrElse {
      val symbols = previousValueOpt
        .collect {
          case (value, `lastModifiedOnDisk`) =>
            value
        }
        .getOrElse {
          compute()
        }
      val format = ToplevelSymbolsCache.Format(
        looseUriOpt,
        Instant.ofEpochMilli(lastModifiedOnDisk)
          .atOffset(ZoneOffset.UTC)
          .toLocalDateTime
          .format(ToplevelSymbolsCache.dateTimeFormatter),
        Map(),
        symbols.map { case (key, pathValues) => key -> pathValues.map(_.toString) }
      )
      val content = writeToArray(format)(ToplevelSymbolsCache.formatCodec)
      // val (compressedContent, compressedContentLength) = {
      //   val compressor = ToplevelSymbolsCache.lz4Factory.fastCompressor()
      //   val maxCompressedLength = compressor.maxCompressedLength(content.length)
      //   val dest = Array.ofDim[Byte](maxCompressedLength)
      //   val size = compressor.compress(content, dest)
      //   (dest, size)
      // }
      val tmpFile = cacheFile / os.up / s".${cacheFile.last}.${UUID.randomUUID()}"
      os.makeDir.all(tmpFile / os.up)
      val outStream = new LZ4FrameOutputStream(new FileOutputStream(tmpFile.toIO))
      outStream.write(content)
      outStream.close()
      try {
        try os.move(tmpFile, cacheFile, replaceExisting = false, atomicMove = true)
        catch {
          case _: AtomicMoveNotSupportedException =>
            os.move(tmpFile, cacheFile, replaceExisting = false, atomicMove = false)
        }
        Right(symbols)
      }
      catch {
        case _: FileAlreadyExistsException =>
          // Assuming another thread or process just wrote the file
          Left((symbols, lastModifiedOnDisk))
      }
    }
  }

  def get(
    file: os.Path,
    compute: () => Map[String, Seq[os.SubPath]]
  ): Option[Map[String, Seq[os.SubPath]]] =
    cacheFileOpt(file).map {
      case (cacheFile, looseUriOpt) =>
        val compute0 =
          if (readOnly)
            () => throw new ToplevelSymbolsCache.ReadOnlyCacheException(file)
          else
            compute

        @tailrec
        def loop(attempt: Int): Map[String, Seq[os.SubPath]] =
          if (attempt <= ToplevelSymbolsCache.maxAttempts)
            readOrCompute(file, cacheFile, looseUriOpt, compute0, None) match {
              case Right(value) => value
              case Left(_)      => loop(attempt + 1)
            }
          else
            throw new Exception(
              s"Error while serializing toplevel symbols of $file to $cacheFile (concurrent attempts?)"
            )

        loop(1)
    }

}

private object ToplevelSymbolsCache {

  private final case class Format(
    sourceLooseUri: Option[String] = None,
    sourceLastModified: String,
    sourceChecksums: Map[String, String] = Map.empty,
    symbols: Map[String, Seq[String]] = Map.empty
  ) {
    lazy val sourceLastModifiedTime: LocalDateTime =
      LocalDateTime.parse(sourceLastModified, dateTimeFormatter)
    lazy val sourceLastModifiedMillis: Long = sourceLastModifiedTime
      .toInstant(ZoneOffset.UTC)
      .toEpochMilli
    lazy val symbolsAsPaths = symbols.map {
      case (key, stringValues) =>
        key -> stringValues.map(os.SubPath(_))
    }
  }

  private def dateTimeFormatter = DateTimeFormatter.ISO_LOCAL_DATE_TIME

  private val formatCodec: JsonValueCodec[Format] =
    JsonCodecMaker.make(CodecMakerConfig.withMapMaxInsertNumber(Int.MaxValue))

  private def maxAttempts: Int = 4

  abstract class ToplevelSymbolsCacheException(message: String, cause: Throwable = null)
      extends Exception(message, cause)

  final class ReadOnlyCacheException(val missingFile: os.Path)
      extends ToplevelSymbolsCacheException(
        s"Toplevel cache is read-only, cannot compute toplevel symbols for $missingFile"
      )

  final class JsonException(val cacheFile: os.Path, cause: JsonReaderException)
      extends ToplevelSymbolsCacheException(s"Error reading $cacheFile", cause)

}
