package plasmon.render

import ch.epfl.scala.{bsp4j => b}
import com.github.plokhotnyuk.jsoniter_scala.core.*
import com.google.gson.GsonBuilder
import org.eclipse.{lsp4j => l}

import java.nio.charset.StandardCharsets

import scala.reflect.ClassTag
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker

object JsonCodecs {

  private lazy val gson = new GsonBuilder().create()
  def gsonCodec[T <: Object: ClassTag]: JsonValueCodec[T] =
    new JsonValueCodec[T] {
      override def nullValue: T = null.asInstanceOf[T]
      override def encodeValue(x: T, out: JsonWriter): Unit = {
        val b = gson.toJson(x).getBytes(StandardCharsets.UTF_8)
        out.writeRawVal(b)
      }
      override def decodeValue(in: JsonReader, default: T): T = {
        val b = in.readRawValAsBytes()
        gson.fromJson(
          new String(b, StandardCharsets.UTF_8),
          summon[ClassTag[T]].runtimeClass.asInstanceOf[Class[T]]
        )
      }
    }

  given JsonValueCodec[b.InitializeBuildResult] =
    gsonCodec
  given JsonValueCodec[b.BuildTarget] =
    gsonCodec
  given JsonValueCodec[b.ScalaBuildTarget] =
    gsonCodec
  given JsonValueCodec[b.JavacOptionsItem] =
    gsonCodec
  given JsonValueCodec[b.ScalacOptionsItem] =
    gsonCodec
  given JsonValueCodec[b.DependencySourcesItem] =
    gsonCodec
  given JsonValueCodec[b.MavenDependencyModule] =
    gsonCodec
  given JsonValueCodec[b.WorkspaceBuildTargetsResult] =
    gsonCodec
  given JsonValueCodec[b.CompileResult] =
    gsonCodec
  given JsonValueCodec[b.BuildTargetIdentifier] =
    stringCodec.bimap(
      new b.BuildTargetIdentifier(_),
      _.getUri
    )
  given JsonValueCodec[l.InitializeParams] =
    gsonCodec
  given JsonValueCodec[l.Diagnostic] =
    gsonCodec

  lazy val stringCodec =
    new JsonValueCodec[String] {
      override def nullValue: String = null.asInstanceOf[String]
      override def encodeValue(x: String, out: JsonWriter): Unit =
        if (x == null) out.writeNull()
        else out.writeVal(x)
      override def decodeValue(in: JsonReader, default: String): String =
        in.readString(default)
    }

  given JsonValueCodec[os.Path] =
    stringCodec.bimap(
      s => Option(s).map(os.Path(_)).orNull,
      p => Option(p).map(_.toString).orNull
    )
  given JsonValueCodec[os.SubPath] =
    stringCodec.bimap(
      s => Option(s).map(os.SubPath(_)).orNull,
      p => Option(p).map(_.toString).orNull
    )

  extension [T <: AnyRef](codec: JsonValueCodec[T])
    def bimap[U <: AnyRef](from: T => U, to: U => T): JsonValueCodec[U] =
      new JsonValueCodec[U] {
        override def nullValue: U =
          Option(codec.nullValue).map(from).getOrElse(null.asInstanceOf[U])
        override def encodeValue(x: U, out: JsonWriter): Unit =
          codec.encodeValue(to(x), out)
        override def decodeValue(in: JsonReader, default: U): U =
          from(codec.decodeValue(in, Option(default).map(to).getOrElse(null.asInstanceOf[T])))
      }

}
