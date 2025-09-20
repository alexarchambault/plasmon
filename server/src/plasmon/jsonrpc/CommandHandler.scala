package plasmon.jsonrpc

import com.google.gson.JsonPrimitive
import org.eclipse.{lsp4j => l}

import java.net.URI
import java.nio.file.InvalidPathException
import java.util.concurrent.CompletableFuture

import scala.jdk.CollectionConverters._
import com.google.gson.Gson
import com.google.gson.JsonElement
import scala.reflect.ClassTag
import com.github.plokhotnyuk.jsoniter_scala.core._
import plasmon.Logger
import plasmon.PlasmonEnrichments.StringThingExtensions

final case class CommandHandler(
  commandName: String,
  call: (l.ExecuteCommandParams, Logger) => CompletableFuture[Object],
  refreshStatus: Boolean
)

object CommandHandler {
  def of(
    commandName: String,
    refreshStatus: Boolean = false
  )(call: (l.ExecuteCommandParams, Logger) => CompletableFuture[Object]): CommandHandler =
    CommandHandler(commandName, call, refreshStatus)

  object ParamsHelpers {

    final case class GsonValue[+T](value: T)

    trait ArgParser[+T] {
      def parse(obj: Object): Either[String, T]

      final def map[U](f: T => U): ArgParser[U] =
        new ArgParser.Mapped(this, f)
      final def mapE[U](f: T => Either[String, U]): ArgParser[U] =
        new ArgParser.MappedE(this, f)
    }

    object ArgParser {

      def apply[T]()(implicit parser: ArgParser[T]): ArgParser[T] =
        parser

      private final class Mapped[T, +U](underlying: ArgParser[T], f: T => U) extends ArgParser[U] {
        def parse(obj: Object): Either[String, U] =
          underlying.parse(obj).map(f)
      }
      private final class MappedE[T, +U](underlying: ArgParser[T], f: T => Either[String, U])
          extends ArgParser[U] {
        def parse(obj: Object): Either[String, U] =
          underlying.parse(obj).flatMap(f)
      }

      implicit lazy val string: ArgParser[String] = {
        case p: JsonPrimitive if p.isString => Right(p.getAsString)
        case other                          => Left(s"Expected string, got '$other'")
      }
      implicit lazy val boolean: ArgParser[Boolean] = {
        case p: JsonPrimitive if p.isBoolean => Right(p.getAsBoolean)
        case other                           => Left(s"Expected boolean, got '$other'")
      }
      implicit lazy val path: ArgParser[os.FilePath] =
        string.mapE { str =>
          try Right(os.FilePath(str))
          catch {
            case e: InvalidPathException =>
              Left(s"Error parsing path '$str': $e")
          }
        }
      implicit def gson[T: JsonValueCodec: ClassTag](implicit
        gson: Gson = new Gson
      ): ArgParser[GsonValue[T]] = {
        case obj: JsonElement if obj.isJsonObject =>
          try Right(GsonValue(readFromString[T](gson.toJson(obj))))
          catch {
            case ex: JsonReaderException =>
              Left(s"Expected ${implicitly[ClassTag[T]].runtimeClass.getName}, got '$obj' ($ex)")
          }
        case other =>
          Left(s"Expected ${implicitly[ClassTag[T]].runtimeClass.getName}, got '$other'")
      }
    }

    implicit class ExecuteCommandParamsParsers(private val params: l.ExecuteCommandParams)
        extends AnyVal {

      def arguments: Seq[Object] =
        Option(params.getArguments).map(_.asScala.toVector).getOrElse(Nil)

      private def error(commandName: String, errors: Seq[String]): CompletableFuture[Object] = {
        // FIXME Trapped error… (for users)
        scribe.warn(s"Error parsing $commandName arguments: ${errors.mkString(", ")}")
        CompletableFuture.completedFuture(null)
      }

      def as[T: ArgParser](commandName: String)(f: T => CompletableFuture[Object])
        : CompletableFuture[Object] =
        arguments match {
          case Seq() => error(commandName, Seq("No argument passed"))
          case Seq(arg) =>
            ArgParser[T]().parse(arg) match {
              case Left(msg) => error(commandName, Seq(msg))
              case Right(t)  => f(t)
            }
          case _ => error(commandName, Seq("Too many arguments passed, expected one"))
        }

      def asFileUri(commandName: String)(f: os.Path => CompletableFuture[Object])
        : CompletableFuture[Object] =
        as[String](commandName) { uri =>
          // TODO Catch more errors
          f(uri.osPathFromUri)
        }

      def asOpt[T: ArgParser](commandName: String)(f: Option[T] => CompletableFuture[Object])
        : CompletableFuture[Object] =
        arguments match {
          case Seq() => f(None)
          case Seq(arg) =>
            ArgParser[T]().parse(arg) match {
              case Left(msg) => error(commandName, Seq(msg))
              case Right(t)  => f(Some(t))
            }
          case _ => error(commandName, Seq("Too many arguments passed, expected one"))
        }

      def asSeq[T: ArgParser](commandName: String)(f: Seq[T] => CompletableFuture[Object])
        : CompletableFuture[Object] = {
        val parsed = arguments.map(ArgParser[T]().parse(_))
        if (parsed.forall(_.isRight)) {
          val values = parsed.collect { case Right(t) => t }
          f(values)
        }
        else {
          val errors = parsed.collect { case Left(err) => err }
          error(commandName, errors)
        }
      }

      def asValues[A: ArgParser, B: ArgParser](commandName: String)(f: (
        A,
        B
      ) => CompletableFuture[Object]): CompletableFuture[Object] =
        arguments match {
          case Seq()  => error(commandName, Seq("No argument passed"))
          case Seq(_) => error(commandName, Seq("Not enough argument passed (expected 2)"))
          case Seq(arg0, arg1) =>
            ArgParser[A]().parse(arg0) match {
              case Left(msg) => error(commandName, Seq(msg))
              case Right(a) =>
                ArgParser[B]().parse(arg1) match {
                  case Left(msg) => error(commandName, Seq(msg))
                  case Right(b) =>
                    f(a, b)
                }
            }
          case _ => error(commandName, Seq("Too many arguments passed (expected 2)"))
        }

      def asValues[A: ArgParser, B: ArgParser, C: ArgParser](commandName: String)(f: (
        A,
        B,
        C
      ) => CompletableFuture[Object]): CompletableFuture[Object] =
        arguments match {
          case Seq()     => error(commandName, Seq("No argument passed"))
          case Seq(_)    => error(commandName, Seq("Not enough argument passed (expected 3)"))
          case Seq(_, _) => error(commandName, Seq("Not enough argument passed (expected 3)"))
          case Seq(arg0, arg1, arg2) =>
            ArgParser[A]().parse(arg0) match {
              case Left(msg) => error(commandName, Seq(msg))
              case Right(a) =>
                ArgParser[B]().parse(arg1) match {
                  case Left(msg) => error(commandName, Seq(msg))
                  case Right(b) =>
                    ArgParser[C]().parse(arg2) match {
                      case Left(msg) => error(commandName, Seq(msg))
                      case Right(c) =>
                        f(a, b, c)
                    }
                }
            }
          case _ => error(commandName, Seq("Too many arguments passed (expected 3)"))
        }
    }
  }
}
