// Originally based on https://github.com/scalameta/metals/blob/0ad0bc184f82dbd178d01f76913ea6bdfa98db14/metals/src/main/scala/scala/meta/internal/parsing/Trees.scala

package plasmon.ide

import scala.collection.concurrent.TrieMap
import scala.reflect.ClassTag

import scala.meta._
import scala.meta.inputs.Input
import scala.meta.inputs.Position
import scala.meta.parsers.ParseException
import scala.meta.parsers.Parsed
import scala.meta.tokens.Tokens

import org.eclipse.{lsp4j => l}
import scala.meta.internal.mtags.SourcePath

import plasmon.PlasmonEnrichments._
import scala.meta.internal.mtags.GlobalSymbolIndex
import plasmon.index.BspData
import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker

/** Manages parsing of Scala source files into Scalameta syntax trees.
  *
  *   - provides the latest good Scalameta tree for a given source file similar as `Buffers`
  *     provides the current text content.
  *   - produces diagnostics for syntax errors.
  */
final class Trees(
  buffers: Buffers,
  bspData: BspData
) {

  private val trees = TrieMap.empty[os.Path, Tree]

  def get(module: GlobalSymbolIndex.Module, path: os.Path)(implicit
    ctx: SourcePath.Context
  ): Option[Tree] =
    get(module, SourcePath.Standard(path.toNIO))

  private def get(module: GlobalSymbolIndex.Module, path: SourcePath)(implicit
    ctx: SourcePath.Context
  ): Option[Tree] =
    path.filePath match {
      case Some(p) =>
        val p0 = os.Path(p)
        trees.get(p0).orElse {
          bspData.getDialect(module, p0).flatMap { dialect =>
            // Fallback to parse without caching result.
            parse(path, dialect).flatMap(_.toOption)
          }
        }
      case None =>
        bspData.getDialect(module, path).flatMap { dialect =>
          parse(path, dialect).flatMap(_.toOption)
        }
    }

  def didClose(fileUri: os.Path): Unit =
    trees.remove(fileUri)

  private def enclosedChildren(
    children: List[Tree],
    pos: Position
  ): List[Tree] =
    children.filter { child =>
      child.pos.start <= pos.start && pos.start <= child.pos.end
    }

  /** Find last tree matching T that encloses the position.
    *
    * @param source
    *   source to load the tree for
    * @param lspPos
    *   cursor position
    * @param predicate
    *   predicate which T must fulfill
    * @return
    *   found tree node of type T or None
    */
  def findLastEnclosingAt[T <: Tree: ClassTag](
    module: GlobalSymbolIndex.Module,
    source: os.Path,
    lspPos: l.Position,
    predicate: T => Boolean = (_: T) => true
  )(implicit ctx: SourcePath.Context): Option[T] = {

    def loop(t: Tree, pos: Position): Option[T] =
      t match {
        case t: T =>
          enclosedChildren(t.children, pos)
            .flatMap(loop(_, pos).toList)
            .headOption
            .orElse(if (predicate(t)) Some(t) else None)
        case other =>
          enclosedChildren(other.children, pos)
            .flatMap(loop(_, pos).toList)
            .headOption
      }

    for {
      tree    <- get(module, source)
      pos     <- lspPos.toMeta(tree.pos.input)
      lastEnc <- loop(tree, pos)
    } yield lastEnc
  }

  /** Parse file at the given path and return a list of errors if there are any.
    *
    * @param path
    *   file to parse
    * @return
    *   list of errors if the file failed to parse
    */
  def didChange(
    dialect: Dialect,
    path: os.Path
  ): List[l.Diagnostic] =
    parse(
      SourcePath.Standard(path.toNIO),
      dialect
    )(using /* unused for std path */ new SourcePath.Context) match {
      case Some(parsed) =>
        parsed match {
          case Parsed.Error(pos, message, _) =>
            List(
              new l.Diagnostic(
                pos.toLsp,
                message,
                l.DiagnosticSeverity.Error,
                "scalameta"
              )
            )
          case Parsed.Success(tree) =>
            trees(path) = tree
            Nil
          case _ =>
            Nil
        }
      case _ => Nil
    }

  def tokenized(module: GlobalSymbolIndex.Module, path: os.Path): Option[Tokens] =
    for {
      sourceText <- buffers.get(path)
      virtualFile = Input.VirtualFile(path.toNIO.toUri.toASCIIString, sourceText)
      tokens <- tokenized(module, virtualFile).flatMap(_.toOption)
    } yield tokens

  def tokenized(
    module: GlobalSymbolIndex.Module,
    input: inputs.Input.VirtualFile
  ): Option[Tokenized] =
    bspData.getDialect(module, input.path.osPathFromUri).map { dialect =>
      input.value.safeTokenize(using dialect)
    }

  private def parse(
    path: SourcePath,
    dialect: Dialect
  )(implicit ctx: SourcePath.Context): Option[Parsed[Tree]] =
    for {
      text <-
        path.filePath
          .map(os.Path(_))
          .flatMap(buffers.get(_))
          .orElse(path.readTextOpt)
    } yield try {
      val skipFistShebang =
        if (text.startsWith("#!")) text.replaceFirst("#!", "//") else text
      val input = Input.VirtualFile(path.uri, skipFistShebang)
      if (path.isMill) {
        val ammoniteInput = Input.Ammonite(input)
        ammoniteInput.safeParse[MultiSource](dialect)
      }
      else
        input.safeParse[Source](dialect)
    }
    catch {
      // if the parsers breaks we should not throw the exception further
      case _: StackOverflowError =>
        val msg = s"Could not parse $path"
        scribe.warn(msg)
        Parsed.Error(
          Position.None,
          msg,
          new ParseException(Position.None, msg)
        )
    }

  def asJson: Trees.AsJson =
    Trees.AsJson(
      trees = trees.toMap.map {
        case (k, v) =>
          (k.toString, v.toString)
      }
    )
}

object Trees {
  final case class AsJson(
    trees: Map[String, String]
  )

  given JsonValueCodec[AsJson] =
    JsonCodecMaker.make
}
