package plasmon.pc

import ch.epfl.scala.bsp4j as b
import plasmon.PlasmonEnrichments.*
import plasmon.index.BspData

import scala.util.matching.Regex

/** How presentation compilers get to see the workspace's own modules.
  *
  * By default, a module is read from the class directories the build tool reports for it and for
  * the modules it depends on, which means those have to have been built - and rebuilt after every
  * change - for the compiler to know about them. With a source path, the compiler is handed the
  * sources of those modules instead, and reads them itself, on demand: nothing needs to be built.
  */
sealed abstract class SourcepathMode extends Product with Serializable {
  def id: String
}

object SourcepathMode {

  /** `-sourcepath` listing the source files themselves, along with `-Ylogical-package-loading`, so
    * that the package a file contributes to is read from its package clause rather than from its
    * directory. Sources can live anywhere - Scala CLI ones at the root of the project, say.
    */
  case object Logical extends SourcepathMode {
    def id = "logical"
  }

  /** Plain `-sourcepath`, listing source roots: a file has to live in the directory its package
    * clause names, under one of those roots, or the compiler won't find it.
    */
  case object Classic extends SourcepathMode {
    def id = "classic"
  }

  def all: Seq[SourcepathMode] = Seq(Logical, Classic)

  def parse(id: String): Option[SourcepathMode] =
    all.find(_.id == id.trim)
}

/** What a presentation compiler gets instead of built output, for a build target.
  *
  * @param sources
  *   what to pass as `-sourcepath`: files or directories in [[SourcepathMode.Logical]] mode, source
  *   roots in [[SourcepathMode.Classic]] mode
  * @param classDirectories
  *   the class directories to drop from the compiler's class path - those of the target and of the
  *   targets it depends on transitively
  */
final case class Sourcepath(
  mode: SourcepathMode,
  sources: Seq[os.Path],
  classDirectories: Seq[os.Path]
)

object Sourcepath {

  def apply(mode: SourcepathMode, bspData: BspData, id: b.BuildTargetIdentifier): Sourcepath = {
    // includes id itself
    val targets = bspData.buildTargetTransitiveDependencies(id).toVector

    val sourceItems = targets
      .flatMap(bspData.sourceItemsOf(_))
      .distinct
      .filter(os.exists(_))
      .sortBy(_.toString)

    val sources = mode match {
      case SourcepathMode.Logical => sourceItems
      case SourcepathMode.Classic =>
        sourceItems
          .flatMap { item =>
            if (os.isDir(item)) Seq(item)
            else rootOf(item).toSeq
          }
          .distinct
    }

    val classDirectories = targets
      .flatMap { target =>
        bspData.scalaTarget(target).map(_.classDirectory).toSeq ++
          bspData.javaTarget(target).toSeq.flatMap { javaTarget =>
            Option(javaTarget.javac.getClassDirectory)
              .filter(_.nonEmpty)
              .map(_.osPathFromUri)
              .toSeq
          }
      }
      .distinct

    Sourcepath(mode, sources, classDirectories)
  }

  private val packageClause: Regex = """^\s*package\s+([\w.`]+)\s*(?:;|$)""".r

  /** The source root a lone source file would have to be found under, for `-sourcepath` to pick it
    * up: its directory, less the path its package clauses name. `None` when the two don't line up,
    * or when the file cannot be read.
    */
  def rootOf(file: os.Path): Option[os.Path] = {
    val content =
      try Some(os.read(file))
      catch {
        case _: java.io.IOException => None
      }
    content.flatMap { content0 =>
      val segments = content0
        .linesIterator
        .takeWhile(line =>
          line.trim.isEmpty || line.trim.startsWith("package") || line.trim.startsWith("//")
        )
        .flatMap {
          case packageClause(name) => name.split('.').toSeq.map(_.stripPrefix("`").stripSuffix("`"))
          case _                   => Nil
        }
        .toVector
      val dir = file / os.up
      if (segments.isEmpty) Some(dir)
      else if (dir.segments.toVector.endsWith(segments))
        Some((1 to segments.length).foldLeft(dir)((d, _) => d / os.up))
      else None
    }
  }
}
