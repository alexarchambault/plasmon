package plasmon.ide

import org.eclipse.lsp4j as l
import org.objectweb.asm.{ClassReader, ClassVisitor, Opcodes}
import plasmon.PlasmonEnrichments.*

import scala.meta.dialects
import scala.meta.internal.mtags.{Mtags, SourcePath}
import scala.util.Try
import scala.util.control.NonFatal

/** Locations in archives (`jar:file:///…/foo.jar!/path/in/archive` URIs), as presentation compilers
  * can return them, mapped to files on disk like the other definitions in dependencies.
  *
  * The Scala 3 presentation compiler, when the symbol index doesn't know a symbol (because its
  * sources can't be parsed, say), points at the class file the symbol comes from, in the class path
  * JAR. That's of no use to editors (nor to the code expecting `file:` URIs), so we look for the
  * corresponding source file in the sources JAR instead.
  */
object ArchiveLocations {

  def isArchiveUri(uri: String): Boolean =
    uri.startsWith("jar:")

  /** The file on disk an archive location corresponds to, if any.
    *
    * @param location
    *   a location with a `jar:` URI
    * @param workspace
    *   the workspace, whose `.plasmon/readonly` directory dependency sources get extracted to
    * @param sourcesJar
    *   finds the sources JAR of a class path JAR
    */
  def toFileLocation(
    location: l.Location,
    workspace: os.Path,
    sourcesJar: os.Path => Option[os.Path]
  ): Option[l.Location] =
    SourcePath.withContext { implicit ctx =>
      Try(SourcePath(location.getUri)).toOption.flatMap {
        case entry: SourcePath.ZipEntry =>
          try
            if (isSource(entry.pathInZip))
              Option.when(entry.exists()) {
                val onDisk = entry.toFileOnDisk(workspace)
                new l.Location(onDisk.uri, location.getRange)
              }
            else if (entry.pathInZip.endsWith(".class"))
              fromClassFile(entry, workspace, sourcesJar)
            else
              None
          catch {
            case NonFatal(e) =>
              scribe.warn(s"Error mapping ${location.getUri} to a file on disk", e)
              None
          }
        case _: SourcePath.Standard =>
          None
      }
    }

  private def isSource(pathInZip: String): Boolean =
    pathInZip.endsWith(".scala") || pathInZip.endsWith(".java")

  private def fromClassFile(
    classFile: SourcePath.ZipEntry,
    workspace: os.Path,
    sourcesJar: os.Path => Option[os.Path]
  )(implicit ctx: SourcePath.Context): Option[l.Location] =
    for {
      sourcesJar0    <- sourcesJar(os.Path(classFile.zipPath))
      sourceFileName <- sourceFileName(classFile.rawContent())
      dir = classFile.pathInZip.split('/').init
      sourceEntry = SourcePath.ZipEntry(
        sourcesJar0.toNIO,
        (dir :+ sourceFileName).mkString("/"),
        -1L
      )
      if sourceEntry.exists()
    } yield {
      val onDisk = sourceEntry.toFileOnDisk(workspace)
      val range = classDefinitionRange(sourceEntry, classFile.pathInZip)
        .getOrElse(new l.Range(new l.Position(0, 0), new l.Position(0, 0)))
      new l.Location(onDisk.uri, range)
    }

  /** The name of the source file a class was compiled from, from its `SourceFile` attribute */
  private def sourceFileName(classFileContent: Array[Byte]): Option[String] = {
    var sourceFileOpt = Option.empty[String]
    val visitor = new ClassVisitor(Opcodes.ASM9) {
      override def visitSource(source: String, debug: String): Unit =
        sourceFileOpt = Option(source).filter(name => name.nonEmpty && !name.contains('/'))
    }
    new ClassReader(classFileContent).accept(
      visitor,
      ClassReader.SKIP_CODE | ClassReader.SKIP_FRAMES
    )
    sourceFileOpt
  }

  /** Where the top-level class or object of a class file is defined in its source file.
    *
    * Relies on the top-level indexer only, that doesn't need to parse the whole file (and is fine
    * with syntax that the Scalameta parser rejects).
    */
  private def classDefinitionRange(
    sourceEntry: SourcePath.ZipEntry,
    classPathInZip: String
  )(implicit ctx: SourcePath.Context): Option[l.Range] = {
    val name = classPathInZip.stripSuffix(".class")
    val (owner, simpleName) = name.lastIndexOf('/') match {
      case -1  => ("", name)
      case idx => (name.take(idx + 1), name.drop(idx + 1))
    }
    val toplevelName = simpleName.takeWhile(_ != '$')
    val isObject     = simpleName == toplevelName + "$"
    val classSym     = owner + toplevelName + "#"
    val objectSym    = owner + toplevelName + "."
    val candidates =
      if (isObject) Seq(objectSym, classSym)
      else Seq(classSym, objectSym)

    val doc = Mtags.allToplevels(sourceEntry.toInput, dialects.Scala3, includeMembers = false)
    candidates.iterator
      .flatMap { sym =>
        doc.occurrences.iterator
          .filter(occ => occ.role.isDefinition && occ.symbol == sym)
          .flatMap(_.range.iterator)
      }
      .nextOption()
      .map(_.toLsp)
  }
}
