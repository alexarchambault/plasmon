package plasmon.servercommand

import plasmon.PlasmonEnrichments.{StringThingExtensions, XtensionSourcePathBuffers}

import java.net.URI
import java.nio.file.Paths

import scala.meta.internal.mtags.SourcePath

/** Resolving the file a command acts on, the same way for every command that takes one. */
object FileArg {

  /** Exactly one file, passed either as a path argument or as `--uri`.
    *
    * Either can also point inside an archive, the way `lsp definition --archive-uris` prints
    * locations in dependencies (`…/foo-sources.jar!path/in/archive`, or
    * `file:///…/foo-sources.jar!path/in/archive`), or as a `jar:file:///…/foo.jar!/path/in/archive`
    * URI. The file is then the entry extracted under the workspace, like the server does for the
    * definitions it returns.
    */
  def single(
    args: Seq[String],
    uriOpt: Option[String],
    workingDir: os.Path
  ): (os.Path, String) =
    (args, uriOpt) match {
      case (Seq(), None) =>
        sys.error("No file specified")
      case (Seq(strPath), None) =>
        val path = archiveEntry(strPath, workingDir)
          .map(extract(_, workingDir))
          .getOrElse(os.Path(strPath, workingDir))
        (path, path.toNIO.toUri.toASCIIString)
      case (Seq(), Some(uri)) =>
        val entryOpt =
          if (uri.startsWith("jar:"))
            SourcePath(uri) match {
              case z: SourcePath.ZipEntry => Some(z)
              case _: SourcePath.Standard => None
            }
          else if (uri.startsWith("file:"))
            archiveEntry(Paths.get(new URI(uri)).toString, workingDir)
          else
            None
        entryOpt match {
          case Some(entry) =>
            val path = extract(entry, workingDir)
            (path, path.toNIO.toUri.toASCIIString)
          case None =>
            (uri.osPathFromUri, uri)
        }
      case (Seq(_), Some(_)) =>
        sys.error("Cannot specify both a file and a URI")
      case (other, _) =>
        assert(other.length > 1)
        sys.error("Too many files specified (only one file accepted)")
    }

  /** `/…/foo.jar!path/in/archive`, when `/…/foo.jar` is a file and `/…/foo.jar!path` isn't.
    *
    * Checking what exists on disk, as file names can have a `!` in them.
    */
  private def archiveEntry(path: String, workingDir: os.Path): Option[SourcePath.ZipEntry] =
    if (os.exists(os.Path(path, workingDir))) None
    else
      Iterator
        .iterate(path.indexOf('!'))(idx => path.indexOf('!', idx + 1))
        .takeWhile(_ >= 0)
        .map(idx => (path.take(idx), path.drop(idx + 1).replace('\\', '/').stripPrefix("/")))
        .collectFirst {
          case (archive, inArchive)
              if inArchive.nonEmpty && os.isFile(os.Path(archive, workingDir)) =>
            SourcePath.ZipEntry(os.Path(archive, workingDir).toNIO, inArchive, -1L)
        }

  /** Where the server keeps its copy of an archive entry - the same place it points at in the
    * definitions it returns - extracting it there if needed.
    */
  private def extract(entry: SourcePath.ZipEntry, workingDir: os.Path): os.Path =
    SourcePath.withContext { implicit ctx =>
      if (!entry.exists())
        sys.error(s"${entry.pathInZip} not found in ${entry.zipPath}")
      os.Path(entry.toFileOnDisk(workingDir).path)
    }

  /** At most one file, passed either as a path argument or as `--uri`. */
  def optional(
    args: Seq[String],
    uriOpt: Option[String],
    workingDir: os.Path
  ): Option[os.Path] =
    (args, uriOpt) match {
      case (Seq(), None) => None
      case _             => Some(single(args, uriOpt, workingDir)._1)
    }
}
