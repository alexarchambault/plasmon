package plasmon.ide

import java.net.URI

/** The file recording which archive a directory under [[Directories.dependencies]] was extracted
  * from - its last modified time, so that a changed archive gets extracted anew, and its path.
  */
object JarMeta {
  val fileName = ".jar.meta"

  def content(jar: os.Path): String =
    s"${os.mtime(jar)}\n$jar"

  def read(jarDir: os.Path): Option[String] = {
    val file = jarDir / fileName
    Option.when(os.exists(file))(os.read(file))
  }

  /** The archive a file under [[Directories.dependencies]] was extracted from, along with its path
    * in that archive.
    *
    * `None` for files elsewhere, or when the archive is not known (nothing recorded, or recorded by
    * something that predates [[JarMeta]]).
    */
  def originalArchive(extracted: os.Path): Option[(os.Path, os.SubPath)] = {
    val depsSegments = Directories.dependencies.segments
    val segments     = extracted.segments.toVector
    Some(segments.indexOfSlice(depsSegments))
      .filter(_ >= 0)
      .map(_ + depsSegments.length)
      .filter(_ + 1 < segments.length) // a jar dir name, and something in it
      .flatMap { jarDirIdx =>
        val jarDir    = os.Path(extracted.toNIO.getRoot) / segments.take(jarDirIdx + 1)
        val inArchive = os.SubPath(segments.drop(jarDirIdx + 1))
        read(jarDir)
          .flatMap(_.linesIterator.drop(1).nextOption())
          .filter(_.nonEmpty)
          .map(jar => (os.Path(jar), inArchive))
      }
  }

  /** `file:///…/foo-sources.jar!path/in/archive` for a file extracted under
    * [[Directories.dependencies]], `None` for any other file.
    */
  def archiveUri(extracted: os.Path): Option[String] =
    originalArchive(extracted).map {
      case (jar, inArchive) =>
        val rawInArchive = new URI(null, null, "/" + inArchive.toString, null).toASCIIString
        jar.toNIO.toUri.toASCIIString + "!" + rawInArchive.stripPrefix("/")
    }

  /** `/…/foo-sources.jar!path/in/archive`, the counterpart of [[archiveUri]] for humans. */
  def archivePath(extracted: os.Path): Option[String] =
    originalArchive(extracted).map {
      case (jar, inArchive) =>
        s"$jar!$inArchive"
    }
}
