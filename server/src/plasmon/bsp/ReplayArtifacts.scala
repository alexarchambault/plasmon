package plasmon.bsp

import plasmon.Logger

import scala.util.control.NonFatal
import scala.util.matching.Regex

/** Fetches the artifacts a recording refers to but the local caches don't hold yet.
  *
  * A recording is made on a machine where a real build tool has just run, so every jar it names is
  * in the coursier cache there. Replaying it somewhere else - another developer, or CI, where no
  * build tool runs at all - finds those paths empty. Since a coursier cache path maps straight back
  * to the URL it came from, we can simply fetch them.
  *
  * The same goes for the BSP data [[plasmon.index.IndexerActor]] caches, which uses the same
  * layout: the coursier cache may have been cleaned since it was written.
  */
object ReplayArtifacts {

  /** Ensures the jars `dataDir`'s recordings point at are present locally.
    *
    * Only the plain coursier cache is filled. JDK homes are recorded as `$JAVA_HOME_URI:<major>$`
    * rather than archive-cache paths (see `BspDataPortability.replaceJavaHomes`), and replay uses
    * the server's JVM anyway. Pulling a couple of hundred megabytes of JDK per job to satisfy a
    * path nothing reads would be a poor trade.
    *
    * @return
    *   the entries still missing afterwards, as cache-relative paths, e.g.
    *   `https/repo1.maven.org/maven2/…/foo.jar`
    */
  def fetchMissing(
    dataDir: os.Path,
    roots: BspDataPortability.Roots,
    logger: Logger
  ): Seq[String] = {
    def missing() =
      references(dataDir, roots)
        .filter(ref => !os.exists(localPath(ref, roots, "COURSIER_CACHE")))

    val missing0 = missing()
    if (missing0.isEmpty) Nil
    else {
      logger.log(
        s"Fetching ${missing0.length} artifact(s) referenced in $dataDir " +
          "that the local cache doesn't have"
      )
      for (ref <- missing0)
        fetch(urlOf(ref), logger) { url =>
          coursierapi.Cache.create().get(coursierapi.Artifact.of(url))
        }
      missing()
    }
  }

  private def references(dataDir: os.Path, roots: BspDataPortability.Roots): Seq[String] = {
    val texts =
      if (os.isDir(dataDir))
        os.list(dataDir).filter(os.isFile).filter(_.last.endsWith(".json")).map(os.read)
      else Nil

    texts.map(substituteSbtBootJars(_, roots)).flatMap(references(_, "COURSIER_CACHE")).distinct
  }

  /** Swaps the Scala jars of sbt's boot directory for the same jars in the coursier cache, if they
    * aren't on this machine.
    *
    * When a project uses the Scala version sbt itself runs on (2.12.21 for sbt 1.12), sbt hands out
    * the jars of its boot directory, `~/.sbt/boot/scala-<version>/lib`, rather than coursier cache
    * ones - for the project's class path and for its compiler jars alike. A machine that never ran
    * that sbt version, like CI replaying a recording, doesn't have them. They are plain copies of
    * the Maven Central artifacts, so point at those instead, which [[fetchMissing]] then fetches.
    *
    * Recordings keep the boot paths, which is what sbt really reports: this is applied when reading
    * them, on recorded (placeholder) strings, before [[BspDataPortability.Roots.denormalize]].
    *
    * Boot jars that exist locally are left alone, so that a machine that has them (or is offline)
    * reads recordings exactly as before.
    */
  def substituteSbtBootJars(str: String, roots: BspDataPortability.Roots): String =
    if (!str.contains(sbtBootMarker)) str
    else
      sbtBootScalaJar.replaceAllIn(
        str,
        m => {
          val replacement = mavenCentralRef(m.group(1), m.group(2)) match {
            case Some(ref) if !os.exists(os.Path(roots.denormalize(m.matched))) =>
              s"$$COURSIER_CACHE$$/$ref"
            case _ => m.matched
          }
          Regex.quoteReplacement(replacement)
        }
      )

  private def sbtBootMarker = "$HOME$/.sbt/boot/scala-"

  private val sbtBootScalaJar = """\$HOME\$/\.sbt/boot/scala-([^/"\\]+)/lib/([^/"\\]+)\.jar""".r

  private val scalaModule = """(scala-[a-z-]+_[0-9.]+)-([^_]+)""".r

  /** Cache-relative path of the Maven Central artifact a jar of `~/.sbt/boot/scala-<v>/lib` is a
    * copy of, if we know which one that is.
    */
  private def mavenCentralRef(scalaVersion: String, name: String): Option[String] = {
    val central = "https/repo1.maven.org/maven2"
    name match {
      case "scala-library" | "scala-compiler" | "scala-reflect" =>
        Some(s"$central/org/scala-lang/$name/$scalaVersion/$name-$scalaVersion.jar")
      case scalaModule(artifact, version) =>
        Some(s"$central/org/scala-lang/modules/$artifact/$version/$artifact-$version.jar")
      case _ => None
    }
  }

  private def fetch(url: String, logger: Logger)(get: String => java.io.File): Unit =
    try {
      get(url)
      ()
    }
    catch {
      case NonFatal(e) =>
        // Not fatal in itself - whatever needed the artifact will fail with a clearer message, and
        // some recorded entries (a build tool's own scratch files, say) were never downloads
        logger.log(s"Could not fetch $url ($e)")
    }

  /** Cache-relative paths, e.g. `https/repo1.maven.org/maven2/…/foo.jar`, under the named root. */
  private def references(text: String, rootName: String): Seq[String] = {
    val marker = s"$$$rootName$$/"
    val res    = Seq.newBuilder[String]
    var from   = text.indexOf(marker)
    while (from >= 0) {
      val start = from + marker.length
      val end   = text.indexWhere(c => c == '"' || c == '\\', start)
      val ref   = (if (end < 0) text.drop(start) else text.substring(start, end)).stripSuffix("/")
      if (ref.nonEmpty) res += ref
      from = text.indexOf(marker, start)
    }
    res.result().distinct
  }

  /** `https/host/a/b.jar` back to `https://host/a/b.jar`.
    *
    * The cache escapes `%` in a file name as `%25`, so undo that or the URL comes back
    * double-encoded (a `+` stored as `%252B` would be asked for as `%25252B`).
    */
  private def urlOf(ref: String): String =
    ref.split("/").toList match {
      case protocol :: rest if rest.nonEmpty =>
        s"$protocol://${rest.mkString("/").replace("%25", "%")}"
      case _ => ref
    }

  private def localPath(
    ref: String,
    roots: BspDataPortability.Roots,
    rootName: String
  ): os.Path =
    os.Path(roots.denormalize(s"$$$rootName$$/$ref"))
}
