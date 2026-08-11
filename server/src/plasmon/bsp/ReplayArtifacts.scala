package plasmon.bsp

import plasmon.Logger

import scala.util.control.NonFatal

/** Fetches the artifacts a recording refers to but the local caches don't hold yet.
  *
  * A recording is made on a machine where a real build tool has just run, so every jar it names is
  * in the coursier cache there. Replaying it somewhere else - another developer, or CI, where no
  * build tool runs at all - finds those paths empty. Since a coursier cache path maps straight back
  * to the URL it came from, we can simply fetch them.
  */
object ReplayArtifacts {

  /** Ensures the jars `dataDir`'s recordings point at are present locally.
    *
    * Only the plain coursier cache is filled. JDK homes are recorded as `$JAVA_HOME_URI:<major>$`
    * rather than archive-cache paths (see `BspDataPortability.replaceJavaHomes`), and replay uses
    * the server's JVM anyway. Pulling a couple of hundred megabytes of JDK per job to satisfy a
    * path nothing reads would be a poor trade.
    */
  def fetchMissing(dataDir: os.Path, roots: BspDataPortability.Roots, logger: Logger): Unit = {
    val texts =
      if (os.isDir(dataDir))
        os.list(dataDir).filter(os.isFile).filter(_.last.endsWith(".json")).map(os.read)
      else Nil

    val missing = texts
      .flatMap(references(_, "COURSIER_CACHE"))
      .distinct
      .filter(ref => !os.exists(localPath(ref, roots, "COURSIER_CACHE")))

    if (missing.nonEmpty) {
      logger.log(
        s"Replay: fetching ${missing.length} artifact(s) the recording refers to " +
          "but the local cache doesn't have"
      )
      for (ref <- missing)
        fetch(urlOf(ref), logger) { url =>
          coursierapi.Cache.create().get(coursierapi.Artifact.of(url))
        }
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
        logger.log(s"Replay: could not fetch $url ($e)")
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
