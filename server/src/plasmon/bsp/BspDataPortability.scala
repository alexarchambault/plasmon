package plasmon.bsp

import com.google.gson.{JsonArray, JsonElement, JsonObject, JsonPrimitive}

import scala.jdk.CollectionConverters.*

/** Makes recorded BSP responses portable across machines and operating systems.
  *
  * BSP responses are full of absolute paths - class directories, class path entries, source and
  * dependency source locations. Recording them as-is would tie a recording to the machine that made
  * it. Instead, we swap the handful of roots those paths live under (the workspace, the coursier
  * caches) for placeholders when writing, and swap them back when reading. JDK homes are handled
  * separately: a path that is actually a JDK is replaced with `$JAVA_HOME_URI:<major>$` (see
  * [[replaceJavaHomes]]), so a recording made against a Linux x64 Temurin 17 still compares equal
  * to one made against the macOS or aarch64 build of the same major version.
  *
  * Nearly everything BSP exchanges is a `file:` URI, which uses `/` on every platform, so a
  * recording made on Linux replays fine on Windows. Plain (non-URI) paths are handled too, on a
  * best-effort basis: they are stored `/`-separated and get the platform separator back on the way
  * out.
  */
object BspDataPortability {

  /** @param values
    *   named roots (`COURSIER_CACHE` → `$COURSIER_CACHE$`), one path per name - the one a
    *   placeholder expands back to on this machine.
    * @param extraSpellings
    *   other paths that stand for an already-named root, e.g. the capital-`Cache` spelling of the
    *   Windows coursier cache. Recognised when recording only: a placeholder has to expand back to
    *   a single path, and that is what [[values]] is for.
    * @param aliases
    *   extra paths that should look like an already-named placeholder, e.g. Windows
    *   `%LOCALAPPDATA%\sbt` recorded as `$HOME$/.sbt`. Used both ways, so only add one where the
    *   placeholder really does name that path on this platform.
    */
  final case class Roots(
    values: Seq[(String, os.Path)],
    extraSpellings: Seq[(String, os.Path)] = Nil,
    aliases: Seq[(String, os.Path)] = Nil
  ) {

    private def named(pairs: Seq[(String, os.Path)]): Seq[(os.Path, String, String, String)] =
      pairs.map { case (name, path) =>
        (path, s"file:///$$$name$$", s"file:/$$$name$$", s"$$$name$$")
      }

    private def aliased: Seq[(os.Path, String, String, String)] =
      aliases.map { case (placeholder, path) =>
        (path, s"file:///$placeholder", s"file:/$placeholder", placeholder)
      }

    /** Longest first, so that nested roots (a coursier cache under the home directory, say) don't
      * get shadowed by their parent.
      */
    private def sortByPathLength(entries: Seq[(os.Path, String, String, String)])
      : Seq[(os.Path, String, String, String)] =
      entries.sortBy { case (path, _, _, _) => -path.toString.length }

    private lazy val toPlaceholders: Seq[(os.Path, String, String, String)] =
      sortByPathLength(named(values ++ extraSpellings) ++ aliased)

    private lazy val fromPlaceholders: Seq[(os.Path, String, String, String)] =
      sortByPathLength(named(values) ++ aliased)

    /** The forms a root shows up in, most specific first.
      *
      * BSP responses mix two spellings of a `file:` URI - `file:///p` (`Path.toUri`) and `file:/p`
      * (`File.toURI`) - and plain paths. Each gets its own placeholder so that the spelling
      * survives the round trip, on whichever platform the recording is replayed.
      */
    private def forms(path: os.Path, fileTriple: String, fileSingle: String, plain: String)
      : Seq[(String, String)] =
      Seq(
        path.toNIO.toUri.toASCIIString.stripSuffix("/") -> fileTriple,
        path.toIO.toURI.toASCIIString.stripSuffix("/")  -> fileSingle,
        path.toString                                   -> plain
      )

    def normalize(str: String): String = {
      var res         = str
      var substituted = false
      for {
        (path, fileTriple, fileSingle, plain) <- toPlaceholders
        (actual, placeholder)                 <- forms(path, fileTriple, fileSingle, plain)
      } {
        val next = res.replace(actual, placeholder)
        if (next != res) substituted = true
        res = next
      }
      // Plain paths keep the platform separator after the root is swapped out (`-Xplugin:…`,
      // Mill's `SOURCECODE_ORIGINAL_FILE_PATH=…`). Recordings use `/` so Linux and Windows match.
      if (substituted && java.io.File.separatorChar != '/' && !res.startsWith("file:"))
        res = res.replace('\\', '/')
      res
    }

    def denormalize(str: String): String = {
      var res         = str
      var substituted = false
      for {
        (path, fileTriple, fileSingle, plain) <- fromPlaceholders
        (actual, placeholder)                 <- forms(path, fileTriple, fileSingle, plain)
        if res.contains(placeholder)
      } {
        res = res.replace(placeholder, actual)
        substituted = true
      }
      // Plain paths are recorded '/'-separated, so that a recording made on one platform replays on
      // another; give them back the platform separator. URIs use '/' everywhere and are left alone,
      // as is anything that held no placeholder at all and so isn't ours to rewrite.
      if (substituted && java.io.File.separatorChar != '/' && !res.startsWith("file:"))
        res = res.replace('/', java.io.File.separatorChar)
      res
    }
  }

  object Roots {
    def apply(
      workspace: os.Path,
      coursierCache: os.Path,
      coursierArchiveCache: os.Path
    ): Roots = {
      // sbt's lmcoursier on Windows writes to `%LOCALAPPDATA%\Coursier\Cache\v1` (capital Cache);
      // coursier-cli / setup-action use `cache` (lowercase). The API location is only one of
      // those. List both, plus the Unix-style default, so a recording does not depend on which
      // process populated the cache. They are recording-only spellings: substitution is by string,
      // which is case-sensitive even where the file system is not, but `$COURSIER_CACHE$` still
      // has to expand back to the one location the API reports.
      val localAppData = os.home / "AppData/Local"
      val extraCaches = Seq(
        localAppData / "Coursier/Cache/v1",
        localAppData / "Coursier/cache/v1",
        os.home / ".cache/coursier/v1"
      )
      val extraArcs = Seq(
        localAppData / "Coursier/Cache/arc",
        localAppData / "Coursier/cache/arc",
        os.home / ".cache/coursier/arc"
      )

      Roots(
        values = Seq(
          "WORKSPACE"              -> workspace,
          "COURSIER_CACHE"         -> coursierCache,
          "COURSIER_ARCHIVE_CACHE" -> coursierArchiveCache,
          "HOME"                   -> os.home
        ),
        extraSpellings = extraCaches.map("COURSIER_CACHE" -> _)
          ++ extraArcs.map("COURSIER_ARCHIVE_CACHE" -> _),
        // Windows sbt boot lives under %LOCALAPPDATA%\sbt, not ~/.sbt. Record it as the latter so
        // a Linux recording and a Windows one compare equal, and expand it back to the Windows
        // location when replaying there. Only there - anywhere else `$HOME$/.sbt` is already the
        // real path, and rewriting it would send readers to a directory that does not exist.
        aliases =
          if (scala.util.Properties.isWin) Seq("$HOME$/.sbt" -> (localAppData / "sbt"))
          else Nil
      )
    }

    /** Note what is deliberately *not* a root here: the JDK the server itself runs on.
      *
      * Reading it would mean `sys.props("java.home")`, which a native image doesn't set - so the
      * native launcher would either fail outright or, worse, normalise differently from the JVM
      * launcher and be unable to restore recordings the other one made. The JDKs that actually turn
      * up in BSP data are whatever the build asked for (or the build tool itself is running on),
      * and those differ by OS and architecture even at the same major version. [[replaceJavaHomes]]
      * collapses them to `$JAVA_HOME_URI:<major>$` instead. Keep every root here
      * launcher-independent.
      */
    def default(workspace: os.Path): Roots =
      apply(
        workspace,
        os.Path(coursierapi.Cache.create().getLocation, os.pwd),
        os.Path(coursierapi.ArchiveCache.create().getLocation, os.pwd)
      )
  }

  /** Puts a JSON tree into a canonical order.
    *
    * BSP responses carry per-target results in arrays whose order build tools don't promise and, in
    * sbt's case, don't keep stable between runs. Sorting them by the target they describe loses
    * nothing - the arrays are really sets - and is what makes a recording comparable against a
    * committed fixture, and reproducible as replay input.
    *
    * Only arrays whose every element is an object carrying an identifying URI are touched. Other
    * ordered data (class path entries, compiler options) is left exactly as the build tool gave it,
    * except for sbt's own boot class path when [[sortSbtBootClasspath]] is set - that jar order is
    * not stable across OS or architecture, and tests that compare recordings opt in to sorting it.
    *
    * @param sortSbtBootClasspath
    *   test-only. The server never sets this for a real user; integration tests pass it so that
    *   sbt's meta-build class path can be compared across machines. See [[sortSbtBootClasspath]].
    */
  def canonicalize(
    elem: JsonElement,
    sortSbtBootClasspath: Boolean = false
  ): JsonElement =
    canonicalize(elem, fieldName = None, sortSbtBootClasspath = sortSbtBootClasspath)

  private def canonicalize(
    elem: JsonElement,
    fieldName: Option[String],
    sortSbtBootClasspath: Boolean
  ): JsonElement =
    elem match {
      case obj: JsonObject =>
        val res = new JsonObject
        for (entry <- obj.entrySet().asScala)
          res.add(
            entry.getKey,
            canonicalize(entry.getValue, Some(entry.getKey), sortSbtBootClasspath)
          )
        res
      case arr: JsonArray =>
        val elems = arr.asScala.toVector.map(e => canonicalize(e, None, sortSbtBootClasspath))
        val ordered =
          if (
            sortSbtBootClasspath &&
            (fieldName.contains("classpath") || fieldName.contains("jars")) &&
            isSbtBootClasspath(elems)
          )
            elems.sortBy(_.getAsString)
          else {
            val keys = elems.map(sortKey)
            if (keys.forall(_.isDefined) && keys.distinct.length == keys.length)
              elems.zip(keys.map(_.get)).sortBy(_._2).map(_._1)
            else
              elems
          }
        val res = new JsonArray
        for (value <- ordered)
          res.add(value)
        res
      case other =>
        other
    }

  /** Env var integration tests set so [[canonicalize]] may sort sbt's boot class path.
    *
    * Unset in production. Tests put this in the server process environment; the server reads it
    * when writing recordings, and nowhere else.
    */
  val sortSbtBootClasspathEnv = "PLASMON_SORT_SBT_BOOT_CLASSPATH"

  def sortSbtBootClasspath: Boolean =
    Option(System.getenv(sortSbtBootClasspathEnv)).exists(_.toBoolean)

  /** sbt's meta-build class path: every entry lives under `~/.sbt`, or has a path element starting
    * with `sbt-` (the generated `sbt-1.0/classes` directory). A project class path that merely
    * *contains* an sbt-boot jar is left alone.
    */
  private def isSbtBootClasspath(elems: Vector[JsonElement]): Boolean =
    elems.nonEmpty &&
    elems.forall { e =>
      e.isJsonPrimitive && e.getAsJsonPrimitive.isString && {
        val s = e.getAsString
        isUnderSbtHome(s) || hasSbtDashElement(s)
      }
    }

  private def isUnderSbtHome(value: String): Boolean = {
    val slash = value.replace('\\', '/')
    slash.contains("$HOME$/.sbt/") ||
    slash.contains("$HOME$/AppData/Local/sbt/") ||
    slash.contains(s"${os.home.toString.replace('\\', '/')}/.sbt/") ||
    slash.contains(s"${(os.home / "AppData/Local/sbt").toString.replace('\\', '/')}/")
  }

  private def hasSbtDashElement(value: String): Boolean =
    value.split("[/\\\\]").exists(_.startsWith("sbt-"))

  private def sortKey(elem: JsonElement): Option[String] =
    elem match {
      case obj: JsonObject =>
        def uriOf(field: String): Option[String] =
          Option(obj.get(field))
            .collect { case o: JsonObject => o }
            .flatMap(o => Option(o.get("uri")))
            .collect { case p: JsonPrimitive if p.isString => p.getAsString }
        uriOf("target")
          .orElse(uriOf("id"))
          .orElse {
            Option(obj.get("uri")).collect {
              case p: JsonPrimitive if p.isString => p.getAsString
            }
          }
      case _ => None
    }

  /** Applies `f` to every string in the JSON tree.
    *
    * Working on the parsed tree rather than the raw JSON text keeps us clear of escaping concerns -
    * in particular Windows paths, whose separators are doubled in JSON source.
    */
  def mapStrings(elem: JsonElement)(f: String => String): JsonElement =
    elem match {
      case obj: JsonObject =>
        val res = new JsonObject
        for (entry <- obj.entrySet().asScala)
          res.add(entry.getKey, mapStrings(entry.getValue)(f))
        res
      case arr: JsonArray =>
        val res = new JsonArray
        for (value <- arr.asScala)
          res.add(mapStrings(value)(f))
        res
      case prim: JsonPrimitive if prim.isString =>
        new JsonPrimitive(f(prim.getAsString))
      case other =>
        other
    }

  /** Replaces every string that is a JDK home with `$JAVA_HOME_URI:<major>$`.
    *
    * The major version is read from the JDK's own files (`release`), not from a neighbouring
    * `javaVersion` field - build tools have been seen to report the running JVM's version there
    * even when `javaHome` points at a different JDK. Versions that still use the historical `1.x`
    * form (Java 8 and earlier) are stored as `x`.
    *
    * Applied to the parsed tree, before the path-root placeholders, so we still have a real path to
    * read. The placeholder is left as-is on the way back in: replay uses the server's JVM, not the
    * one the recording machine happened to have.
    */
  def replaceJavaHomes(elem: JsonElement): JsonElement =
    elem match {
      case obj: JsonObject =>
        val res = new JsonObject
        val javaHomeMajor =
          Option(obj.get("javaHome")).collect {
            case p: JsonPrimitive if p.isString => p.getAsString
          }.flatMap(jdkMajor)
        val javaVersionMajor =
          Option(obj.get("javaVersion")).collect {
            case p: JsonPrimitive if p.isString => p.getAsString
          }.flatMap(parseMajor)
        for (entry <- obj.entrySet().asScala) {
          val key   = entry.getKey
          val value = entry.getValue
          val replaced =
            (key, value) match {
              case ("javaHome", p: JsonPrimitive) if p.isString && javaHomeMajor.isDefined =>
                new JsonPrimitive(s"$$JAVA_HOME_URI:${javaHomeMajor.get}$$")
              case ("javaVersion", p: JsonPrimitive) if p.isString && javaVersionMajor.isDefined =>
                // Patch-level (21.0.10 vs 17.0.7 vs 17.0.20) is whatever JVM the build tool
                // happened to start with. Keep only the major, same rule as javaHome.
                new JsonPrimitive(s"$$JAVA_VERSION:${javaVersionMajor.get}$$")
              case _ =>
                replaceJavaHomes(value)
            }
          res.add(key, replaced)
        }
        res
      case arr: JsonArray =>
        val res = new JsonArray
        for (value <- arr.asScala)
          res.add(replaceJavaHomes(value))
        res
      case prim: JsonPrimitive if prim.isString =>
        jdkMajor(prim.getAsString) match {
          case Some(major) => new JsonPrimitive(s"$$JAVA_HOME_URI:$major$$")
          case None        => prim
        }
      case other =>
        other
    }

  /** Major version of the JDK at `value`, if `value` is a path or `file:` URI of a JDK home. */
  private def jdkMajor(value: String): Option[Int] =
    pathOf(value).filter(os.exists).flatMap(majorFromJdkFiles)

  private def pathOf(value: String): Option[os.Path] = {
    val trimmed = value.stripSuffix("/").stripSuffix("\\")
    try
      if (trimmed.startsWith("file:"))
        Some(os.Path(java.nio.file.Paths.get(new java.net.URI(trimmed))))
      else
        Some(os.Path(trimmed, os.pwd))
    catch {
      case _: IllegalArgumentException | _: java.net.URISyntaxException |
          _: java.nio.file.InvalidPathException =>
        None
    }
  }

  /** Reads `JAVA_VERSION` from the JDK `release` file.
    *
    * Looks at `javaHome/release` and `javaHome/../release` - the latter is what a JRE-inside-JDK
    * layout (and macOS `Contents/Home` vs. the bundle root) needs.
    */
  private def majorFromJdkFiles(javaHome: os.Path): Option[Int] =
    Iterator(javaHome / "release", javaHome / os.up / "release")
      .filter(os.isFile)
      .flatMap(versionFromReleaseFile)
      .flatMap(parseMajor)
      .take(1)
      .toSeq
      .headOption

  private def versionFromReleaseFile(release: os.Path): Option[String] = {
    val props = new java.util.Properties
    val in    = os.read.inputStream(release)
    try props.load(in)
    finally in.close()
    Option(props.getProperty("JAVA_VERSION")).map(_.stripPrefix("\"").stripSuffix("\""))
  }

  /** `1.8.0_xxx` -> 8, `17.0.7` -> 17, `17-ea` -> 17. */
  private def parseMajor(version: String): Option[Int] = {
    val numbers =
      version.split('-').head.split('.').toList.flatMap(s => scala.util.Try(s.toInt).toOption)
    numbers match {
      case 1 :: minor :: _ => Some(minor)
      case major :: _      => Some(major)
      case Nil             => None
    }
  }
}
