package plasmon.pc

/** Where the presentation compilers (and the class path indexer) read the JDK classes from
  *
  * The files of [[JdkClasses.classPath]] get appended to the class path of every build target, and
  * the presentation compilers read the JDK classes from those rather than from the JDK they run on
  * (they handle `.jmod` files and `lib/ct.sym` themselves).
  */
sealed abstract class JdkClasses(val id: String) extends Product with Serializable {

  /** The files the JDK classes of `javaHome` are read from, for Java 9+ */
  protected def modernClassPath(javaHome: os.Path): Seq[os.Path]

  /** The files the JDK classes of `javaHome` are read from: `rt.jar` for Java 8, else depends on
    * this mode. Empty if the JDK doesn't have the files this mode needs.
    */
  final def classPath(javaHome: os.Path): Seq[os.Path] = {
    val rtJar = javaHome / "jre/lib/rt.jar"
    if (os.isFile(rtJar)) Seq(rtJar)
    else modernClassPath(javaHome)
  }

  /** What [[classPath]] looks for, for messages */
  def description: String
}

object JdkClasses {

  /** The class signatures of `lib/ct.sym`, for the JDK's own release
    *
    * javac reads that file for `--release`, and it's there in every JDK 9+ that has javac,
    * including the ones built without JMODs (JEP 493, like Temurin 24 and later).
    *
    * Up to JDK 21, ct.sym doesn't have the signatures of the JDK's own release, only those of the
    * previous ones, so the `.jmod` files are read instead there (all those JDKs ship them). Only if
    * there are none does ct.sym get used anyway, for the API of the previous release.
    */
  case object CtSym extends JdkClasses("ct-sym") {
    protected def modernClassPath(javaHome: os.Path): Seq[os.Path] = {
      val ctSym = javaHome / "lib/ct.sym"
      if (os.isFile(ctSym) && hasOwnRelease(javaHome, ctSym)) Seq(ctSym)
      else {
        val jmods = jmodFiles(javaHome)
        if (jmods.nonEmpty) jmods
        else Seq(ctSym).filter(os.isFile(_))
      }
    }
    def description = "rt.jar, lib/ct.sym or .jmod files"

    /** Whether ct.sym has the class signatures of the release of the JDK it comes from */
    def hasOwnRelease(javaHome: os.Path, ctSym: os.Path): Boolean = {
      val release = scala.meta.internal.metals.CtSym.layout(ctSym.toNIO).release
      plasmon.bsp.BspDataPortability.majorFromJdkFiles(javaHome) match {
        case Some(major) => release == scala.meta.internal.metals.CtSym.releaseChar(major)
        case None        => true
      }
    }
  }

  /** The `.jmod` files under `jmods/`, that recent JDKs don't necessarily ship */
  case object Jmods extends JdkClasses("jmods") {
    protected def modernClassPath(javaHome: os.Path): Seq[os.Path] =
      jmodFiles(javaHome)
    def description = "rt.jar or .jmod files"
  }

  private def jmodFiles(javaHome: os.Path): Seq[os.Path] = {
    val jmodsDir = javaHome / "jmods"
    if (os.isDir(jmodsDir))
      os.list(jmodsDir)
        .filter(_.last.endsWith(".jmod"))
        .filter(os.isFile(_))
    else
      Nil
  }

  def default: JdkClasses = CtSym

  def all: Seq[JdkClasses] = Seq(CtSym, Jmods)

  def parse(id: String): Option[JdkClasses] =
    all.find(_.id == id)
}
