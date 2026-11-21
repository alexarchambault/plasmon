package plasmon.pc

/** Sets of JDK files presentation compilers can read the JDK classes from, for tests */
sealed abstract class TestJdkFiles(val id: String) extends Product with Serializable {
  def files(javaHome: os.Path): Seq[os.Path]

  /** What the URI of a class location in these files contains */
  def locationMarker: String
}

object TestJdkFiles {
  case object CtSym extends TestJdkFiles("ct.sym") {
    def files(javaHome: os.Path) = Seq(javaHome / "lib/ct.sym")
    def locationMarker           = "/lib/ct.sym!/"
  }
  case object Jmods extends TestJdkFiles("jmods") {
    def files(javaHome: os.Path) = JdkClasses.Jmods.classPath(javaHome)
    def locationMarker           = "/jmods/java.base.jmod!/classes/"
  }

  def all: Seq[TestJdkFiles] = Seq(CtSym, Jmods)
}
