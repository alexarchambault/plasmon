package plasmon.integration

/** How a test talks to the plasmon server it started.
  *
  * Both ways reach the same server-side code (see `plasmon.servercommand.ProjectOps` and the
  * handlers around it), so a test runs unchanged either way and is checked against the same
  * fixtures - a difference between the two modes is a bug in one of the two entry points.
  */
sealed abstract class TestMode extends Product with Serializable {
  def id: String

  /** Appended to test names, so the two runs of a test are told apart in reports and logs. */
  def testNameSuffix: String

  /** How much longer than the LSP run a test is given here.
    *
    * Per-test budgets are hang detectors rather than performance targets, and every CLI call in a
    * test is a JVM start of its own - roughly a second each, which a test asking a hundred
    * questions of the server feels. Scaling the budget keeps the detector useful without turning
    * the slower mode into a flaky one.
    */
  def timeoutFactor: Int
}

object TestMode {

  /** Over LSP / JSON-RPC, the way the editor extension drives the server. */
  case object Lsp extends TestMode {
    def id             = "lsp"
    def testNameSuffix = ""
    def timeoutFactor  = 1
  }

  /** Through `plasmon <command>`, the way a terminal drives the server. */
  case object Cli extends TestMode {
    def id             = "cli"
    def testNameSuffix = " via CLI"
    def timeoutFactor  = 4
  }

  def all: Seq[TestMode] = Seq(Lsp, Cli)

  def parse(id: String): Option[TestMode] =
    all.find(_.id == id.trim)
}
