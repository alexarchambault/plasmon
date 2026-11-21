// Originally based on https://github.com/scalameta/metals/blob/0ad0bc184f82dbd178d01f76913ea6bdfa98db14/metals/src/main/scala/scala/meta/internal/metals/Directories.scala

package plasmon.ide

object Directories {

  @volatile private var state0: os.SubPath = os.sub / ".plasmon"

  /** Where the server keeps its state (build tools and modules loaded, caches, logs, dependency
    * sources…), relative to its working directory: `.plasmon` unless `--state-dir` says otherwise.
    *
    * Process-wide, as there's one server per process: helpers with no server at hand read it.
    */
  def state: os.SubPath = state0
  def setState(dir: os.SubPath): Unit =
    state0 = dir

  def readonly0: os.SubPath =
    state / "readonly"
  def readonly: os.SubPath =
    state / "readonly"
  def dependencies: os.SubPath =
    readonly0 / dependenciesName
  def semanticdb: os.SubPath =
    os.sub / "META-INF/semanticdb"
  def bestEffort: os.SubPath =
    os.sub / "META-INF/best-effort"

  val dependenciesName = "dependencies"
}
