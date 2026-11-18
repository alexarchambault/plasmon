package plasmon.pc

/** How to create the presentation compiler of a Scala build target
  *
  * @param createCompiler
  *   creates the presentation compiler, for a given Java home
  * @param adaptScala2Options
  *   whether the Scala 2 options of the build target should be adapted for a Scala 3 compiler (when
  *   using the Scala 3 presentation compiler for Scala 2 build targets)
  */
final case class MakeCompiler(
  createCompiler: os.Path => scala.meta.pc.PresentationCompiler,
  adaptScala2Options: Boolean = false
)
