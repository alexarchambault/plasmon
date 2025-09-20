package plasmon.pc

final case class MakeCompiler(
  createCompiler: os.Path => scala.meta.pc.PresentationCompiler with scala.meta.internal.pc.HasCompilerAccess
)
