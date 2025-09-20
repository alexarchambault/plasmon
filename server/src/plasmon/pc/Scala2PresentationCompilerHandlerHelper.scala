package plasmon.pc

import scala.meta.internal.mtags.GlobalSymbolIndex
import java.util.function.Consumer
import java.util.function.Supplier
import java.nio.file.Path
import scala.meta.internal.pc.ScalaPresentationCompiler

class Scala2PresentationCompilerHandlerHelper {
  def createCompiler(
    javaHome: Path,
    userLoggerSupplier: Supplier[Consumer[String]],
    module: GlobalSymbolIndex.Module
  ): ScalaPresentationCompiler =
    new ScalaPresentationCompiler(javaHome, userLoggerSupplier, module)
}
