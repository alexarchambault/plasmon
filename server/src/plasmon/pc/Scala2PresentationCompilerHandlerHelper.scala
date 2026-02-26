package plasmon.pc

import java.nio.file.Path
import java.util.function.{Consumer, Supplier}

import scala.meta.internal.mtags.GlobalSymbolIndex
import scala.meta.internal.pc.ScalaPresentationCompiler

class Scala2PresentationCompilerHandlerHelper {
  def createCompiler(
    javaHome: Path,
    userLoggerSupplier: Supplier[Consumer[String]],
    module: GlobalSymbolIndex.Module
  ): ScalaPresentationCompiler =
    new ScalaPresentationCompiler(javaHome, userLoggerSupplier, module)
}
