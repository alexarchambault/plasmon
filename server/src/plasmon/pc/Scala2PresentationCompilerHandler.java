package plasmon.pc;

import java.nio.file.Path;
import java.util.function.Consumer;
import java.util.function.Supplier;

import scala.meta.internal.mtags.GlobalSymbolIndex;
import scala.meta.internal.pc.ScalaPresentationCompiler;
import scala.meta.pc.PresentationCompiler;

import plasmon.internal.DisableScala2Pc;

public class Scala2PresentationCompilerHandler {

  public boolean available() {
    return !(new DisableScala2Pc()).getAsBoolean();
  }

  public PresentationCompiler create(
    Path javaHome,
    Supplier<Consumer<String>> userLoggerSupplier,
    GlobalSymbolIndex.Module module
  ) {
    return new Scala2PresentationCompilerHandlerHelper().createCompiler(
      javaHome,
      userLoggerSupplier,
      module
    );
  }

}
