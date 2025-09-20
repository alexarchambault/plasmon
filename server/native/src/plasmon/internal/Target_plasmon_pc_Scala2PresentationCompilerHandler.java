package plasmon.internal;

import java.nio.file.Path;
import java.util.function.Consumer;
import java.util.function.Supplier;

import scala.meta.internal.mtags.GlobalSymbolIndex;
import scala.meta.pc.PresentationCompiler;

import com.oracle.svm.core.annotate.*;

@TargetClass(className = "plasmon.pc.Scala2PresentationCompilerHandler", onlyWith = DisableScala2Pc.class)
final class Target_plasmon_pc_Scala2PresentationCompilerHandler {
  @Substitute
  public boolean available() {
    return false;
  }

  @Delete
  public PresentationCompiler create(
    Path javaHome,
    Supplier<Consumer<String>> userLoggerSupplier,
    GlobalSymbolIndex.Module module
  ) { return null; };
}
