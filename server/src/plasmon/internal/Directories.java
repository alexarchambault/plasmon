package plasmon.internal;

import coursier.paths.shaded.dirs.ProjectDirectories;
import coursier.paths.shaded.dirs.impl.Windows;
import coursier.paths.shaded.dirs.jni.WindowsJni;

import java.util.function.Supplier;

public class Directories {

  private final ProjectDirectories projDirs;

  public Directories() {
    this("Plasmon");
  }

  public Directories(String appName) {
    /*

    Seems scalac crashes when trying to do that from Scala code:

[error] ## Exception when compiling 83 sources to ./out/server/compile.dest/classes
[error] java.lang.AssertionError: assertion failed:
[error]   No RuntimeVisibleAnnotations in classfile with ScalaSignature attribute: package object coursier
[error]      while compiling: ./server/src/plasmon/command/Code.scala
[error]         during phase: globalPhase=typer, enteringPhase=parser
[error]      library version: version 2.13.12
[error]     compiler version: version 2.13.12
[error]   reconstructed args: -bootclasspath …
[error]
[error]   last tree to typer: Ident(coursierapi)
[error]        tree position: line 10 of ./server/src/plasmon/command/Code.scala
[error]             tree tpe: coursierapi.type
[error]               symbol: final package coursierapi
[error]    symbol definition: final package coursierapi (a ModuleSymbol)
[error]       symbol package: <none>
[error]        symbol owners: package coursierapi
[error]            call site: method run in object Code in package command
[error]
[error] == Source file context for tree position ==
[error]
[error]      7   override def stopAtFirstUnrecognized = true
[error]      8   def run(options: CodeOptions, remainingArgs: RemainingArgs): Unit = {
[error]      9     val a = {
[error]     10       import coursierapi.shaded.coursier.cache.shaded.dirs.GetWinDirs
[error]     11       import coursierapi.shaded.coursier.cache.shaded.dirs.ProjectDirectories
[error]     12       import plasmon.internal.JniGetWinDirs
[error]     13       val getWinDirs =
[error] scala.reflect.internal.SymbolTable.throwAssertionError(SymbolTable.scala:171)
[error] scala.tools.nsc.symtab.classfile.ClassfileParser.unpickleOrParseInnerClasses(ClassfileParser.scala:1193)
[error] scala.tools.nsc.symtab.classfile.ClassfileParser.parseClass(ClassfileParser.scala:489)
[error] scala.tools.nsc.symtab.classfile.ClassfileParser.$anonfun$parse$2(ClassfileParser.scala:174)
[error] scala.tools.nsc.symtab.classfile.ClassfileParser.$anonfun$parse$1(ClassfileParser.scala:159)
[error] scala.tools.nsc.symtab.classfile.ClassfileParser.parse(ClassfileParser.scala:142)
[error] scala.tools.nsc.symtab.SymbolLoaders$ClassfileLoader.doComplete(SymbolLoaders.scala:342)
[error] scala.tools.nsc.symtab.SymbolLoaders$SymbolLoader.$anonfun$complete$2(SymbolLoaders.scala:249)
[error] …

    */
    Supplier<Windows> windows;
    if (coursier.paths.Util.useJni())
      windows = WindowsJni.getJdkAwareSupplier();
    else
      windows = Windows.getDefaultSupplier();
    projDirs = ProjectDirectories.from(null, null, appName, windows);
  }

  public String dataDir() {
    return projDirs.dataDir;
  }

  public String dataLocalDir() {
    return projDirs.dataLocalDir;
  }

  public String cacheDir() {
    return projDirs.cacheDir;
  }

}
