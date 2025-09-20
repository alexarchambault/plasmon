package scala.tools.nsc.ast

import scala.tools.nsc.Global

abstract class TreeBrowsers {
  val global: Global
  import global._

  def create(): SwingBrowser = new SwingBrowser()

  class SwingBrowser {
    def browse(pName: String, units: Iterator[CompilationUnit]): Unit =
      browse(pName, units.toList)

    def browse(pName: String, units: List[CompilationUnit]): Unit =
      System.err.println("TreeBrowser not available from native image")
  }
}
