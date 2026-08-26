//> using scala 3.8.4
//> using dep com.lihaoyi::os-lib:0.11.8
//> using dep com.github.alexarchambault::case-app:2.1.0

import caseapp.*
import caseapp.core.RemainingArgs
import caseapp.core.help.Help
import caseapp.core.parser.Parser

/** Print the first few failing integration-test log files.
  *
  * Each test writes to out/integration-test-logs/running/ while it runs. Success deletes that file;
  * failure moves it to failed/. Leftovers in running/ are printed too (JVM crash / timeout
  * mid-test).
  *
  * Usage: scala-cli --server=false integration/print-failing-test-logs.scala -- --max 5 --dir
  * out/integration-test-logs
  */
// format: off
final case class Options(
  @HelpMessage("Maximum number of failing test logs to print")
    max: Int = 5,
  @HelpMessage("Root directory that contains failed/ and running/")
    dir: String = "out/integration-test-logs"
)
// format: on

object Options {
  implicit lazy val parser: Parser[Options] = Parser.derive
  implicit lazy val help: Help[Options]     = Help.derive
}

object PrintFailingTestLogs extends CaseApp[Options] {
  override def name = "print-failing-test-logs"

  def run(options: Options, remainingArgs: RemainingArgs): Unit = {
    if (remainingArgs.all.nonEmpty)
      sys.error(s"Unexpected arguments: ${remainingArgs.all.mkString(" ")}")

    val root = os.Path(options.dir, os.pwd)
    def logsIn(sub: String): Seq[os.Path] = {
      val d = root / sub
      if (os.isDir(d))
        os.list(d).filter(p => os.isFile(p) && p.ext == "log")
      else
        Nil
    }
    val files = (logsIn("failed") ++ logsIn("running")).sortBy(os.mtime)
    if (files.isEmpty)
      println("No failing integration test logs found")
    else {
      val toPrint = files.take(options.max)
      println(s"Printing ${toPrint.size} of ${files.size} failing integration test log(s)")
      for ((path, idx) <- toPrint.zipWithIndex) {
        val text  = os.read(path)
        val first = text.linesIterator.nextOption().getOrElse("")
        val title =
          if (first.startsWith("# test: ")) first.stripPrefix("# test: ")
          else path.last
        val n = idx + 1
        println()
        println(s"::group::Failing test $n/${toPrint.size}: $title")
        println("=" * 80)
        println(s"Failing test $n/${toPrint.size}: $title")
        println(s"Log: $path")
        println("=" * 80)
        print(text)
        if (!text.endsWith("\n"))
          println()
        println("::endgroup::")
      }
      if (files.size > options.max)
        println(s"(${files.size - options.max} more failing test log(s) in ${root / "failed"})")
    }
  }
}
