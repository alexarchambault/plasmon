package plasmon.util

import ch.epfl.scala.{bsp4j => b}
import org.eclipse.{lsp4j => l}

import java.io.File

import scala.jdk.CollectionConverters.*

// Originally based on https://github.com/VirtusLab/scala-cli/blob/82b39a24bdcb54ff00c3e9a00efd41cdd4c9f1bc/modules/build/src/main/scala/scala/build/ConsoleBloopBuildClient.scala#L144-L237

object PrintDiagnostic {

  def diagnosticPrefix(severity: b.DiagnosticSeverity, color: Boolean): String = {

    val reset  = if (color) Console.RESET else ""
    val red    = if (color) Console.RED else ""
    val yellow = if (color) Console.YELLOW else ""

    severity match {
      case b.DiagnosticSeverity.ERROR       => s"[${red}error$reset] "
      case b.DiagnosticSeverity.WARNING     => s"[${yellow}warn$reset] "
      case b.DiagnosticSeverity.INFORMATION => "[info] "
      case b.DiagnosticSeverity.HINT        => s"[${yellow}hint$reset] "
    }
  }

  private def lspToBspRange(bspRange: l.Range): b.Range =
    new b.Range(
      new b.Position(bspRange.getStart.getLine, bspRange.getStart.getCharacter),
      new b.Position(bspRange.getEnd.getLine, bspRange.getEnd.getCharacter)
    )

  def printFileDiagnostic(
    printLine: String => Unit,
    path: Either[String, os.Path],
    diag: l.Diagnostic,
    color: Boolean
  ): Unit = {
    val diag0 = new b.Diagnostic(
      lspToBspRange(diag.getRange),
      diag.getMessage
    )
    for (code <- Option(diag.getCode))
      if (code.isLeft)
        diag0.setCode(code.getLeft)
    diag0.setData(diag.getData)
    for (severity <- Option(diag.getSeverity))
      diag0.setSeverity {
        severity match {
          case l.DiagnosticSeverity.Error       => b.DiagnosticSeverity.ERROR
          case l.DiagnosticSeverity.Warning     => b.DiagnosticSeverity.WARNING
          case l.DiagnosticSeverity.Information => b.DiagnosticSeverity.INFORMATION
          case l.DiagnosticSeverity.Hint        => b.DiagnosticSeverity.HINT
        }
      }
    for (relatedInformation <- Option(diag.getRelatedInformation))
      diag0.setRelatedInformation {
        relatedInformation
          .asScala
          .map { info =>
            new b.DiagnosticRelatedInformation(
              new b.Location(
                info.getLocation.getUri,
                lspToBspRange(info.getLocation.getRange)
              ),
              info.getMessage
            )
          }
          .asJava
      }
    diag0.setSource(diag.getSource)
    printFileDiagnostic(printLine, path, diag0, color = color)
  }

  def printFileDiagnostic(
    printLine: String => Unit,
    path: Either[String, os.Path],
    diag: b.Diagnostic,
    color: Boolean
  ): Unit = {
    val isWarningOrErrorOrHint = diag.getSeverity == b.DiagnosticSeverity.ERROR ||
      diag.getSeverity == b.DiagnosticSeverity.WARNING ||
      diag.getSeverity == b.DiagnosticSeverity.HINT
    if (isWarningOrErrorOrHint) {
      val prefix = diagnosticPrefix(diag.getSeverity, color = color)

      val line  = (diag.getRange.getStart.getLine + 1).toString + ":"
      val col   = (diag.getRange.getStart.getCharacter + 1).toString
      val msgIt = diag.getMessage.linesIterator

      val path0 = path match {
        case Left(source) => source
        case Right(p) if p.startsWith(os.pwd) =>
          "." + File.separator + p.relativeTo(os.pwd).toString
        case Right(p) => p.toString
      }
      printLine(s"$prefix$path0:$line$col")
      for (line <- msgIt)
        printLine(prefix + line)
      val codeOpt = {
        val lineOpt =
          if (diag.getRange.getStart.getLine == diag.getRange.getEnd.getLine)
            Option(diag.getRange.getStart.getLine)
          else None
        for {
          line <- lineOpt
          p    <- path.toOption
          if os.isFile(p)
          lines = os.read.lines(p)
          line <- if (line < lines.length) Some(lines(line)) else None
        } yield line
      }
      for (code <- codeOpt)
        code.linesIterator.map(prefix + _).foreach(printLine)
      val canPrintUnderline = diag.getRange.getStart.getLine == diag.getRange.getEnd.getLine &&
        diag.getRange.getStart.getCharacter != null &&
        diag.getRange.getEnd.getCharacter != null &&
        codeOpt.nonEmpty
      if (canPrintUnderline) {
        val len =
          math.max(1, diag.getRange.getEnd.getCharacter - diag.getRange.getStart.getCharacter)
        printLine(
          prefix + " " * diag.getRange.getStart.getCharacter + "^" * len
        )
      }
    }
  }

}
