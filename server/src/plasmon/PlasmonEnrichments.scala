package plasmon

import ch.epfl.scala.bsp4j as b
import com.google.gson.{
  Gson,
  GsonBuilder,
  JsonDeserializationContext,
  JsonDeserializer,
  JsonElement,
  JsonObject
}
import org.eclipse.lsp4j as l
import org.eclipse.lsp4j.jsonrpc.messages.Either as JEither
import org.scalameta.UnreachableError
import org.scalameta.invariants.InvariantFailedException
import plasmon.ide.*

import java.io.IOException
import java.lang.reflect.Type
import java.net.URI
import java.nio.file.{
  FileAlreadyExistsException,
  FileSystemNotFoundException,
  Files,
  Paths
}
import java.util.Optional
import java.util.concurrent.{
  CancellationException,
  CompletableFuture,
  CompletionStage
}

import scala.annotation.tailrec
import scala.compat.java8.FutureConverters
import scala.concurrent.{ExecutionContext, Future}
import scala.jdk.CollectionConverters.*
import scala.meta.{Dialect, Tree}
import scala.meta.inputs.{Input, Position}
import scala.meta.internal.semanticdb as s
import scala.meta.internal.metals.{
  CompilerRangeParamsUtils,
  EmptyCancelToken,
  ScalaVersions
}
import scala.meta.internal.mtags.{GlobalSymbolIndex, SourcePath}
import scala.meta.internal.pc.{CompletionItemData, RangeOffset}
import scala.meta.internal.semanticdb.Language
import scala.meta.internal.semanticdb.SymbolInformation.Kind as k
import scala.meta.internal.tokenizers.{LegacyScanner, LegacyTokenData}
import scala.meta.io.AbsolutePath
import scala.meta.pc.{OffsetParams, VirtualFileParams}
import scala.meta.tokens.Token
import scala.util.{Properties, Try}
import scala.util.control.NonFatal

// Many things here copied from
//   https://github.com/scalameta/metals/blob/0ad0bc184f82dbd178d01f76913ea6bdfa98db14/mtags-shared/src/main/scala/scala/meta/internal/mtags/CommonMtagsEnrichments.scala
//   https://github.com/scalameta/metals/blob/0ad0bc184f82dbd178d01f76913ea6bdfa98db14/mtags/src/main/scala-2/scala/meta/internal/mtags/MtagsEnrichments.scala
//   https://github.com/scalameta/metals/blob/0ad0bc184f82dbd178d01f76913ea6bdfa98db14/metals/src/main/scala/scala/meta/internal/metals/MetalsEnrichments.scala
//   https://github.com/scalameta/metals/blob/0ad0bc184f82dbd178d01f76913ea6bdfa98db14/mtags/src/main/scala/scala/meta/internal/mtags/ScalametaCommonEnrichments.scala

object PlasmonEnrichments {

  implicit class XtensionAbsolutePathBuffers(path: AbsolutePath) {
    def toOs: os.Path =
      os.Path(path.toNIO)
  }

  private def filenameToLanguage(filename: String): Language =
    if (filename.endsWith(".java")) Language.JAVA
    else if (
      filename.endsWith(".scala") || filename.endsWith(".sc")
      || filename.endsWith(".sbt") || filename.endsWith(".mill")
    )
      Language.SCALA
    else Language.UNKNOWN_LANGUAGE

  implicit class XtensionSourcePathBuffers(path: SourcePath) {

    /** Reads file contents from editor buffer with fallback to disk.
      */
    def toInputFromBuffers(buffers: Buffers)(implicit
      ctx: SourcePath.Context
    ): Input.VirtualFile =
      path match {
        case z: SourcePath.ZipEntry =>
          z.toInput
        case p: SourcePath.Standard =>
          buffers.get(os.Path(p.path)) match {
            case Some(text) => Input.VirtualFile(p.uri, text)
            case None       => p.toInput
          }
      }

    def toInput(implicit
      context: SourcePath.Context
    ): Input.VirtualFile =
      Input.VirtualFile(path.uri, path.content())

    def toLanguage: Language =
      filenameToLanguage(path.uri)
    def isScalaScript: Boolean =
      path match {
        case p: SourcePath.Standard => p.path.toString.isScalaScript
        case _: SourcePath.ZipEntry => false
      }

    def toFileOnDisk(workspace: os.Path)(implicit
      ctx: SourcePath.Context
    ): SourcePath.Standard =
      SourcePath.Standard(toFileOnDisk0(workspace, 0).toNIO)

    def isMill: Boolean =
      path match {
        case p: SourcePath.Standard => p.path.toString.isMill
        case _: SourcePath.ZipEntry => false
      }

    private def toFileOnDisk0(
      workspace: os.Path,
      retryCount: Int
    )(implicit ctx: SourcePath.Context): os.Path = {
      def toJarMeta(jar: os.Path): String =
        s"${os.mtime(jar)}\n$jar"

      def readJarMeta(jarMetaFile: os.Path): Option[String] =
        Option.when(os.exists(jarMetaFile))(os.read(jarMetaFile))

      def withJarDirLock[A](dir: os.Path)(f: => A)(fallback: => A): A = {
        val lockFile = dir / ".lock"
        if (os.exists(lockFile))
          fallback
        else
          try {
            os.write(lockFile, Array.emptyByteArray, createFolders = true)
            f
          }
          catch {
            case _: IOException =>
              fallback
          }
          finally
            os.remove(lockFile)
      }

      def retry: os.Path = {
        Thread.sleep(50)
        this.toFileOnDisk0(workspace, retryCount + 1)
      }

      def writeFile(dest: os.Path, content: Array[Byte]): Unit = {
        if (!Properties.isWin && os.isFile(dest))
          dest.toIO.setWritable(true)
        try {
          os.write.over(dest, content, createFolders = true)
          // Don't use readOnly files on Windows, makes it impossible to walk
          // the entire directory later on.
          if (!Properties.isWin)
            dest.toIO.setReadOnly()
        }
        catch {
          case _: FileAlreadyExistsException =>
            () // ignore
        }
      }

      // prevent infinity loop
      if (retryCount > 5)
        throw new Exception(s"Unable to save $path in workspace")
      else
        path match {
          case s: SourcePath.Standard =>
            os.Path(s.path, os.pwd)
          case z: SourcePath.ZipEntry =>
            val jarDir      = workspace / Directories.dependencies / z.zipPath.getFileName.toString
            val out         = jarDir / z.pathInZip.split("/").toSeq
            val jarMetaFile = jarDir / ".jar.meta"

            lazy val currentJarMeta = readJarMeta(jarMetaFile)
            lazy val jarMeta        = toJarMeta(os.Path(z.zipPath, os.pwd))

            val updateMeta = !os.exists(jarDir) || !currentJarMeta.contains(jarMeta)
            if (!os.exists(out) || updateMeta)
              withJarDirLock(jarDir) {
                if (updateMeta) {
                  os.walk(jarDir)
                    .filter(_.last != ".lock")
                    .filter(os.isFile)
                    .foreach(os.remove)
                  os.write(jarMetaFile, jarMeta)
                }
                writeFile(out, z.rawContent())
                out
              }(retry)
            else
              out
        }
    }

    def readTextOpt(implicit ctx: SourcePath.Context): Option[String] =
      Option.when(path.exists())(path.content())
  }

  implicit class XtensionJavaFuture[T](future: CompletionStage[T]) {
    def asScala: Future[T] = FutureConverters.toScala(future)
  }

  implicit class XtensionCompletionItemData(item: l.CompletionItem) {
    def data: Option[CompletionItemData] =
      item.getData match {
        case d: CompletionItemData =>
          Some(d)
        case data =>
          decodeJson(data, classOf[CompletionItemData])
      }

    def getLeftTextEdit: Option[l.TextEdit] =
      for {
        either   <- Option(item.getTextEdit)
        textEdit <- Option(either.getLeft)
      } yield textEdit
  }

  private def decodeJson[T](
    obj: AnyRef,
    cls: java.lang.Class[T],
    gson: Option[Gson] = None
  ): Option[T] =
    Option(obj).flatMap { data =>
      try
        Option(
          gson
            .getOrElse(new Gson())
            .fromJson[T](data.asInstanceOf[JsonElement], cls)
        )
      catch {
        case NonFatal(e) =>
          scribe.error(s"decode error: $cls", e)
          None
      }
    }

  implicit class XtensionPositionLspInverse(pos: l.Position) {

    def isNone: Boolean =
      pos.getLine < 0 &&
      pos.getCharacter < 0

    def <=(other: l.Position): Boolean =
      pos.getLine < other.getLine ||
      (pos.getLine == other.getLine && pos.getCharacter <= other.getCharacter)

    /** LSP position translated to scalameta position. Might return None if pos is not contained in
      * input
      *
      * @param input
      *   file input the position relates to
      * @return
      *   scalameta position with offset if the pos is contained in the file
      */
    def toMeta(input: Input): Option[Position] =
      Try(
        Position.Range(
          input,
          pos.getLine,
          pos.getCharacter,
          pos.getLine,
          pos.getCharacter
        )
      ).toOption

    def toBsp: b.Position =
      new b.Position(pos.getLine, pos.getCharacter)
  }

  implicit class NioPathExtensions(path: java.nio.file.Path) {
    def maybeToRealPath: java.nio.file.Path =
      if (Files.exists(path)) path.toRealPath()
      else path
  }

  implicit class URIThingExtensions(uri: URI) {
    // toRealPath converts lower case driver letters (that VSCode often handles us)
    // to upper case. The latter matches the JVM convention.
    def toOsPath: os.Path =
      os.Path(Paths.get(uri).maybeToRealPath)
  }

  implicit class StringThingExtensions(value: String) {
    // toRealPath converts lower case driver letters (that VSCode often handles us)
    // to upper case. The latter matches the JVM convention.
    def osPathFromUri: os.Path =
      os.Path(Paths.get(new URI(value)).maybeToRealPath)
    def maybeOsPathFromUri: Option[os.Path] =
      try Some(os.Path(Paths.get(new URI(value)).maybeToRealPath))
      catch {
        case _: FileSystemNotFoundException => None
      }

    /** Returns true if this is a Scala.js or Scala Native target
      *
      * FIXME: https://github.com/scalacenter/bloop/issues/700
      */
    def isNonJVMPlatformOption: Boolean = {
      def isCompilerPlugin(name: String, organization: String): Boolean =
        value.startsWith("-Xplugin:") &&
        value.contains(name) &&
        value.contains(organization)
      // Scala Native and Scala.js are not needed to navigate dependency sources
      isCompilerPlugin("nscplugin", "scala-native") ||
      isCompilerPlugin("scalajs-compiler", "scala-js") ||
      value.startsWith("-P:scalajs:")
    }

    def safeTokenize(implicit
      dialect: Dialect
    ): scala.meta.Tokenized = {
      import scala.meta._
      try
        value.tokenize
      catch {
        case invariant: InvariantFailedException =>
          scribe.error(
            s"Got invariant failed exception for '$value', which should not happen:\n" +
              invariant.getMessage
          )
          Tokenized.Error(scala.meta.Position.None, invariant.getMessage, invariant)
        case unreachable: UnreachableError =>
          scribe.error(
            s"Got unreachable exception for '$value', which should not happen:\n" +
              unreachable.getMessage
          )
          Tokenized.Error(
            scala.meta.Position.None,
            unreachable.getMessage,
            new RuntimeException(unreachable.getMessage)
          )
      }
    }

    def lastIndexBetween(
      char: Char,
      lowerBound: Int,
      upperBound: Int
    ): Int = {
      val safeLowerBound = Math.max(0, lowerBound)
      var index          = upperBound
      while (index >= safeLowerBound && value(index) != char)
        index -= 1
      if (index < safeLowerBound) -1 else index
    }
  }

  implicit class XtensionOptionScala[T](opt: Option[T]) {
    def asJava: Optional[T] =
      if (opt.isDefined) Optional.of(opt.get)
      else Optional.empty()
  }

  implicit class XtensionOptionalJava[T](opt: Optional[T]) {
    def asScala: Option[T] =
      if (opt.isPresent) Some(opt.get())
      else None
  }

  implicit class XtensionLspRangeMeta(range: l.Range) {
    def isNone: Boolean =
      range.getStart.isNone &&
      range.getEnd.isNone
    def toMeta(input: Input): Option[Position] =
      if (range.isNone)
        None
      else
        Try(
          Position.Range(
            input,
            range.getStart.getLine,
            range.getStart.getCharacter,
            range.getEnd.getLine,
            range.getEnd.getCharacter
          )
        ).toOption

    def overlapsWith(other: l.Range): Boolean = {
      val startsBeforeOtherEnds =
        range.getStart.getLine < other.getEnd.getLine ||
        (range.getStart.getLine == other.getEnd.getLine &&
        range.getStart.getCharacter <= other.getEnd.getCharacter)

      val endsAfterOtherStarts =
        range.getEnd.getLine > other.getStart.getLine ||
        (range.getEnd.getLine == other.getStart.getLine &&
        range.getEnd.getCharacter >= other.getStart.getCharacter)

      startsBeforeOtherEnds && endsAfterOtherStarts
    }
  }

  implicit class XtensionJEitherCross[A, B](either: JEither[A, B]) {
    def asScala: Either[A, B] =
      if (either.isLeft) Left(either.getLeft)
      else Right(either.getRight)
  }

  implicit class XtensionMetaPosition(pos: Position) {
    def encloses(other: Position): Boolean =
      pos.start <= other.start && pos.end >= other.end

    def toLsp: l.Range =
      new l.Range(
        new l.Position(pos.startLine, pos.startColumn),
        new l.Position(pos.endLine, pos.endColumn)
      )

    def encloses(other: l.Range): Boolean = {
      val start = other.getStart
      val end   = other.getEnd
      val isBefore =
        pos.startLine < start.getLine ||
        (pos.startLine == start.getLine && pos.startColumn <= start
          .getCharacter)

      val isAfter = pos.endLine > end.getLine ||
        (pos.endLine >= end.getLine && pos.endColumn >= end.getCharacter)

      isBefore && isAfter
    }
  }

  implicit class XtensionSemanticdbRange(range: s.Range) {
    def isPoint: Boolean =
      range.startLine == range.endLine &&
      range.startCharacter == range.endCharacter

    def toLocation(uri: String): l.Location =
      new l.Location(uri, range.toLsp)
    def toLsp: l.Range = {
      val start = new l.Position(range.startLine, range.startCharacter)
      val end   = new l.Position(range.endLine, range.endCharacter)
      new l.Range(start, end)
    }
    def encloses(
      other: l.Position,
      includeLastCharacter: Boolean = false
    ): Boolean = {
      val startsBeforeOrAt =
        range.startLine < other.getLine ||
        (range.startLine == other.getLine &&
        range.startCharacter <= other.getCharacter)
      val endCharCondition =
        if (includeLastCharacter)
          range.endCharacter >= other.getCharacter
        else
          range.endCharacter > other.getCharacter
      val endsAtOrAfter =
        range.endLine > other.getLine ||
        (range.endLine == other.getLine &&
        endCharCondition)
      startsBeforeOrAt && endsAtOrAfter
    }
  }

  implicit class XtensionSymbolOccurrenceProtocol(occ: s.SymbolOccurrence) {
    def toLocation(uri: String): l.Location =
      occ.range.getOrElse(s.Range(0, 0, 0, 0)).toLocation(uri)
    def encloses(
      pos: l.Position,
      includeLastCharacter: Boolean = false
    ): Boolean =
      occ.range.isDefined &&
      occ.range.get.encloses(pos, includeLastCharacter)
  }

  implicit class XtensionTextDocumentSemanticdb(textDocument: s.TextDocument) {

    /** Returns true if the symbol is defined in this document
      */
    def definesSymbol(symbol: String): Boolean =
      textDocument.occurrences.exists { localOccurrence =>
        localOccurrence.role.isDefinition &&
        localOccurrence.symbol == symbol
      }

    def toLocation(uri: URI, symbol: String): Option[l.Location] =
      toLocation(uri.toString, symbol)

    def toLocation(uri: String, symbol: String): Option[l.Location] =
      textDocument.occurrences
        .find(o => o.role.isDefinition && o.symbol == symbol)
        .map(_.toLocation(uri))
  }

  implicit class XtensionEditDistance(result: Either[EmptyResult, Position]) {
    def toPosition(dirty: l.Position): Option[l.Position] =
      foldResult(
        onPosition =
          pos => Some(new l.Position(pos.startLine, pos.startColumn)),
        onUnchanged = () => Some(dirty),
        onNoMatch = () => None
      )

    def toLocation(dirty: l.Location): Option[l.Location] =
      foldResult(
        pos =>
          Some(
            new l.Location(
              dirty.getUri,
              new l.Range(
                new l.Position(pos.startLine, pos.startColumn),
                new l.Position(pos.endLine, pos.endColumn)
              )
            )
          ),
        () => Some(dirty),
        () => None
      )
    def foldResult[B](
      onPosition: Position => B,
      onUnchanged: () => B,
      onNoMatch: () => B
    ): B =
      result match {
        case Right(pos)                  => onPosition(pos)
        case Left(EmptyResult.Unchanged) => onUnchanged()
        case Left(EmptyResult.NoMatch)   => onNoMatch()
      }
  }

  implicit class XtensionTextDocumentPositionParams(
    params: l.TextDocumentPositionParams
  ) {
    def printed(buffers: Buffers): String = {
      val path  = params.getTextDocument.getUri.osPathFromUri
      val input = path.toInputFromBuffers(buffers)
      Try(
        params
          .getPosition
          .toMeta(input)
          .map { meta =>
            CompilerRangeParamsUtils
              .offsetOrRange(meta, EmptyCancelToken)
              .printed()
          }
      ).toOption.flatten
        .getOrElse(input.text)
    }
  }

  implicit class XtensionStringDoc(doc: String) {
    def isScala: Boolean =
      doc.endsWith(".scala")
    def isSbt: Boolean =
      doc.endsWith(".sbt")
    def isScalaScript: Boolean =
      doc.endsWith(".sc")
    def isScalaFilename: Boolean =
      doc.isScala || isScalaScript || isSbt || isMill
    def isMill: Boolean =
      doc.endsWith(".mill") ||
      doc.endsWith(".mill.scala") ||
      doc.endsWith("/build.sc")
    def isJavaFilename: Boolean =
      doc.endsWith(".java")

    def findIndicesOf(symbols: List[Char]): List[Int] = {
      @tailrec
      def loop(
        index: Int,
        afterEscape: Boolean,
        inBackticks: Boolean,
        acc: List[Int]
      ): List[Int] =
        if (index >= doc.length()) acc.reverse
        else {
          val c = doc.charAt(index)
          val newAcc =
            if (symbols.contains(c) && !inBackticks && !afterEscape)
              index :: acc
            else acc
          loop(
            index + 1,
            afterEscape = c == '\\',
            inBackticks = c == '`' ^ inBackticks,
            acc = newAcc
          )
        }
      loop(
        index = 0,
        afterEscape = false,
        inBackticks = false,
        acc = List.empty
      )
    }
  }

  implicit class XtensionVirtualFileParams(params: VirtualFileParams) {
    def printed(marker: String = "@@"): String = {
      def textWithPosMarker(markerPos: Int, text: String) =
        if (markerPos < 0) marker ++ text
        else if (markerPos >= text.length()) text ++ marker
        else {
          val (head, tail) = text.splitAt(markerPos)
          head ++ marker ++ tail
        }
      params match {
        case r: RangeOffset =>
          val withStartMarker = textWithPosMarker(r.start, r.text())
          val withMarkers =
            textWithPosMarker(r.end + marker.length(), withStartMarker)
          s"""|range: ${r.start} - ${r.end}
              |uri: ${r.uri()}
              |text:
              |```scala
              |$withMarkers
              |```
              |""".stripMargin
        case o: OffsetParams =>
          s"""|offset: ${o.offset()}
              |uri: ${o.uri()}
              |text:
              |```scala
              |${textWithPosMarker(o.offset(), o.text())}
              |```
              |""".stripMargin
        case v =>
          s"""|uri: ${v.uri()}
              |text:
              |```scala
              |${v.text()}
              |```
              |""".stripMargin
      }
    }
  }

  private object DiagnosticCodes {
    import org.eclipse.lsp4j.jsonrpc.messages.{Either => LspEither}

    val Unused: Int = 198

    def isEqual(lspCode: LspEither[String, Integer], other: Int): Boolean =
      lspCode.asScala match {
        case Left(strCode)  => strCode == other.toString()
        case Right(intCode) => intCode == other
      }
  }

  implicit class XtensionDiagnosticBsp(diag: b.Diagnostic) {
    def toLsp: l.Diagnostic = {
      val ld = new l.Diagnostic(
        diag.getRange.toLsp,
        fansi.Str(diag.getMessage, fansi.ErrorMode.Strip).plainText,
        if (diag.getSeverity == null) l.DiagnosticSeverity.Warning
        else diag.getSeverity.toLsp,
        diag.getSource
      )

      for (code <- Option(diag.getCode))
        ld.setCode(code)

      for (tags <- Option(diag.getTags)) {
        val converted = tags.asScala.flatMap {
          case num if l.DiagnosticTag.Unnecessary.getValue == num =>
            Some(l.DiagnosticTag.Unnecessary)
          case num if l.DiagnosticTag.Deprecated.getValue == num =>
            Some(l.DiagnosticTag.Deprecated)
          case _ => None
        }
        val all =
          if (DiagnosticCodes.isEqual(diag.getCode, DiagnosticCodes.Unused))
            converted :+ l.DiagnosticTag.Unnecessary
          else converted
        ld.setTags(all.distinct.asJava)
      }

      Option(diag.getCodeDescription).foreach { codeDesc =>
        ld.setCodeDescription(
          new l.DiagnosticCodeDescription(codeDesc.getHref)
        )
      }

      Option(diag.getRelatedInformation).foreach { related =>
        ld.setRelatedInformation(related.asScala.map { r =>
          new l.DiagnosticRelatedInformation(
            new l.Location(
              r.getLocation.getUri,
              r.getLocation.getRange.toLsp
            ),
            r.getMessage
          )
        }.asJava)
      }

      ld.setData(diag.getData)
      ld
    }
  }

  implicit class XtensionSeverityBsp(sev: b.DiagnosticSeverity) {
    def toLsp: l.DiagnosticSeverity =
      l.DiagnosticSeverity.forValue(sev.getValue)
  }

  implicit class XtensionPositionBsp(pos: b.Position) {
    def toLsp: l.Position =
      new l.Position(pos.getLine, pos.getCharacter)
  }

  implicit class XtensionRangeBsp(range: b.Range) {
    def toLsp: l.Range =
      new l.Range(range.getStart.toLsp, range.getEnd.toLsp)
  }

  implicit class XtensionScalaFuture[A](future: Future[A]) {
    def ignoreValue(implicit ec: ExecutionContext): Future[Unit] =
      future.map(_ => ())
    def asJava: CompletableFuture[A] =
      FutureConverters.toJava(future).toCompletableFuture
    def asJavaObject: CompletableFuture[Object] =
      future.asJava.asInstanceOf[CompletableFuture[Object]]
  }

  implicit class XtensionScalaAction(scalaAction: b.ScalaAction) {
    def asLspTextEdits: Seq[l.TextEdit] =
      scalaAction
        .getEdit
        .getChanges
        .asScala
        .toSeq
        .map(edit => new l.TextEdit(edit.getRange.toLsp, edit.getNewText))

    def setEditFromLspTextEdits(lspTextEdits: Seq[l.TextEdit]): Unit = {
      val scalaWorkspaceEdit =
        new b.ScalaWorkspaceEdit(
          lspTextEdits.map { edit =>
            new b.ScalaTextEdit(edit.getRange.toBsp, edit.getNewText)
          }.asJava
        )
      scalaAction.setEdit(scalaWorkspaceEdit)
    }
  }

  implicit class XtensionJavacOptions(item: b.JavacOptionsItem) {
    def targetroot: Option[os.Path] =
      item.getOptions.asScala
        .find(_.startsWith("-Xplugin:semanticdb"))
        .map { arg =>
          val targetRootOpt = "-targetroot:"
          val sourceRootOpt = "-sourceroot:"
          val targetRootPos = arg.indexOf(targetRootOpt)
          val sourceRootPos = arg.indexOf(sourceRootOpt)
          if (targetRootPos > sourceRootPos)
            arg.substring(targetRootPos + targetRootOpt.size).trim()
          else
            arg
              .substring(sourceRootPos + sourceRootOpt.size, targetRootPos - 1)
              .trim()
        }
        .filter(_ != "javac-classes-directory")
        .map(os.Path(_))
        .orElse {
          val classes = item.getClassDirectory
          Option.when(classes.nonEmpty)(classes.osPathFromUri)
        }

    def sourceroot: Option[String] =
      item.getOptions.asScala
        .find(_.startsWith("-Xplugin:semanticdb"))
        .flatMap { value =>
          val idx = value.indexOf("-sourceroot:")
          if (idx >= 0) Some(value.drop(idx + "-sourceroot:".length))
          else None
        }
        .map { value =>
          val idx = value.indexOf(" -targetroot")
          if (idx >= 0) value.take(idx)
          else value
        }

    def isSemanticdbEnabled: Boolean =
      item.getOptions.asScala.exists { opt =>
        opt.startsWith("-Xplugin:semanticdb")
      }

    def isSourcerootDeclared: Boolean =
      item.getOptions.asScala
        .find(_.startsWith("-Xplugin:semanticdb"))
        .map(_.contains("-sourceroot:"))
        .getOrElse(false)

    def isTargetrootDeclared: Boolean =
      item.getOptions.asScala
        .find(_.startsWith("-Xplugin:semanticdb"))
        .map(_.contains("-targetroot:"))
        .getOrElse(false)

    def releaseVersion: Option[String] =
      item.getOptions.asScala
        .dropWhile(_ != "--release")
        .drop(1)
        .headOption

    def sourceVersion: Option[String] =
      item.getOptions.asScala
        .dropWhile(f => f != "--source" && f != "-source" && f != "--release")
        .drop(1)
        .headOption

    def targetVersion: Option[String] =
      item.getOptions.asScala
        .dropWhile(f => f != "--target" && f != "-target" && f != "--release")
        .drop(1)
        .headOption
  }

  implicit class XtensionScalacOptions(item: b.ScalacOptionsItem) {
    def targetroot(scalaVersion: String): os.Path =
      if (ScalaVersions.isScala3Version(scalaVersion)) {
        val options = item.getOptions.asScala
        val targetOption =
          if (options.contains("-semanticdb-target")) {
            val index = options.indexOf("-semanticdb-target") + 1
            if (options.size > index) Some(os.Path(options(index)))
            else None
          }
          else
            None
        targetOption.getOrElse(item.getClassDirectory.osPathFromUri)
      }
      else
        semanticdbFlag("targetroot")
          .map(os.Path(_))
          .getOrElse(item.getClassDirectory.osPathFromUri)

    def isSemanticdbEnabled(scalaVersion: String): Boolean =
      if (ScalaVersions.isScala3Version(scalaVersion))
        item.getOptions.asScala.exists { opt =>
          opt == "-Ysemanticdb" || opt == "-Xsemanticdb"
        }
      else
        item.getOptions.asScala.exists { opt =>
          opt.startsWith("-Xplugin:") &&
          opt.contains("semanticdb-scalac")
        }

    def isSourcerootDeclared(scalaVersion: String): Boolean = {
      val soughtOption =
        if (ScalaVersions.isScala3Version(scalaVersion)) "-sourceroot"
        else "-P:semanticdb:sourceroot"
      item.getOptions.asScala.exists { option =>
        option.startsWith(soughtOption)
      }
    }

    def isJVM: Boolean =
      // FIXME: https://github.com/scalacenter/bloop/issues/700
      !item.getOptions.asScala.exists(_.isNonJVMPlatformOption)

    def sourceroot(scalaVersion: String): Option[String] = {
      val soughtOption =
        if (ScalaVersions.isScala3Version(scalaVersion)) "-sourceroot"
        else "-P:semanticdb:sourceroot"
      val options = item.getOptions.asScala.toVector
      val idx     = options.indexOf(soughtOption)
      if (idx >= 0 && idx < options.length - 1) Some(options(idx + 1))
      else None
    }

    /** Returns the value of a -P:semanticdb:$option:$value compiler flag.
      */
    def semanticdbFlag(name: String): Option[String] = {
      val flag = s"-P:semanticdb:$name:"
      item.getOptions.asScala
        .find(_.startsWith(flag))
        .map(_.stripPrefix(flag))
    }
  }

  implicit class XtensionDependencyModule(module: b.DependencyModule) {
    def asMavenDependencyModule: Option[b.MavenDependencyModule] =
      if (module.getDataKind == b.DependencyModuleDataKind.MAVEN)
        decodeJson(module.getData, classOf[b.MavenDependencyModule])
      else
        None
  }

  implicit class XtensionBuildTarget(buildTarget: b.BuildTarget) {

    def isSbtBuild: Boolean = dataKind == "sbt"

    def isScalaBuild: Boolean = dataKind == "scala"

    def dataKind: String = Option(buildTarget.getDataKind).getOrElse("")

    def asScalaBuildTarget: Option[b.ScalaBuildTarget] =
      asSbtBuildTarget
        .map(_.getScalaBuildTarget)
        .orElse(
          if (isScalaBuild) decodeJson(buildTarget.getData, classOf[b.ScalaBuildTarget])
          else None
        )

    def asSbtBuildTarget: Option[b.SbtBuildTarget] =
      if (isSbtBuild) decodeJson(buildTarget.getData, classOf[b.SbtBuildTarget])
      else None

    def baseDirectory: String =
      Option(buildTarget.getBaseDirectory).getOrElse("")

    def getName: String =
      if (buildTarget.getDisplayName == null) {
        val name =
          if (buildTarget.getBaseDirectory != null)
            buildTarget
              .getId
              .getUri
              .stripPrefix(buildTarget.getBaseDirectory)
          else
            buildTarget.getId.getUri

        name.replaceAll("[^a-zA-Z0-9]+", "-")
      }
      else
        buildTarget.getDisplayName
  }

  implicit class XtensionClasspath(classpath: List[String]) {
    def toAbsoluteClasspath: Iterator[os.Path] =
      classpath.iterator
        .map(_.osPathFromUri)
  }

  implicit class XtensionClientCapabilities(params: l.InitializeParams) {
    def supportsCompletionSnippets: Boolean =
      Option(params.getCapabilities)
        .flatMap(cap => Option(cap.getTextDocument))
        .flatMap(doc => Option(doc.getCompletion))
        .flatMap(comp => Option(comp.getCompletionItem))
        .flatMap(item => Option(item.getSnippetSupport))
        .map(_.booleanValue)
        .getOrElse(false)
  }

  implicit class XtensionScanner(scanner: LegacyScanner) {

    import scala.meta.internal.tokenizers.LegacyToken._

    def foreach(f: LegacyTokenData => Unit): Unit = {
      scanner.initialize()
      var curr = scanner.nextToken()
      while (curr.token != EOF) {
        f(curr)
        curr = scanner.nextToken()
      }
    }
  }

  implicit class XtensionLspRange(range: l.Range) {
    def encloses(position: l.Position): Boolean =
      range.getStart <= position && position <= range.getEnd
    def encloses(other: l.Range): Boolean =
      encloses(other.getStart) && encloses(other.getEnd)
    def toBsp: b.Range =
      new b.Range(range.getStart.toBsp, range.getEnd.toBsp)
  }

  implicit class XtensionInputOffset(input: Input) {
    def toOffset(line: Int, column: Int): Int =
      scala.meta.PlasmonHelpers.inputLineToOffset(input, line) + column

    def safeParse[T <: Tree](dialect: Dialect)(implicit
      parse: scala.meta.parsers.Parse[T]
    ): scala.meta.parsers.Parsed[T] =
      try parse(input, dialect)
      catch {
        case t: InvariantFailedException =>
          scala.meta.parsers.Parsed.Error(Position.None, t.toString(), t)
      }
  }

  implicit class XtensionSymbolInformation(info: s.SymbolInformation) {
    // Metals comment: This works only for SymbolInformation produced in metals in `ScalaTopLevelMtags`.
    def isExtension: Boolean = (EXTENSION & info.properties) != 0
  }

  private val EXTENSION: Int = s.SymbolInformation.Property.values.map(_.value).max << 1

  implicit class XtensionSymbolInformationKind(kind: s.SymbolInformation.Kind) {
    def isRelevantKind: Boolean =
      kind match {
        case k.OBJECT | k.PACKAGE_OBJECT | k.CLASS | k.TRAIT | k.INTERFACE |
            k.METHOD | k.TYPE =>
          true
        case _ => false
      }
  }

  implicit class XtensionToken(token: Token) {
    def isWhiteSpaceOrComment: Boolean =
      token match {
        case _: Token.Whitespace | _: Token.Comment =>
          true
        case _ => false
      }
  }

  implicit class XtensionCancelChecker(token: l.jsonrpc.CancelChecker) {
    def isCancelled: Boolean =
      try {
        token.checkCanceled()
        false
      }
      catch {
        case _: CancellationException =>
          true
      }
  }

  implicit class XtensionDiagnosticLSP(d: l.Diagnostic) {
    def formatMessage(uri: String, hint: String): String = {
      val severity = d.getSeverity.toString.toLowerCase()
      s"$severity:$hint $uri:${d.getRange.getStart.getLine} ${d.getMessage}"
    }
    def asTextEdit: Option[l.TextEdit] =
      decodeJson(d.getData, classOf[l.TextEdit])

    /** Useful for decoded the diagnostic data since there are overlapping unrequired keys in the
      * structure that causes issues when we try to deserialize the old top level text edit vs the
      * newly nested actions.
      */
    object DiagnosticDataDeserializer
        extends JsonDeserializer[Either[l.TextEdit, b.ScalaDiagnostic]] {
      override def deserialize(
        json: JsonElement,
        typeOfT: Type,
        context: JsonDeserializationContext
      ): Either[l.TextEdit, b.ScalaDiagnostic] =
        json match {
          case o: JsonObject if o.has("actions") =>
            Right(new Gson().fromJson(o, classOf[b.ScalaDiagnostic]))
          case o => Left(new Gson().fromJson(o, classOf[l.TextEdit]))
        }
    }

    def asScalaDiagnostic: Option[Either[l.TextEdit, b.ScalaDiagnostic]] = {
      val gson = new GsonBuilder()
        .registerTypeAdapter(
          classOf[Either[l.TextEdit, b.ScalaDiagnostic]],
          DiagnosticDataDeserializer
        )
        .create()
      decodeJson(
        d.getData,
        classOf[Either[l.TextEdit, b.ScalaDiagnostic]],
        Some(gson)
      )
    }
  }

  implicit class XtensionOsPath(path: os.Path) {
    def toAbsPath: AbsolutePath =
      AbsolutePath(path.toNIO)
    def toInput: Input.VirtualFile =
      Input.VirtualFile(
        path.toNIO.toUri.toASCIIString,
        os.read(path)
      )
    def toInputFromBuffers(buffers: Buffers): Input.VirtualFile =
      buffers.get(path) match {
        case Some(text) => Input.VirtualFile(path.toNIO.toUri.toASCIIString, text)
        case None       => path.toInput
      }
    def isSameFileSystem(other: os.Path): Boolean =
      path.toNIO.getFileSystem == other.toNIO.getFileSystem

    def isJava: Boolean =
      toLanguage == Language.JAVA && os.isFile(path)
    def isScala: Boolean =
      toLanguage == Language.SCALA && os.isFile(path)
    def isJar: Boolean =
      path.last.endsWith(".jar") || path.last.endsWith(".srcjar")
    def isSbt: Boolean =
      path.last.endsWith(".sbt")
    def isSemanticdb: Boolean =
      path.last.endsWith(".semanticdb")
    def isMill: Boolean =
      path.last.endsWith(".mill") ||
      path.last.endsWith(".mill.scala") || path.last == "build.sc"
    def isScalaScript: Boolean =
      path.last.endsWith(".sc") && !isMill

    def resolveIfJar: os.Path =
      if (isJar)
        path / os.up / "_semanticdb" / path.last.stripSuffix(".jar").stripSuffix(".srcjar")
      else
        path

    // Using [[Files.isSymbolicLink]] is not enough.
    // It will be false when one of the parents is a symlink (e.g. /dir/link/file.txt)
    def dealias: os.Path =
      if (os.exists(path)) os.Path(path.toNIO.maybeToRealPath)
      else path

    def toLanguage: Language =
      filenameToLanguage(path.last)
    def isScalaOrJava: Boolean =
      toLanguage match {
        case Language.SCALA | Language.JAVA => os.isFile(path)
        case _                              => false
      }
    def isJavaFilename: Boolean =
      path.last.isJavaFilename
    def isScalaFilename: Boolean =
      path.last.isScalaFilename

    def toIdeallyRelativeURI(sourceItemOpt: Option[os.Path]): String =
      sourceItemOpt match {
        case Some(sourceItem) =>
          if (sourceItem.isScalaOrJava)
            sourceItem.last
          else
            path.relativeTo(sourceItem).toNIO.toUri.toASCIIString
        case None =>
          path.toNIO.toUri.toASCIIString
      }

    def isInReadonlyDirectory(workspace: os.Path): Boolean =
      path.startsWith(workspace / Directories.readonly)
    def isJarFileSystem: Boolean =
      path.toNIO.getFileSystem.provider().getScheme.equals("jar")
    def isDependencySource(workspace: os.Path): Boolean =
      (isSameFileSystem(workspace) && isInReadonlyDirectory(workspace)) ||
      isJarFileSystem

    def jarPath: Option[os.Path] = {
      val filesystem = path.toNIO.getFileSystem
      Option.when(filesystem.provider().getScheme.equals("jar")) {
        os.Path(Paths.get(filesystem.toString))
      }
    }
  }

  implicit class OsSubPathExt(path: os.SubPath) {
    def toUri(isDirectory: Boolean): URI = {
      val b = new StringBuilder
      for (elem <- path.segments) {
        if (b.nonEmpty) b.append('/')
        b.append(new URI(null, null, elem, null))
      }
      if (isDirectory) b.append('/')
      URI.create(b.result())
    }
  }

  implicit class InitParamsExt(params: l.InitializeParams) {

    def codeLensRefreshSupport: Boolean = {
      val opt: Option[Boolean] = for {
        capabilities   <- Option(params.getCapabilities)
        workspace      <- Option(capabilities.getWorkspace)
        codeLens       <- Option(workspace.getCodeLens)
        refreshSupport <- Option(codeLens.getRefreshSupport)
      } yield refreshSupport
      opt.getOrElse(false)
    }

    def applyEditSupport: Boolean = {
      val opt: Option[Boolean] = for {
        capabilities <- Option(params.getCapabilities)
        workspace    <- Option(capabilities.getWorkspace)
        support      <- Option(workspace.getApplyEdit)
      } yield support
      opt.getOrElse(false)
    }
  }

  implicit class BuildTargetIdentifierExt(targetId: b.BuildTargetIdentifier) {
    def module: GlobalSymbolIndex.Module =
      GlobalSymbolIndex.BuildTarget(targetId.getUri)
  }
}
