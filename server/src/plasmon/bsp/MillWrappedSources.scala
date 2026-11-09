package plasmon.bsp

import ch.epfl.scala.bsp4j as b
import plasmon.PlasmonEnrichments.*

import scala.build.bsp.{WrappedSourceItem, WrappedSourcesItem, WrappedSourcesResult}
import scala.jdk.CollectionConverters.*

/** Works out the wrapped sources of Mill builds that don't report them.
  *
  * Since Mill 1.3, the sources Mill generates out of `build.mill` / `package.mill` files have the
  * user code preceded by a `///SOURCE_CODE_START:<original path>` line, but
  * `buildTarget/wrappedSources` still looks for the former
  * `//SOURCECODE_ORIGINAL_CODE_START_MARKER` one, and reports nothing. The generated sources are
  * still around, next to the `generatedScriptSources.dest/support` directory Mill reports as a
  * generated source directory, so we compute the wrappers from them, the way Mill should.
  */
object MillWrappedSources {

  private def userCodeStartMarker = "///SOURCE_CODE_START:"

  /** Splits a generated source into the code before and after the user code.
    *
    * The user code starts right after the marker line, and spans as many lines as the original
    * source, so what follows it is the code after that many lines.
    *
    * Its length can differ from the original's: Mill splices the user code into a template it then
    * passes to `stripMargin` (as of Mill 1.3.1), so user lines starting with `<spaces>|` (in
    * multi-line `stripMargin` strings) lose their margin. That keeps lines intact, though.
    *
    * @param originalContent
    *   the content of an original source, if it can be read
    * @return
    *   the path of the original source, the top wrapper, and the bottom wrapper
    */
  def wrappers(
    generatedContent: String,
    originalContent: os.Path => Option[String]
  ): Option[(os.Path, String, String)] = {
    val markerIdx =
      if (generatedContent.startsWith(userCodeStartMarker)) 0
      else
        generatedContent.indexOf("\n" + userCodeStartMarker) match {
          case -1 => -1
          case n  => n + 1
        }
    val markerLineEnd =
      if (markerIdx < 0) -1
      else generatedContent.indexOf('\n', markerIdx)
    for {
      _ <- Some(()).filter(_ => markerLineEnd >= 0)
      original <- {
        val path = generatedContent
          .substring(markerIdx + userCodeStartMarker.length, markerLineEnd)
          .trim
        try Some(os.Path(path))
        catch {
          case _: IllegalArgumentException => None
        }
      }
      content     <- originalContent(original)
      userCodeEnd <- userCodeEnd(generatedContent, markerLineEnd + 1, content)
    } yield (
      original,
      generatedContent.take(markerLineEnd + 1),
      generatedContent.drop(userCodeEnd)
    )
  }

  /** Where the user code starting at `start` in `generatedContent` ends, given the original code.
    *
    * After as many line breaks as in the original code, and, if the original doesn't end with one,
    * after the line that follows.
    */
  private def userCodeEnd(
    generatedContent: String,
    start: Int,
    originalContent: String
  ): Option[Int] = {
    val lineBreaks = originalContent.count(_ == '\n')
    val afterLineBreaks = Iterator
      .iterate(Option(start)) { idxOpt =>
        idxOpt.flatMap { idx =>
          generatedContent.indexOf('\n', idx) match {
            case -1 => None
            case n  => Some(n + 1)
          }
        }
      }
      .drop(lineBreaks)
      .next()
    afterLineBreaks.map { idx =>
      if (originalContent.isEmpty || originalContent.endsWith("\n")) idx
      else {
        // the last line of the original, without line break
        val lineEnd = generatedContent.indexOf('\n', idx) match {
          case -1 => generatedContent.length
          case n  => n
        }
        // don't leave a '\r' of a "\r\n" line break in the user code
        if (
          lineEnd > idx && generatedContent.charAt(lineEnd - 1) == '\r' &&
          !originalContent.endsWith("\r")
        )
          lineEnd - 1
        else lineEnd
      }
    }
  }

  /** Wrapped sources of the targets Mill reported none for, computed from their generated sources
    */
  def fallback(
    wrappedSources: WrappedSourcesResult,
    sources: b.SourcesResult
  ): WrappedSourcesResult = {

    val withWrappedSources = wrappedSources.getItems.asScala
      .filter(!_.getSources.isEmpty)
      .map(_.getTarget)
      .toSet

    val computed = for {
      item <- sources.getItems.asScala.toList
      if !withWrappedSources.contains(item.getTarget)
      wrappedDirs = item.getSources.asScala.toList
        .filter(_.getGenerated)
        .flatMap(_.getUri.maybeOsPathFromUri)
        .filter(p => p.last == "support" && (p / os.up).last == "generatedScriptSources.dest")
        .map(_ / os.up / "wrapped")
        .filter(os.isDir)
      sourceItems = for {
        dir       <- wrappedDirs
        generated <- os.walk(dir).filter(_.last.endsWith(".mill")).filter(os.isFile)
        (original, top, bottom) <- wrappers(
          os.read(generated),
          p => if (os.isFile(p)) Some(os.read(p)) else None
        )
      } yield {
        val sourceItem = new WrappedSourceItem(
          original.toNIO.toUri.toASCIIString,
          generated.toNIO.toUri.toASCIIString
        )
        sourceItem.setTopWrapper(top)
        sourceItem.setBottomWrapper(bottom)
        sourceItem
      }
      if sourceItems.nonEmpty
    } yield new WrappedSourcesItem(item.getTarget, sourceItems.asJava)

    if (computed.isEmpty) wrappedSources
    else {
      val computedTargets = computed.map(_.getTarget).toSet
      val kept = wrappedSources.getItems.asScala.filter(i => !computedTargets(i.getTarget))
      new WrappedSourcesResult((kept ++ computed).asJava)
    }
  }
}
