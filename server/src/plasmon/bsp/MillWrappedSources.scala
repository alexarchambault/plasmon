package plasmon.bsp

import ch.epfl.scala.bsp4j as b
import plasmon.PlasmonEnrichments.*

import scala.build.bsp.{WrappedSourceItem, WrappedSourcesItem, WrappedSourcesResult}
import scala.jdk.CollectionConverters.*

/** Works out the wrapped sources of Mill builds that don't report them.
  *
  * Since Mill 1.3, the sources Mill generates out of `build.mill` / `package.mill` files have the
  * user code preceded by a `///SOURCE_CODE_START:<original path>` line, but `buildTarget/wrappedSources`
  * still looks for the former `//SOURCECODE_ORIGINAL_CODE_START_MARKER` one, and reports nothing.
  * The generated sources are still around, next to the `generatedScriptSources.dest/support`
  * directory Mill reports as a generated source directory, so we compute the wrappers from them,
  * the way Mill should.
  */
object MillWrappedSources {

  private def userCodeStartMarker = "///SOURCE_CODE_START:"

  /** Splits a generated source into the code before and after the user code.
    *
    * The user code starts right after the marker line, and has the same length as the original
    * source (Mill only makes length-preserving edits to it, as `-Ymagic-offset-header` relies on
    * offsets), so what follows it is the code after `originalLength` characters.
    *
    * @return
    *   the path of the original source, the top wrapper, and the bottom wrapper
    */
  def wrappers(
    generatedContent: String,
    originalLength: os.Path => Option[Int]
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
      length <- originalLength(original)
      userCodeEnd = markerLineEnd + 1 + length
      if userCodeEnd <= generatedContent.length
    } yield (
      original,
      generatedContent.take(markerLineEnd + 1),
      generatedContent.drop(userCodeEnd)
    )
  }

  /** Wrapped sources of the targets Mill reported none for, computed from their generated sources */
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
          p => if (os.isFile(p)) Some(os.read(p).length) else None
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
      val kept            = wrappedSources.getItems.asScala.filter(i => !computedTargets(i.getTarget))
      new WrappedSourcesResult((kept ++ computed).asJava)
    }
  }
}
