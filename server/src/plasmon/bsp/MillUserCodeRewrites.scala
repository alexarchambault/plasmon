package plasmon.bsp

import scala.jdk.CollectionConverters.*

/** The edits Mill makes to the user code it copies into the sources it generates out of build files
  *
  * Mill doesn't copy the code of `build.mill` / `package.mill` files verbatim into the sources it
  * compiles. All versions since 0.12 rewrite the header of an `object `package``:
  *   - since 1.2 / 1.3.0-M1, while keeping offsets: `object `package` extends ScalaModule {`
  *     becomes `class  package_  extends _MillRootMo {`, the actual parents going to an `abstract
  *     class _MillRootMo` stub after the user code, and package clauses get commented out
  *     (`//ckage`),
  *   - before, into `abstract class package_ extends <Mill parent>, ScalaModule {`, dropping
  *     package clauses.
  *
  * Code generated around the user code relies on these, like an `object package_ extends package_`
  * that expects a `class package_` in the user code. So the presentation compiler needs the
  * rewritten code too. Rather than replicating what each Mill version does, we compare the user
  * code in the generated source with the original one, and replay the changes on the code being
  * edited.
  *
  * @param base
  *   lines of the original source the rewrites were computed from
  * @param rewritten
  *   rewritten lines, by index in `base`
  */
final class MillUserCodeRewrites private (
  base: IndexedSeq[String],
  rewritten: Map[Int, String]
) {
  import MillUserCodeRewrites.*

  def isEmpty: Boolean = rewritten.isEmpty

  /** Applies the rewrites to the current content of the original source
    *
    * Lines left as is since the rewrites were computed get their rewritten version. On edited
    * lines, the parts that were rewritten get rewritten again, if they can still be found as is,
    * and only once, on the line.
    */
  def applyTo(content: String): String =
    apply(content).content

  /** Applies the rewrites to the current content of the original source, see [[applyTo]]
    *
    * @return
    *   the rewritten content, and the changes made to it
    */
  def apply(content: String): Applied =
    if (rewritten.isEmpty) Applied(content, Nil)
    else {
      val contentLines = content.linesWithSeparators.toVector
      val contentLinesNoSep =
        contentLines.map(_.stripLineEnd)
      // base line index -> content line index, for lines kept or edited in place
      val pairs     = Map.newBuilder[Int, Int]
      var baseIdx   = 0
      var contentIx = 0
      val deltas = difflib.DiffUtils
        .diff(base.asJava, contentLinesNoSep.asJava)
        .getDeltas
        .asScala
        .sortBy(_.getOriginal.getPosition)
      for (delta <- deltas) {
        val orig = delta.getOriginal
        val rev  = delta.getRevised
        while (baseIdx < orig.getPosition) {
          pairs += baseIdx -> contentIx
          baseIdx += 1
          contentIx += 1
        }
        if (orig.size == rev.size)
          for (i <- 0 until orig.size)
            pairs += (orig.getPosition + i) -> (rev.getPosition + i)
        else {
          // pair the rewritten lines with the most similar edited lines, keeping their order
          var nextRev = rev.getPosition
          for {
            i <- orig.getPosition until orig.getPosition + orig.size
            if rewritten.contains(i)
          } {
            val candidates = (nextRev until rev.getPosition + rev.size)
              .map(j => (j, similarity(base(i), contentLinesNoSep(j))))
              .filter(_._2 > 0)
            if (candidates.nonEmpty) {
              val best = candidates.maxBy(_._2)._1
              pairs += i -> best
              nextRev = best + 1
            }
          }
        }
        baseIdx = orig.getPosition + orig.size
        contentIx = rev.getPosition + rev.size
      }
      while (baseIdx < base.length) {
        pairs += baseIdx -> contentIx
        baseIdx += 1
        contentIx += 1
      }

      val replacements = pairs.result().iterator.flatMap {
        case (baseIdx, contentIdx) =>
          rewritten.get(baseIdx).filter(_ => contentIdx < contentLines.length).flatMap {
            rewrittenLine =>
              val line = contentLinesNoSep(contentIdx)
              val newLineAndChanges =
                if (line == base(baseIdx))
                  Some((rewrittenLine, tokenChanges(base(baseIdx), rewrittenLine)))
                else rewriteEditedLine(base(baseIdx), rewrittenLine, line)
              newLineAndChanges.map {
                case (newLine, changes) =>
                  contentIdx -> (
                    newLine + contentLines(contentIdx).drop(line.length),
                    changes.map {
                      case (col, from, to) =>
                        Change(contentIdx, col, from, to)
                    }
                  )
              }
          }
      }.toMap

      if (replacements.isEmpty) Applied(content, Nil)
      else
        Applied(
          contentLines.iterator.zipWithIndex
            .map {
              case (line, idx) =>
                replacements.get(idx).fold(line)(_._1)
            }
            .mkString,
          replacements.toVector.sortBy(_._1).flatMap(_._2._2)
        )
    }

  override def toString: String =
    s"MillUserCodeRewrites(${rewritten.toVector.sortBy(_._1).map {
        case (idx, line) => s"$idx: ${base(idx)} -> $line"
      }.mkString(", ")})"
}

object MillUserCodeRewrites {

  /** A change made to the user code
    *
    * @param line
    *   line of the change in the user code
    * @param col
    *   column of the change, on the line of the user code before it gets rewritten
    * @param from
    *   user code replaced, at `col`
    * @param to
    *   what `from` gets replaced with
    */
  final case class Change(line: Int, col: Int, from: String, to: String)

  /** User code once rewritten, and the changes it underwent */
  final case class Applied(content: String, changes: Seq[Change])

  /** Code of the user code that has a copy after it, in the generated code
    *
    * Mill moves the parents of an `object `package`` to a stub class after the user code. Queries
    * on these parents in the user code are better answered by the compiler on that copy (which is
    * the actual code) than on the stub that replaces them.
    *
    * @param userLine
    *   line of the code in the user code
    * @param userCol
    *   column of the code in the user code
    * @param length
    *   length of the code
    * @param compilerLine
    *   line of the copy in the code passed to the compiler
    * @param compilerCol
    *   column of the copy in the code passed to the compiler
    */
  final case class Redirect(
    userLine: Int,
    userCol: Int,
    length: Int,
    compilerLine: Int,
    compilerCol: Int
  ) {

    /** The position of the copy for a user position, if it's in the redirected code */
    def toCompiler(line: Int, char: Int): Option[(Int, Int)] =
      Option.when(line == userLine && char >= userCol && char <= userCol + length) {
        (compilerLine, compilerCol + char - userCol)
      }

    /** The user position for a position in the copy, if it's in it */
    def toUser(line: Int, char: Int): Option[(Int, Int)] =
      Option.when(line == compilerLine && char >= compilerCol && char <= compilerCol + length) {
        (userLine, userCol + char - compilerCol)
      }
  }

  private def isIdentifierChar(c: Char): Boolean =
    c.isLetterOrDigit || c == '_' || c == '$'

  /** Redirects for the changes whose original code can be found in the code after the user code
    *
    * @param bottomLine
    *   line where `bottomWrapper` starts, in the code passed to the compiler
    * @param bottomCol
    *   column where `bottomWrapper` starts, in the code passed to the compiler
    */
  def redirects(
    changes: Seq[Change],
    bottomWrapper: String,
    bottomLine: Int,
    bottomCol: Int
  ): Seq[Redirect] =
    changes.flatMap { change =>
      val from = change.from
      def boundaries(idx: Int): Boolean =
        (!isIdentifierChar(from.head) || idx == 0 ||
          !isIdentifierChar(bottomWrapper.charAt(idx - 1))) &&
        (!isIdentifierChar(from.last) || idx + from.length >= bottomWrapper.length ||
        !isIdentifierChar(bottomWrapper.charAt(idx + from.length)))
      val idxOpt =
        if (from.trim.isEmpty) None
        else
          Iterator
            .iterate(bottomWrapper.lastIndexOf(from))(idx =>
              if (idx <= 0) -1 else bottomWrapper.lastIndexOf(from, idx - 1)
            )
            .takeWhile(_ >= 0)
            .find(boundaries)
      idxOpt.map { idx =>
        val before        = bottomWrapper.substring(0, idx)
        val lineBreaks    = before.count(_ == '\n')
        val lastLineBreak = before.lastIndexOf('\n')
        Redirect(
          change.line,
          change.col,
          from.length,
          bottomLine + lineBreaks,
          if (lastLineBreak < 0) bottomCol + idx else idx - lastLineBreak - 1
        )
      }
    }

  val empty: MillUserCodeRewrites = new MillUserCodeRewrites(Vector.empty, Map.empty)

  /** The user code in a generated source, given the code generated before and after it
    */
  def userCode(
    generatedContent: String,
    topWrapper: String,
    bottomWrapper: String
  ): Option[String] =
    Option.when(
      generatedContent.length >= topWrapper.length + bottomWrapper.length &&
      generatedContent.startsWith(topWrapper) &&
      generatedContent.endsWith(bottomWrapper)
    ) {
      generatedContent.substring(
        topWrapper.length,
        generatedContent.length - bottomWrapper.length
      )
    }

  /** Computes the rewrites that turned the user code of `original` into `userCode`
    *
    * Only lines changed in place are considered. Lines whose only change is a stripped margin are
    * ignored: Mill passes the user code to `stripMargin` (as of Mill 1.3 / 1.4), which isn't a
    * rewrite of its own, and we'd rather keep the user offsets on these lines.
    */
  def compute(original: String, userCode: String): MillUserCodeRewrites =
    if (original == userCode) empty
    else {
      val base      = original.linesIterator.toVector
      val generated = userCode.linesIterator.toVector
      val rewritten = difflib.DiffUtils
        .diff(base.asJava, generated.asJava)
        .getDeltas
        .asScala
        .iterator
        .filter(d => d.getOriginal.size == d.getRevised.size)
        .flatMap { delta =>
          val orig = delta.getOriginal
          val rev  = delta.getRevised
          orig.getLines.asScala.iterator
            .zip(rev.getLines.asScala.iterator)
            .zipWithIndex
            .collect {
              case ((origLine, revLine), i) if origLine.stripMargin != revLine =>
                (orig.getPosition + i) -> revLine
            }
        }
        .toMap
      if (rewritten.isEmpty) empty
      else new MillUserCodeRewrites(base, rewritten)
    }

  /** Length of the common prefix and suffix of two lines */
  private def similarity(a: String, b: String): Int = {
    val prefix = a.iterator.zip(b.iterator).takeWhile(t => t._1 == t._2).length
    val suffix = a.reverseIterator.zip(b.reverseIterator).takeWhile(t => t._1 == t._2).length
    math.min(prefix + suffix, math.min(a.length, b.length))
  }

  private val tokenRegex = raw"""`[^`\n]*`|"(?:[^"\\\n]|\\.)*"|[\p{L}\p{N}_$$]+|\s+|.""".r

  private def tokens(line: String): Vector[String] =
    tokenRegex.findAllIn(line).toVector

  /** The tokens that changed between a line and its rewritten version
    *
    * @return
    *   the column in `originalLine` of each change, and the original and new code
    */
  private def tokenChanges(
    originalLine: String,
    rewrittenLine: String
  ): Vector[(Int, String, String)] = {
    val origTokens = tokens(originalLine)
    val offsets    = origTokens.scanLeft(0)(_ + _.length)
    difflib.DiffUtils
      .diff(origTokens.asJava, tokens(rewrittenLine).asJava)
      .getDeltas
      .asScala
      .toVector
      .map { delta =>
        (
          offsets(delta.getOriginal.getPosition),
          delta.getOriginal.getLines.asScala.mkString,
          delta.getRevised.getLines.asScala.mkString
        )
      }
      .sortBy(_._1)
  }

  /** Rewrites an edited line, given how its original version was rewritten
    *
    * The tokens that changed between the original line and the rewritten one get changed in the
    * edited line too, if they can be found as is there, only once. Rewrites that can't be located
    * are left out.
    *
    * @return
    *   the rewritten line, and the changes made to it, if any
    */
  private def rewriteEditedLine(
    originalLine: String,
    rewrittenLine: String,
    editedLine: String
  ): Option[(String, Vector[(Int, String, String)])] = {
    val located = tokenChanges(originalLine, rewrittenLine)
      .filter(_._2.nonEmpty)
      .flatMap {
        case (_, from, to) =>
          val idx = editedLine.indexOf(from)
          if (idx >= 0 && editedLine.lastIndexOf(from) == idx) Seq((idx, from, to))
          else Nil
      }
      .sortBy(_._1)
    val nonOverlapping = located.foldLeft(Vector.empty[(Int, String, String)]) {
      case (acc, elem @ (idx, _, _)) =>
        acc.lastOption match {
          case Some((prevIdx, prevFrom, _)) if prevIdx + prevFrom.length > idx => acc
          case _                                                               => acc :+ elem
        }
    }
    Option.when(nonOverlapping.nonEmpty) {
      val newLine = nonOverlapping.foldRight(editedLine) {
        case ((idx, from, to), line) =>
          line.patch(idx, to, from.length)
      }
      (newLine, nonOverlapping)
    }
  }

  /** Keeps track of the rewrites of the user code of a generated source
    *
    * The rewrites are computed from the generated source and the original one on disk, if the
    * generated source was written after the original one (so that Mill generated it from the
    * current content of the original source). Otherwise, the last rewrites computed are kept.
    */
  final class Tracker(
    generatedPath: os.Path,
    originalPath: os.Path,
    topWrapper: String,
    bottomWrapper: String
  ) {
    private var last         = Option.empty[(Long, Long)]
    private var lastRewrites = empty

    def current(): MillUserCodeRewrites = synchronized {
      val mtimes =
        try Some((os.mtime(generatedPath), os.mtime(originalPath)))
        catch {
          case _: java.io.IOException => None
        }
      mtimes match {
        case Some(mtimes0 @ (generatedMtime, originalMtime))
            if generatedMtime >= originalMtime && !last.contains(mtimes0) =>
          val rewritesOpt =
            try
              userCode(os.read(generatedPath), topWrapper, bottomWrapper)
                .map(compute(os.read(originalPath), _))
            catch {
              case _: java.io.IOException => None
            }
          for (rewrites <- rewritesOpt) {
            if (!rewrites.isEmpty || lastRewrites.isEmpty)
              scribe.debug(s"Rewrites of $originalPath in $generatedPath: $rewrites")
            lastRewrites = rewrites
          }
          last = Some(mtimes0)
        case _ =>
      }
      lastRewrites
    }
  }
}
