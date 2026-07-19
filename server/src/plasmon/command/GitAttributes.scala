package plasmon.command

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

import scala.jdk.CollectionConverters.*
import scala.util.Try
import scala.util.matching.Regex

/** Reads attributes of files from `.gitattributes` files, like `linguist-language`
  *
  * Looks at the `.gitattributes` files from the top of the Git repository containing `root` (or
  * `root` itself, if it's not in a repository) down to the directory of a file, then at
  * `info/attributes` in the Git directory. Like Git, the last matching line wins.
  *
  * Not supported: macro attributes (`[attr]…` lines), quoted patterns, global attribute files.
  */
private[command] final class GitAttributes(root: Path) {
  import GitAttributes.*

  private val top: Path =
    Iterator.iterate(root)(_.getParent)
      .takeWhile(_ != null)
      .find(dir => Files.exists(dir.resolve(".git")))
      .getOrElse(root)

  private val infoAttributes: Option[Path] = Try {
    val dotGit = top.resolve(".git")
    val gitDir =
      if (Files.isDirectory(dotGit)) dotGit
      else {
        // worktrees and submodules: `gitdir: …` file, info/ is in the common directory
        val content = Files.readString(dotGit, StandardCharsets.UTF_8).trim
        val dir     = top.resolve(content.stripPrefix("gitdir:").trim).normalize()
        val commonDir = dir.resolve("commondir")
        if (Files.isRegularFile(commonDir))
          dir.resolve(Files.readString(commonDir, StandardCharsets.UTF_8).trim).normalize()
        else dir
      }
    gitDir.resolve("info/attributes")
  }.toOption.filter(Files.isRegularFile(_))

  /** The rules applying to the files directly in `dir`, lowest priority first */
  def rules(dir: Path): Seq[Rule] =
    if (!dir.startsWith(top)) Nil
    else {
      val dirs = top.relativize(dir).iterator().asScala.toSeq
        .scanLeft(top)((parent, name) => parent.resolve(name))
      dirs.flatMap(d => parse(d.resolve(".gitattributes"), d)) ++
        infoAttributes.toSeq.flatMap(parse(_, top))
    }

  /** The value of `attribute` for `file`, if it's set to a value (`attribute=value`) */
  def value(rules: Seq[Rule], file: Path, attribute: String): Option[String] =
    rules.reverseIterator
      .filter(_.matches(file))
      .flatMap(_.attributes.reverseIterator.find(_._1 == attribute))
      .nextOption()
      .flatMap(_._2)

  private def parse(file: Path, base: Path): Seq[Rule] =
    if (!Files.isRegularFile(file)) Nil
    else
      Try(Files.readAllLines(file, StandardCharsets.UTF_8).asScala.toSeq).getOrElse(Nil)
        .map(_.trim)
        .filter(line => line.nonEmpty && !line.startsWith("#") && !line.startsWith("[attr]"))
        .flatMap { line =>
          line.split("\\s+").toList match {
            case pattern :: attrs if !pattern.endsWith("/") =>
              val attributes = attrs.map {
                case a if a.startsWith("-") || a.startsWith("!") => (a.drop(1), None)
                case a =>
                  a.split("=", 2) match {
                    case Array(k, v) => (k, Some(v))
                    case _           => (a, None)
                  }
              }
              Seq(Rule(base, pattern, attributes))
            case _ => Nil
          }
        }
}

private[command] object GitAttributes {

  final case class Rule(base: Path, pattern: String, attributes: Seq[(String, Option[String])]) {
    // Patterns without a slash match file names at any depth, others match paths relative to
    // the directory of the .gitattributes file
    private val anchored = pattern.stripSuffix("/").contains("/")
    private val regex: Regex =
      globRegex(if (anchored) pattern.stripPrefix("/") else pattern).r
    def matches(file: Path): Boolean =
      file.startsWith(base) && {
        val target =
          if (anchored) base.relativize(file).iterator().asScala.mkString("/")
          else file.getFileName.toString
        regex.matches(target)
      }
  }

  def globRegex(glob: String): String = {
    val b = new StringBuilder
    var i = 0
    while (i < glob.length) {
      val c = glob(i)
      if (glob.startsWith("**/", i)) { b ++= "(?:.*/)?"; i += 3 }
      else if (glob.startsWith("/**", i) && i + 3 == glob.length) { b ++= "/.*"; i += 3 }
      else if (glob.startsWith("**", i)) { b ++= ".*"; i += 2 }
      else if (c == '*') { b ++= "[^/]*"; i += 1 }
      else if (c == '?') { b ++= "[^/]"; i += 1 }
      else if (c == '\\' && i + 1 < glob.length) { b ++= Regex.quote(glob(i + 1).toString); i += 2 }
      else if (c == '[' && glob.indexOf(']', i + 2) > 0) {
        val end   = glob.indexOf(']', i + 2)
        val inner = glob.substring(i + 1, end)
        val negated = inner.startsWith("!") || inner.startsWith("^")
        val chars   = (if (negated) inner.drop(1) else inner).replace("\\", "\\\\").replace("[", "\\[")
        b ++= (if (negated) s"[^/$chars]" else s"[$chars]")
        i = end + 1
      }
      else { b ++= Regex.quote(c.toString); i += 1 }
    }
    b.result()
  }
}
