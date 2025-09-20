// Originally based on https://github.com/scalameta/metals/blob/0ad0bc184f82dbd178d01f76913ea6bdfa98db14/mtags/src/main/scala/scala/meta/internal/mtags/SemanticdbClasspath.scala or an earlier version of that file

package plasmon.semdb

import java.nio.charset.Charset
import java.nio.charset.StandardCharsets
import java.nio.file.Path

import scala.meta.AbsolutePath
import scala.meta.internal.mtags.ScalametaCommonEnrichments._
import scala.meta.io.Classpath
import scala.meta.io.RelativePath
import scala.meta.internal.mtags.Md5Fingerprints
import scala.meta.internal.mtags.OpenClassLoader
import scala.meta.internal.mtags.SourcePath
import scala.meta.internal.mtags.GlobalSymbolIndex
import plasmon.semdb.TextDocumentLookup
import scala.annotation.tailrec

final case class SemanticdbClasspath(
  sourceroot: os.Path,
  classpath: Classpath = Classpath(Nil),
  semanticdbTargets: List[Path] = Nil,
  charset: Charset = StandardCharsets.UTF_8,
  fingerprints: Md5Fingerprints = Md5Fingerprints.empty
) extends Semanticdbs {
  val loader = new OpenClassLoader
  loader.addClasspath(classpath.entries.map(_.toNIO))
  loader.addClasspath(semanticdbTargets)

  def textDocument(
    scalaOrJavaPath: SourcePath,
    module: GlobalSymbolIndex.Module
  ): Option[TextDocumentLookup] =
    Semanticdbs.loadTextDocument(
      scalaOrJavaPath,
      sourceroot,
      optScalaVersion = None,
      charset,
      fingerprints,
      path =>
        // loader
        //   .resolve(path.toNIO)
        //   .map(AbsolutePath(_))
        //   .map(FoundSemanticDbPath(_, None))
        ???
    ).toOption
}

object SemanticdbClasspath {

  def semanticdbRoot(path: os.Path): Option[os.Path] = {
    val end = os.sub / "META-INF/semanticdb"
    @tailrec def root(path0: os.Path): Option[os.Path] =
      if (path0.endsWith(end)) Some(path0)
      else if (path0.segmentCount > 0) root(path0 / os.up)
      else None
    root(path)
  }

  def toScala(
    workspace: os.Path,
    semanticdb: os.Path
  ): Option[os.Path] =
    semanticdbRoot(semanticdb).map { root =>
      val semdbSubPath  = semanticdb.relativeTo(root).asSubPath
      val sourceSubPath = semdbSubPath / os.up / semdbSubPath.last.stripSuffix(".semanticdb")
      workspace / sourceSubPath
    }

  def fromScalaOrJava(path: os.SubPath): os.SubPath = {
    require(path.last.isScalaOrJavaFilename, path.toString)
    val semanticdbSibling = path / os.up / (path.last + ".semanticdb")
    os.sub / "META-INF/semanticdb" / semanticdbSibling
  }
}
