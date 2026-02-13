// Based on https://github.com/scalameta/metals/blob/0ad0bc184f82dbd178d01f76913ea6bdfa98db14/metals/src/main/scala/scala/meta/internal/metals/PackageProvider.scala

package plasmon.ide

import scala.annotation.tailrec
import scala.meta.internal.mtags.SourcePath

import scala.meta._
import scala.meta.internal.pc.Identifier

import ch.epfl.scala.{bsp4j => b}
import org.eclipse.{lsp4j => l}
import org.eclipse.lsp4j.jsonrpc.messages.{Either => JEither}

import plasmon.PlasmonEnrichments._
import scala.jdk.CollectionConverters._
import plasmon.index.BspData

class PackageProvider(
  bspData: BspData,
  trees: Trees
) {
  import PackageProvider.*

  def workspaceEdit(
    path: os.Path,
    fileContent: String,
    documentVersion: Option[Int]
  )(implicit ctx: SourcePath.Context): Option[l.WorkspaceEdit] =
    packageStatement(path, fileContent).map { template =>
      workspaceEdit(
        path,
        template.fileContent,
        documentVersion = documentVersion
      )
    }

  private def packageStatement(
    path: os.Path,
    fileContent: String
  )(implicit
    ctx: SourcePath.Context
  ): Option[NewFileTemplate] = {

    def packageObjectStatement(packageParts: List[String]): Option[NewFileTemplate] =
      packageParts.lastOption.map { packageObjectName =>
        val packageDeclaration =
          if (packageParts.size > 1)
            s"package ${packageParts.dropRight(1).map(p => wrap(p)).mkString(".")}\n\n"
          else ""
        val indent         = "  "
        val backtickedName = wrap(packageObjectName)
        NewFileTemplate.create(
          s"""|${packageDeclaration}package object $backtickedName {
              |$indent@@
              |}
              |""".stripMargin
        )
      }

    for {
      packageParts <- Option
        .when(
          path.isScalaOrJava && !path.isJarFileSystem &&
          !path.isScalaScript && path.toIO.length() == 0 &&
          fileContent.isEmpty()
        )(deducePackageParts(path))
        .flatten
      if packageParts.size > 0
      newFileTemplate <-
        if (path.last == "package.scala")
          packageObjectStatement(packageParts)
        else {
          val packageName = packageParts.mkString(".")
          val text =
            if (path.isScala) s"package $packageName\n\n@@"
            else s"package $packageName;\n\n@@"
          Some(NewFileTemplate.create(text))
        }
    } yield newFileTemplate
  }

  private def findPackages(tree: Tree): PackagesStructure = {

    @tailrec
    def extractOuterPackage(
      tree: Tree,
      acc: PackagesStructure = PackagesStructure.empty
    ): PackagesStructure =
      tree match {
        case p @ Pkg(_, Nil)     => acc.addPackage(p)
        case p @ Pkg(_, st :: _) => extractOuterPackage(st, acc.addPackage(p))
        case p: Pkg.Object       => acc.withObject(p)
        case _                   => acc
      }

    tree match {
      case Source(List(pk: Pkg)) => extractOuterPackage(pk).reverse()
      case _                     => PackagesStructure.empty
    }
  }

  private def wrap(str: String): String = Identifier.backtickWrap(str)

  private def workspaceEdit(
    path: os.Path,
    replacement: String,
    documentVersion: Option[Int]
  ): l.WorkspaceEdit = {
    val textEdit =
      new l.TextEdit(new l.Range(new l.Position(0, 0), new l.Position(0, 0)), replacement)
    documentVersion match {
      case None =>
        val textEdits = List(textEdit).asJava
        val changes   = Map(path.toNIO.toUri.toASCIIString -> textEdits).asJava
        new l.WorkspaceEdit(changes)
      case Some(version) =>
        val id = new l.VersionedTextDocumentIdentifier(path.toNIO.toUri.toASCIIString, version)
        val textDocEdit =
          new l.TextDocumentEdit(id, List(l.jsonrpc.messages.Either.forLeft(textEdit)).asJava)
        val changes = List(
          JEither.forLeft[l.TextDocumentEdit, l.ResourceOperation](textDocEdit)
        ).asJava
        new l.WorkspaceEdit(changes)
    }
  }

  private def deducePackageParts(
    path: os.Path
  )(implicit ctx: SourcePath.Context): Option[List[String]] = {
    def basePackage = bspData
      .inverseSources(path)
      .flatMap(deduceBuildTargetBasePackage(_, _ != path))
      .getOrElse(Nil)
    deducePackagePartsFromPath(path).map(basePackage ++ _)
  }

  private def deducePackagePartsFromPath(
    path: os.Path
  ): Option[List[String]] =
    calcPathToSourceRoot(path).map(_.dropRight(1))

  /** Infer any implicit package prefix for a build target.
    *
    * This is to help with the case where packages in a build target all start with some common
    * package prefix that is not reflected in the directory structure.
    */
  private def deduceBuildTargetBasePackage(
    buildTargetId: b.BuildTargetIdentifier,
    pathShouldBeSampled: os.Path => Boolean
  )(implicit ctx: SourcePath.Context): Option[List[String]] = {

    /** If a sequence ends in a given suffix, return the sequence without that suffix
      *
      * @param original
      *   original sequence
      * @param maybeSuffix
      *   suffix to remove from that sequence
      */
    def stripSuffix[A](
      original: List[A],
      maybeSuffix: List[A]
    ): Option[List[A]] = {
      @tailrec
      def loop(
        originalRev: List[A],
        maybeSuffixRev: List[A]
      ): Option[List[A]] =
        maybeSuffixRev match {
          case Nil => Some(originalRev.reverse)
          case lastSuffix :: maybeRestSuffixRev =>
            originalRev match {
              case `lastSuffix` :: maybeRestOriginalRev =>
                loop(maybeRestOriginalRev, maybeRestSuffixRev)
              case _ => None
            }
        }

      loop(original.reverse, maybeSuffix.reverse)
    }

    // Pull out an arbitrary source file from the build target to infering the base package
    val sampleSourcePathAndTree = bspData
      .buildTargetSources(buildTargetId)
      .iterator
      .filter(pathShouldBeSampled)
      .flatMap(path => trees.get(buildTargetId.module, path).map(path -> _))
      .find(_ => true)

    for {
      (sampleSourcePath, tree) <- sampleSourcePathAndTree
      packagePartsFromTree = findPackages(tree).allParts()
      packagePartsFromPath <- deducePackagePartsFromPath(sampleSourcePath)
      packagePrefix        <- stripSuffix(packagePartsFromTree, packagePartsFromPath)
    } yield packagePrefix
  }

  private def calcPathToSourceRoot(
    path: os.Path
  ): Option[List[String]] =
    bspData.inverseSourceItem(path)
      .orElse(bspData.sourceRoot(path))
      .map(path.relativeTo(_))
      .map(_.segments.map(p => wrap(p.toString)).toList)
}

object PackageProvider {
  @tailrec
  private def extractNames(
    t: Term,
    acc: List[String] = List.empty
  ): List[String] =
    t match {
      case n: Term.Name   => n.value +: acc
      case s: Term.Select => extractNames(s.qual, s.name.value +: acc)
    }

  private case class PackageWithName(pkg: Pkg, name: List[String])

  private case class PackagesStructure(
    pkgs: List[PackageWithName],
    pkgObject: Option[Pkg.Object] = None
  ) {
    def withObject(pkgObject: Pkg.Object): PackagesStructure =
      PackagesStructure(pkgs, Some(pkgObject))
    def addPackage(pkg: Pkg): PackagesStructure =
      PackagesStructure(
        PackageWithName(pkg, extractNames(pkg.ref)) :: pkgs,
        pkgObject
      )
    def reverse(): PackagesStructure =
      PackagesStructure(pkgs.reverse, pkgObject)
    def allPackagesParts(): List[List[String]] =
      pkgs.map(_.name) ++ pkgObject.map(obj => List(obj.name.value)).toList
    def allParts(): List[String] = allPackagesParts().flatten
  }

  private object PackagesStructure {
    def empty: PackagesStructure = PackagesStructure(List(), None)
  }

  /** File template which allows specifying the cursor position using @@
    */
  private final case class NewFileTemplate(template: String) {
    import NewFileTemplate._

    lazy val fileContent: String = template.replace(cursorMarker, "")
  }

  private object NewFileTemplate {
    private val cursorMarker = "@@"

    /** @param template
      *   the file template string. Must contain exactly one cursor marker (@@)
      */
    def create(template: String): NewFileTemplate = {
      val cursorMarkersCount = cursorMarker.r.findAllMatchIn(template).length
      require(
        cursorMarkersCount == 1,
        s"File templates must contain exactly one cursor marker '$cursorMarker'. Found $cursorMarkersCount"
      )
      new NewFileTemplate(template)
    }
  }
}
