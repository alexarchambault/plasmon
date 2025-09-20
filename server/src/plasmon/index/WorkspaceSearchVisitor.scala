// Originally based on https://github.com/scalameta/metals/blob/0ad0bc184f82dbd178d01f76913ea6bdfa98db14/metals/src/main/scala/scala/meta/internal/metals/WorkspaceSearchVisitor.scala or earlier versions of that file

package plasmon.index

import java.util.{ArrayList, Comparator}

import scala.collection.mutable

import scala.meta.Dialect
import scala.meta.internal.mtags.GlobalSymbolIndex
import scala.meta.internal.mtags.Symbol
import scala.meta.internal.semanticdb.Scala.Descriptor
import scala.meta.internal.semanticdb.Scala.Symbols
import scala.meta.pc.SymbolSearchVisitor

import org.eclipse.lsp4j.jsonrpc.CancelChecker
import org.eclipse.{lsp4j => l}
import scala.meta.internal.mtags.SourcePath
import scala.meta.internal.metals.WorkspaceSymbolQuery
import plasmon.ide.Directories
import scala.meta.internal.metals.Classfile
import scala.meta.internal.metals.SemanticdbDefinition
import scala.meta.PlasmonHelpers
import plasmon.pc.NopReportContext
import scala.jdk.CollectionConverters.*
import plasmon.PlasmonEnrichments.*
import scala.meta.internal.mtags.ScalametaCommonEnrichments.XtensionWorkspaceSymbolQuery

/** A symbol search visitor for `workspace/symbol`.
  *
  *   - workspace symbols are converted directly to l.SymbolInformation
  *   - classpath symbols are converted into "goto definition" requests, which creates files on
  *     disk, and then into l.SymbolInformation.
  */
private class WorkspaceSearchVisitor(
  workspace: os.Path,
  query: WorkspaceSymbolQuery,
  token: CancelChecker,
  index: GlobalSymbolIndex,
  saveClassFileToDisk: Boolean,
  resultOrdering: Ordering[SourcePath] = DefaultSymbolDefinitionOrdering
)(implicit ctx: SourcePath.Context)
    extends SymbolSearchVisitor {
  private val fromWorkspace     = new ArrayList[l.SymbolInformation]
  private val fromClasspath     = new ArrayList[l.SymbolInformation]
  private val bufferedClasspath = new ArrayList[(String, String)]
  def allResults(): Seq[l.SymbolInformation] = {
    if (fromWorkspace.isEmpty)
      bufferedClasspath.forEach { case (pkg, name) =>
        expandClassfile(pkg, name)
      }

    fromWorkspace.sort(byNameLength)
    fromClasspath.sort(byNameLength)

    val result = new ArrayList[l.SymbolInformation]
    result.addAll(fromWorkspace)
    result.addAll(fromClasspath)

    // if (!bufferedClasspath.isEmpty && fromClasspath.isEmpty)
    //   Tell "Add ';' to search library dependencies" to users?

    result.asScala.toSeq
  }
  private val byNameLength = new Comparator[l.SymbolInformation] {
    def compare(x: l.SymbolInformation, y: l.SymbolInformation): Int =
      Integer.compare(x.getName.length(), y.getName.length())
  }

  private val isVisited: mutable.Set[SourcePath] =
    mutable.Set.empty[SourcePath]

  private def definition(
    pkg: String,
    filename: String,
    index: GlobalSymbolIndex
  ): Option[(SourcePath, Option[Dialect])] = {
    val nme    = Classfile.name(filename)
    val tpe    = Symbol(Symbols.Global(pkg, Descriptor.Type(nme)))
    val forTpe = index.findFileForToplevel(tpe)
    val defs = if (forTpe.isEmpty) {
      val term = Symbol(Symbols.Global(pkg, Descriptor.Term(nme)))
      index.findFileForToplevel(term)
    }
    else forTpe
    defs.sortBy(_._1)(resultOrdering).headOption
  }
  override def shouldVisitPackage(pkg: String): Boolean = true
  override def visitWorkspaceSymbol(
    path: java.nio.file.Path,
    symbol: String,
    kind: l.SymbolKind,
    range: l.Range
  ): Int = {
    val (desc, owner) = PlasmonHelpers.DescriptorParser(symbol)
    fromWorkspace.add(
      new l.SymbolInformation(
        desc.name.value,
        kind,
        new l.Location(path.toUri.toASCIIString, range),
        owner.replace('/', '.')
      )
    )
    1
  }
  override def visitClassfile(
    pkg: String,
    filename: String,
    ctx: scala.meta.pc.SourcePathContext
  ): Int =
    if (fromWorkspace.isEmpty || query.isClasspath)
      expandClassfile(pkg, filename)
    else {
      bufferedClasspath.add(pkg -> filename)
      1
    }
  override def isCancelled: Boolean = token.isCancelled
  private def expandClassfile(pkg: String, filename: String): Int = {
    var isHit = false
    for {
      (path, dialect) <- definition(pkg, filename, index)
      if !isVisited(path)
    } {
      isVisited += path
      val input = path.toInput
      SemanticdbDefinition.foreach(
        input,
        dialect,
        includeMembers = false
      ) { semanticDefn =>
        if (query.matches(semanticDefn.info)) {
          val adjustedPath =
            if (saveClassFileToDisk) path.toFileOnDisk(workspace)
            else path
          val uri = adjustedPath.uri
          fromClasspath.add(semanticDefn.toLsp(uri))
          isHit = true
        }
      }(NopReportContext)
    }
    if (isHit) 1 else 0
  }
}

object DefaultSymbolDefinitionOrdering extends Ordering[SourcePath] {

  override def compare(x: SourcePath, y: SourcePath): Int =
    x.uri.length() - y.uri.length()

}
