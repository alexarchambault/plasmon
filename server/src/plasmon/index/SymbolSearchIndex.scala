// Originally based on https://github.com/scalameta/metals/blob/0ad0bc184f82dbd178d01f76913ea6bdfa98db14/metals/src/main/scala/scala/meta/internal/metals/WorkspaceSymbolProvider.scala or earlier versions of that file

package plasmon.index

import scala.annotation.tailrec
import scala.collection.concurrent.TrieMap
import scala.collection.mutable

import scala.meta.internal.mtags.GlobalSymbolIndex
import scala.meta.internal.pc.InterruptException
import scala.meta.pc.SymbolSearch
import scala.meta.pc.SymbolSearchVisitor

import ch.epfl.scala.{bsp4j => b}
import org.eclipse.lsp4j.jsonrpc.CancelChecker
import org.eclipse.{lsp4j => l}
import scala.meta.internal.mtags.SourcePath
import scala.meta.internal.metals.ExcludedPackagesHandler
import scala.meta.internal.metals.ClasspathSearch
import scala.meta.internal.metals.WorkspaceSymbolQuery
import scala.meta.internal.metals.CompressedPackageIndex
import scala.meta.internal.metals.WorkspaceSymbolInformation
import scala.meta.internal.metals.Fuzzy
import scala.meta.internal.metals.PackageIndex
import plasmon.index.BspData
import scala.meta.internal.metals.StringBloomFilter
import plasmon.PlasmonEnrichments.*

// MOSTLY INTERNAL TO SymbolSearchImpl

final class SymbolSearchIndex(
  val workspace: os.Path,
  javaHome: os.Path,
  val bspData: BspData,
  val index: GlobalSymbolIndex,
  saveClassFileToDisk: Boolean,
  excludedPackageHandler: () => ExcludedPackagesHandler,
  bucketSize: Int = CompressedPackageIndex.DefaultBucketSize,
  classpathSearchIndexer: ClasspathSearch.Indexer =
    ClasspathSearch.Indexer.default
) {

  import SymbolSearchIndex._

  private val MaxWorkspaceMatchesForShortQuery = 100
  private val inWorkspace: TrieMap[os.Path, WorkspaceSymbolsIndex] =
    TrieMap.empty[os.Path, WorkspaceSymbolsIndex]

  private val packages: TrieMap[b.BuildTargetIdentifier, TrieMap[String, PackageNode]] =
    TrieMap.empty

  // symbols for extension methods
  private val inWorkspaceMethods: TrieMap[os.Path, Seq[WorkspaceSymbolInformation]] =
    TrieMap.empty[os.Path, Seq[WorkspaceSymbolInformation]]
  private var inDependencies: Map[GlobalSymbolIndex.Module, ClasspathSearch] =
    Map.empty

  def search(
    module: GlobalSymbolIndex.Module,
    query: String,
    fileInFocus: Option[os.Path]
  ): Seq[l.SymbolInformation] =
    search(module, query, () => (), fileInFocus)

  private def search(
    module: GlobalSymbolIndex.Module,
    query: String,
    token: CancelChecker,
    fileInFocus: Option[os.Path]
  ): Seq[l.SymbolInformation] =
    if (query.isEmpty) Nil
    else
      try
        searchUnsafe(module, query, token, fileInFocus)
      catch {
        case InterruptException() =>
          Nil
      }

  def search(
    query: WorkspaceSymbolQuery,
    visitor: SymbolSearchVisitor,
    module: GlobalSymbolIndex.Module
  )(implicit ctx: SourcePath.Context): (SymbolSearch.Result, Int) = {
    implicit val ctx0: scala.meta.pc.SourcePathContext = ctx.iface
    val workspaceCount                                 = workspaceSearch(query, visitor, module)
    val (res, inDepsCount) = inDependencies
      .getOrElse(module, ClasspathSearch.empty)
      .search(query, visitor)
    (res, workspaceCount + inDepsCount)
  }

  def searchMethods(
    query: String,
    visitor: SymbolSearchVisitor,
    module: GlobalSymbolIndex.Module
  ): SymbolSearch.Result = {
    workspaceMethodSearch(query, visitor, module)
    SymbolSearch.Result.COMPLETE
  }

  private def addWorkspacePackages(
    symbols: Seq[WorkspaceSymbolInformation],
    buildTargetIdentifier: b.BuildTargetIdentifier
  ): Unit = {
    val toAdd = symbols.map(_.symbol.split("/").dropRight(1)).distinct
    @tailrec
    def loop(
      current: Seq[String],
      packages: TrieMap[String, PackageNode]
    ): Unit = {
      current match {
        case Nil =>
        case head :: next =>
          val pkg = packages.getOrElseUpdate(head, PackageNode())
          pkg.count += 1
          loop(next, pkg.children)
      }
    }
    toAdd.foreach(pkg =>
      loop(
        pkg.toList,
        packages.getOrElseUpdate(buildTargetIdentifier, TrieMap.empty)
      )
    )
  }

  private def removeWorkspacePackages(
    symbols: Seq[WorkspaceSymbolInformation],
    buildTargetIdentifier: b.BuildTargetIdentifier
  ): Unit = {
    packages.get(buildTargetIdentifier) match {
      case Some(packages) =>
        val toRemove = symbols.map(_.symbol.split("/").dropRight(1)).distinct
        @tailrec
        def loop(
          current: Seq[String],
          packages: TrieMap[String, PackageNode]
        ): Unit = {
          current match {
            case Nil =>
            case head :: next =>
              packages.get(head).foreach(_.count -= 1)
              packages.synchronized {
                if (packages.get(head).exists(_.count <= 0))
                  packages.remove(head)
              }
              packages.get(head) match {
                case Some(pkg) => loop(next, pkg.children)
                case None      =>
              }
          }
        }
        toRemove.foreach(pkg => loop(pkg.toList, packages))
      case None =>
    }
  }

  def didChange(
    source: os.Path,
    symbols: Seq[WorkspaceSymbolInformation],
    methodSymbols: Seq[WorkspaceSymbolInformation]
  ): Unit = {
    val bloom = Fuzzy.bloomFilterSymbolStrings(symbols.map(_.symbol))
    val prev  = inWorkspace.get(source)
    inWorkspace(source) = WorkspaceSymbolsIndex(bloom, symbols)

    // methodSymbols will be searched when we type `qual.x@@`
    // where we want to match by prefix-match query.
    // Do not index by bloom filter for (extension) method symbols here because
    // - currently, we don't index each prefix of the name to bloom filter, so we can't find `incr` by `i`
    //   if we index it by bloom filter and lookup against it.
    // - symbol search will take O(N), if we don't use bloom filter, but
    //   inWorkspaceMethods stores extension methods only, and the number of symbols (N) are quite limited.
    //   Therefore, we can expect symbol lookup for extension methods could be fast enough without bloom-filter.
    if (methodSymbols.nonEmpty)
      inWorkspaceMethods(source) = methodSymbols

    bspData.inverseSources(source).foreach { buildTargetIdentifier =>
      prev.foreach(prev =>
        removeWorkspacePackages(prev.symbols, buildTargetIdentifier)
      )
      addWorkspacePackages(symbols, buildTargetIdentifier)
    }
  }

  def indexClasspathUnsafe(
    manualCp: Seq[(GlobalSymbolIndex.Module, Seq[os.Path])],
    extraCp: Seq[os.Path]
  ): Unit = {
    val cache = new mutable.HashMap[os.Path, PackageIndex]
    inDependencies = Map.empty
    for (jar <- extraCp if !cache.contains(jar))
      cache(jar) =
        PackageIndex.fromFile(jar.toNIO, excludedPackageHandler().isExcludedPackage)
    for {
      data     <- bspData.allWritableData
      targetId <- data.allBuildTargetIds
    } {
      val jars = data.targetJarClasspath(targetId).getOrElse(Nil)
      for (jar <- jars if !cache.contains(jar))
        cache(jar) = PackageIndex.fromFile(
          jar.toNIO,
          excludedPackageHandler().isExcludedPackage
        )
      inDependencies += targetId.module -> classpathSearchIndexer.index(
        (jars.toSeq ++ extraCp).map(cache(_)),
        excludedPackageHandler(),
        javaHome.toNIO,
        bucketSize
      )
    }
    for ((module, jars) <- manualCp) {
      for (jar <- jars if !cache.contains(jar))
        cache(jar) =
          PackageIndex.fromFile(jar.toNIO, excludedPackageHandler().isExcludedPackage)
      inDependencies += module -> classpathSearchIndexer.index(
        (jars ++ extraCp).map(cache(_)),
        excludedPackageHandler(),
        javaHome.toNIO,
        bucketSize
      )
    }
  }

  private def workspaceMethodSearch(
    query: String,
    visitor: SymbolSearchVisitor,
    module: GlobalSymbolIndex.Module
  ): Unit = {
    for {
      (path, symbols) <- module match {
        case _: GlobalSymbolIndex.Standalone =>
          Iterator.empty // inWorkspaceMethods.iterator // FIXME
        case t: GlobalSymbolIndex.BuildTarget =>
          val target = new b.BuildTargetIdentifier(t.targetId)
          for {
            source  <- bspData.buildTargetTransitiveSources(target)
            symbols <- inWorkspaceMethods.get(source)
          } yield (source, symbols)
      }
      isDeleted = !os.isFile(path)
      _         = if (isDeleted) inWorkspaceMethods.remove(path)
      if !isDeleted
      symbol <- symbols
      if Fuzzy.matches(query, symbol.symbol)
    }
      visitor.visitWorkspaceSymbol(
        path.toNIO,
        symbol.symbol,
        symbol.kind,
        symbol.range
      )
  }

  private def workspaceSearch(
    query: WorkspaceSymbolQuery,
    visitor: SymbolSearchVisitor,
    id: GlobalSymbolIndex.Module
  ): Int = {
    val symbols = for {
      (path, index) <- id match {
        case _: GlobalSymbolIndex.Standalone =>
          Iterator.empty // inWorkspace.iterator // FIXME
        case m: GlobalSymbolIndex.BuildTarget =>
          val target = new b.BuildTargetIdentifier(m.targetId)
          for {
            source <- bspData.buildTargetTransitiveSources(target)
            index  <- inWorkspace.get(source)
          } yield (source, index)
      }
      if query.matches(index.bloom)
      isDeleted = !os.isFile(path)
      _         = if (isDeleted) inWorkspace.remove(path)
      if !isDeleted
      symbol <- index.symbols
      if query.matches(symbol.symbol)
    } yield (path, symbol)

    @tailrec
    def loopSearch(count: Int): Int =
      if (!symbols.hasNext || (query.isShortQuery && count >= MaxWorkspaceMatchesForShortQuery))
        count
      else {
        val (path, symbol) = symbols.next()
        val added = visitor.visitWorkspaceSymbol(
          path.toNIO,
          symbol.symbol,
          symbol.kind,
          symbol.range
        )
        loopSearch(count + added)
      }

    loopSearch(0)
  }

  private def searchUnsafe(
    module: GlobalSymbolIndex.Module,
    textQuery: String,
    token: CancelChecker,
    fileInFocus: Option[os.Path]
  ): Seq[l.SymbolInformation] = {
    val query = WorkspaceSymbolQuery.fromTextQuery(textQuery)
    SourcePath.withContext { implicit ctx =>
      val visitor =
        new WorkspaceSearchVisitor(
          workspace,
          query,
          token,
          index,
          saveClassFileToDisk,
          SymbolDefinitionOrdering.fromOptPath(fileInFocus) // stop doing that
        )
      search(query, visitor, module)
      visitor.allResults()
    }
  }

  private class PreferredScalaVersionOrdering(preferredScalaVersions: Set[String])
      extends Ordering[SourcePath] {
    private def pathMatchesPreferred(path: SourcePath) =
      path match {
        case s: SourcePath.Standard =>
          bspData
            .possibleScalaVersions(os.Path(s.path))
            .exists(preferredScalaVersions(_))
        case _: SourcePath.ZipEntry =>
          false
      }

    private def pathLength(path: SourcePath) =
      path.uri.length()

    override def compare(x: SourcePath, y: SourcePath): Int = {
      val xVersionMatches = pathMatchesPreferred(x)
      val yVersionMatches = pathMatchesPreferred(y)

      if (xVersionMatches && !yVersionMatches) -1
      else if (yVersionMatches && !xVersionMatches) 1
      else pathLength(x) - pathLength(y)
    }
  }

  private object SymbolDefinitionOrdering {
    def fromOptPath(path: Option[os.Path]): Ordering[SourcePath] =
      path.toList.flatMap(bspData.possibleScalaVersions(_)) match {
        case Nil => DefaultSymbolDefinitionOrdering
        case preferredScalaVersions =>
          new PreferredScalaVersionOrdering(preferredScalaVersions.toSet)
      }
  }
}

object SymbolSearchIndex {
  private case class PackageNode(children: TrieMap[String, PackageNode] = TrieMap.empty) {
    @volatile var count: Int = 0
  }

  case class WorkspaceSymbolsIndex(
    bloom: StringBloomFilter,
    // NOTE(olafur) the original plan was to compress these in memory
    // to reduce memory usage but measurements in large repos like akka
    // show that it still uses <5mb in total.
    symbols: Seq[WorkspaceSymbolInformation]
  )
}
