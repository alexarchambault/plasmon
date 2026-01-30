// Originally based on https://github.com/scalameta/metals/blob/0ad0bc184f82dbd178d01f76913ea6bdfa98db14/metals/src/main/scala/scala/meta/internal/metals/BuildTargets.scala#L38

package plasmon.index

import scala.collection.mutable
import scala.concurrent.ExecutionContext
import scala.concurrent.Future

import plasmon.bsp.PlasmonBuildServer

import ch.epfl.scala.{bsp4j => b}
import scala.meta.internal.mtags.SourcePath
import scala.meta.{Dialect, dialects}
import plasmon.ide.{Directories, JavaTarget, ScalaTarget}

import plasmon.PlasmonEnrichments._
import scala.jdk.CollectionConverters._
import plasmon.bsp.PlasmonBuildClientImpl
import scala.meta.internal.mtags.GlobalSymbolIndex
import plasmon.bsp.BspServers
import plasmon.bsp.BuildServerInfo
import plasmon.render.JsonCodecs.given
import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker

/** In-memory cache for looking up build server metadata.
  */
final class BspData(
  workspace: os.Path,
  bspServers: BspServers
) {

  def allTargetData: Seq[TargetData] =
    targetDataMap.values.toList
  private[plasmon] var targetDataMap = Map.empty[BuildServerInfo, TargetData]
  def clearTargetData: Unit = {
    val keyValues = bspServers.list.flatMap(_._2).map(_.info -> new TargetData)
    targetDataMap = keyValues.toMap
    clear(keyValues.map(_._2))
    // compilers.reset()
  }
  def targetData(info: BuildServerInfo): Option[TargetData] =
    targetDataMap.get(info)

  private[plasmon] var latestTargetIds = List.empty[b.BuildTargetIdentifier]
  def touchTarget(id: b.BuildTargetIdentifier): Unit =
    if (!latestTargetIds.headOption.contains(id))
      latestTargetIds = id :: latestTargetIds.filter(_ != id)

  private[plasmon] var data: BspData.DataSeq =
    BspData.DataSeq((new TargetData) :: Nil)
  def clear(targetData: Seq[TargetData]): Unit =
    data = BspData.DataSeq(targetData.toList)
  def allWritableData = data.list

  private def buildTargetsOrder: b.BuildTargetIdentifier => Int = {
    val latestTargetIds0 = latestTargetIds.toArray
    targetId =>
      val idx = latestTargetIds0.indexOf(targetId)
      if (idx >= 0) idx else latestTargetIds0.length
  }

  def sourceItems: Iterable[os.Path] =
    data.iterable.flatMap(_.sourceItemsToBuildTarget.keys)
  def mappedTo(path: os.Path): Option[TargetData.MappedSource] =
    data.fromOptions(_.actualSources.get(path))
  def mappedFrom(path: os.Path): Option[os.Path] =
    data.fromOptions(_.actualSources.collectFirst {
      case (source, mapped) if mapped.path == path => source
    })
  private def findMappedSource(
    mappedPath: os.Path
  ): Option[TargetData.MappedSource] =
    data
      .fromOptions(_.actualSources.collectFirst {
        case (_, mapped) if mapped.path == mappedPath => mapped
      })
  def mappedLineForServer(mappedPath: os.Path, line: Int): Option[Int] =
    findMappedSource(mappedPath).flatMap(_.lineForServer(line))
  def mappedLineForClient(mappedPath: os.Path, line: Int): Option[Int] =
    findMappedSource(mappedPath).flatMap(_.lineForClient(line))

  def allTargetRoots: Iterator[os.Path] =
    data.fromIterators(_.allTargetRoots)

  def allJava: Iterator[JavaTarget] =
    data.fromIterators(_.allJava)
  def allScala: Iterator[ScalaTarget] =
    data.fromIterators(_.allScala)

  def info(id: b.BuildTargetIdentifier): Option[b.BuildTarget] =
    data.fromOptions(_.info(id))

  private def targetData(id: b.BuildTargetIdentifier): Option[TargetData] =
    data.fromOptions(data0 => if (data0.info(id).isEmpty) None else Some(data0))

  def scalaTarget(id: b.BuildTargetIdentifier): Option[ScalaTarget] =
    data.fromOptions(_.scalaTarget(id))

  def javaTarget(id: b.BuildTargetIdentifier): Option[JavaTarget] =
    data.fromOptions(_.javaTarget(id))

  def targetJarClasspath(id: b.BuildTargetIdentifier): Option[List[os.Path]] =
    data.fromOptions(_.targetJarClasspath(id))

  def targetClasspath(
    id: b.BuildTargetIdentifier
  )(implicit executionContext: ExecutionContext): Option[Future[List[String]]] =
    data.fromOptions(_.targetClasspath(id))

  def onCreate(source: os.Path): Unit =
    for {
      buildTargetIds <- sourceBuildTargets(source)
      buildTargetId  <- buildTargetIds
      targetData     <- targetData(buildTargetId)
    }
      targetData.onCreate(source)

  def buildTargetSources(
    id: b.BuildTargetIdentifier
  ): Iterable[os.Path] =
    data
      .fromOptions(_.buildTargetSources.get(id))
      .map(_.asScala)
      .getOrElse(Nil)

  def buildTargetTransitiveSources(
    id: b.BuildTargetIdentifier
  ): Iterator[os.Path] =
    for {
      dependency <- buildTargetTransitiveDependencies(id).iterator
      sources    <- data.fromOptions(_.buildTargetSources.get(dependency)).iterator
      source     <- sources.asScala.iterator
    } yield source

  def buildTargetTransitiveDependencies(
    id: b.BuildTargetIdentifier
  ): Iterable[b.BuildTargetIdentifier] =
    buildTargetTransitiveDependencies(List(id))

  private def buildTargetTransitiveDependencies(
    ids: List[b.BuildTargetIdentifier]
  ): Iterable[b.BuildTargetIdentifier] = {
    val isVisited = mutable.Set.empty[b.BuildTargetIdentifier]
    val toVisit   = new java.util.ArrayDeque[b.BuildTargetIdentifier]
    ids.foreach(toVisit.add(_))
    while (!toVisit.isEmpty) {
      val next = toVisit.pop()
      if (!isVisited(next)) {
        isVisited.add(next)
        for {
          info       <- info(next).iterator
          dependency <- info.getDependencies.asScala.iterator
        }
          toVisit.add(dependency)
      }
    }
    isVisited
  }

  def javaTargetRoot(
    buildTarget: b.BuildTargetIdentifier
  ): Option[os.Path] =
    data.fromOptions(_.javaTargetRoot(buildTarget))

  def scalaTargetRoot(
    buildTarget: b.BuildTargetIdentifier
  ): Option[os.Path] =
    data.fromOptions(_.scalaTargetRoot(buildTarget))

  def workspaceDirectory(
    buildTarget: b.BuildTargetIdentifier
  ): Option[os.Path] =
    data.fromOptions(_.targetToWorkspace.get(buildTarget))

  /** Returns the first build target containing this source file.
    */
  def inverseSources(
    source: os.Path
  ): Option[b.BuildTargetIdentifier] = {
    val bspData = sourceBuildTargets(source)
    val orSbtBuildTarget =
      bspData.map(_.toSeq).getOrElse(sbtBuildScalaTarget(source).toSeq)
    if (orSbtBuildTarget.isEmpty)
      inferBuildTarget(SourcePath.Standard(source.toNIO))
    else
      Some(orSbtBuildTarget.maxBy(buildTargetsOrder))
  }

  def inverseSources0(source: os.Path)
    : Either[Seq[b.BuildTargetIdentifier], Seq[b.BuildTargetIdentifier]] = {
    val bspData = sourceBuildTargets0(source).toSeq ++ sbtBuildScalaTarget0(source)
    if (bspData.isEmpty)
      inferBuildTargets(SourcePath.Standard(source.toNIO)).toLeft(bspData)
    else
      Right(bspData)
  }

  /** Returns all build targets containing this source file.
    */
  private def inverseSourcesAll(
    source: os.Path
  ): List[b.BuildTargetIdentifier] = {
    val bspData = sourceBuildTargets(source)
    val orSbtBuildTarget =
      bspData.map(_.toList).getOrElse(sbtBuildScalaTarget(source).toList)
    if (orSbtBuildTarget.isEmpty)
      inferBuildTargets(SourcePath.Standard(source.toNIO)).getOrElse(Nil)
    else orSbtBuildTarget
  }

  def inverseSourcesAll(
    source: SourcePath
  ): List[b.BuildTargetIdentifier] =
    source match {
      case s: SourcePath.Standard =>
        inverseSourcesAll(os.Path(s.path))
      case z: SourcePath.ZipEntry =>
        inferBuildTargets(z).getOrElse(Nil)
    }

  def scalaVersion(source: os.Path): Option[String] =
    for {
      id     <- inverseSources(source)
      target <- scalaTarget(id)
    } yield target.scalaVersion

  def possibleScalaVersions(source: os.Path): List[String] =
    for {
      id     <- inverseSourcesAll(source)
      target <- scalaTarget(id).toList
    } yield target.scalaVersion

  /** Resolves sbt auto imports if a file belongs to a Sbt build target.
    */
  def sbtAutoImports(path: os.Path): Option[Seq[String]] =
    for {
      targetId <- inverseSources(path)
      target   <- scalaTarget(targetId)
      imports  <- target.autoImports
    } yield imports

  private def inferBuildTargets(
    source: SourcePath
  ): Option[List[b.BuildTargetIdentifier]] =
    source match {
      case z: SourcePath.ZipEntry =>
        Some(inverseDependencySource(os.Path(z.zipPath)).toList)
      case p: SourcePath.Standard =>
        val depsDir = workspace / Directories.dependencies
        if (os.Path(p.path).startsWith(depsDir)) {
          val names = os.Path(p.path).relativeTo(depsDir).asSubPath.segments
          names.headOption
            .flatMap(sourceJarFile(_))
            .map(inverseDependencySource(_).toList)
        }
        else
          None
    }

  def inferBuildTarget(source: SourcePath): Option[b.BuildTargetIdentifier] =
    inferBuildTargets(source).flatMap(_.maxByOption(buildTargetsOrder))

  // @deprecated("Jar and source jar might not always be in the same directory")
  // private def jarPath(source: os.Path): Option[os.Path] = {
  //   source.jarPath.map { sourceJarPath =>
  //     sourceJarPath.parent.resolve(
  //       source.filename.replace("-sources.jar", ".jar")
  //     )
  //   }
  // }

  /** Returns meta build target for `*.sbt` or `*.scala` files. It selects build target by directory
    * of its connection because `*.sbt` and `*.scala` aren't included in `sourceFiles` set
    */
  private def sbtBuildScalaTarget(
    file: os.Path
  ): Option[b.BuildTargetIdentifier] = {
    val targetMetaBuildDir =
      if (file.isSbt) file / os.up / "project" else file / os.up
    data
      .fromIterators(_.buildTargetInfo.valuesIterator)
      .find { target =>
        val isMetaBuild = target.isSbtBuild
        if (isMetaBuild)
          workspaceDirectory(target.getId)
            .map(_ == targetMetaBuildDir)
            .getOrElse(false)
        else
          false
      }
      .map(_.getId)
  }

  private def sbtBuildScalaTarget0(file: os.Path): Seq[b.BuildTargetIdentifier] = {
    val targetMetaBuildDir =
      (if (file.isSbt) file / os.up / "project" else file / os.up).toAbsPath
    data
      .fromIterators(_.buildTargetInfo.valuesIterator)
      .filter { target =>
        target.isSbtBuild &&
        workspaceDirectory(target.getId).contains(targetMetaBuildDir)
      }
      .map(_.getId)
      .toSeq
  }

  private case class InferredBuildTarget(
    jar: os.Path,
    symbol: String,
    id: b.BuildTargetIdentifier,
    sourceJar: Option[os.Path]
  )

  def sourceBuildTargets(
    sourceItem: os.Path
  ): Option[Iterable[b.BuildTargetIdentifier]] =
    data.fromOptions(_.sourceBuildTargets(sourceItem))

  private def sourceBuildTargets0(sourceItem: os.Path): Iterator[b.BuildTargetIdentifier] =
    data.fromIterators(_.sourceBuildTargets(sourceItem).getOrElse(Nil).iterator)

  def inverseSourceItem(source: os.Path): Option[os.Path] =
    sourceItems.find(item => source.toNIO.startsWith(item.toNIO))

  def originalInverseSourceItem(source: os.Path): Option[os.Path] =
    data
      .fromIterators(_.originalSourceItems.asScala.iterator)
      .find(item => source.toNIO.startsWith(item.dealias.toNIO))

  def allInverseDependencies(
    target: b.BuildTargetIdentifier
  ): collection.Set[b.BuildTargetIdentifier] =
    computeInverseDependencies(target).visited
  private def computeInverseDependencies(
    target: b.BuildTargetIdentifier
  ): BspData.InverseDependencies =
    BspData.inverseDependencies(
      List(target),
      id => data.fromOptions(_.inverseDependencies.get(id))
    )

  @deprecated(
    "This might return false positives since names of jars could repeat."
  )
  private def sourceJarFile(sourceJarName: String): Option[os.Path] =
    data.fromOptions(_.sourceJarNameToJarFile.get(sourceJarName))

  private def inverseDependencySource(
    sourceJar: os.Path
  ): collection.Set[b.BuildTargetIdentifier] =
    data
      .fromOptions(_.inverseDependencySources.get(sourceJar))
      .getOrElse(Set.empty)

  def sourceRoots: Iterable[os.Path] =
    data.iterable.flatMap(_.isSourceRoot.asScala)

  def isInsideSourceRoot(path: os.Path): Boolean =
    data.iterator.exists(_.isSourceRoot.contains(path)) &&
    data.fromIterators(_.isSourceRoot.asScala.iterator).exists { root =>
      path.toNIO.startsWith(root.toNIO)
    }

  def sourceRoot(path: os.Path): Option[os.Path] =
    data
      .fromIterators(_.isSourceRoot.asScala.iterator)
      .filter(root => path.toNIO.startsWith(root.toNIO))
      .find(_ => true)

  def isSourceFile(source: os.Path): Boolean =
    data.iterator.exists(_.isSourceFile(source))

  def checkIfGeneratedSource(source: os.Path): Boolean =
    data.iterator.exists(_.checkIfGeneratedSource(source))

  def buildServerOf(id: b.BuildTargetIdentifier): Option[PlasmonBuildServer] =
    data.fromOptions { data =>
      if (data.targetToWorkspace.contains(id)) data.buildServerOpt
      else None
    }

  def buildClientOf(id: b.BuildTargetIdentifier): Option[PlasmonBuildClientImpl] =
    data.fromOptions { data =>
      if (data.targetToWorkspace.contains(id)) data.buildClientOpt
      else None
    }

  private def dialectFromBuildTarget(targetId: b.BuildTargetIdentifier): Option[Dialect] =
    scalaTarget(targetId).map(_.dialect(pathIsSbt = false))

  def getDialect(
    extension: String,
    isMill: Boolean,
    targetId: b.BuildTargetIdentifier
  ): Option[Dialect] =
    if (isMill)
      dialectFromBuildTarget(targetId).map { dialect =>
        dialect
          .withAllowToplevelTerms(true)
          .withAllowToplevelStatements(true)
      }
    else
      extension match {
        case "scala" =>
          dialectFromBuildTarget(targetId)
        case "sc" =>
          dialectFromBuildTarget(targetId).map { dialect =>
            dialect
              .withAllowToplevelTerms(true)
              .withToplevelSeparator("")
          }
        case _ =>
          None
      }

  def buildClients(): Seq[PlasmonBuildClientImpl] =
    data.fromIterators(_.buildClientOpt.iterator).toSeq

  private def dialectFromBuildTarget(
    targetId: b.BuildTargetIdentifier,
    path: os.Path
  ): Option[Dialect] =
    scalaTarget(targetId).map(_.dialect(path))

  def getDialect(module: GlobalSymbolIndex.Module, path: os.Path): Option[Dialect] =
    getDialect0(
      module,
      path.ext,
      path.isMill,
      Some(path)
    )

  def getDialect(module: GlobalSymbolIndex.Module, path: SourcePath): Option[Dialect] =
    getDialect0(
      module,
      path.extension,
      path.isMill,
      path.filePath.map(os.Path(_))
    )

  private def getDialect0(
    module: GlobalSymbolIndex.Module,
    extension: String,
    isMill: Boolean,
    pathOpt: Option[os.Path]
  ): Option[Dialect] = {
    val targetId = module match {
      case t: GlobalSymbolIndex.BuildTarget => new b.BuildTargetIdentifier(t.targetId)
      case _: GlobalSymbolIndex.Standalone  => ???
    }
    Option(extension) match {
      case _ if isMill =>
        pathOpt
          .flatMap(dialectFromBuildTarget(targetId, _))
          .map { dialect =>
            dialect
              .withAllowToplevelTerms(true)
              .withAllowToplevelStatements(true)
          }
      case Some("scala") =>
        pathOpt
          .flatMap(dialectFromBuildTarget(targetId, _))
      case Some("sbt") => Some(dialects.Sbt)
      case Some("sc") =>
        pathOpt
          .flatMap(dialectFromBuildTarget(targetId, _))
          .map { dialect =>
            dialect
              .withAllowToplevelTerms(true)
              .withToplevelSeparator("")
          }
      case _ =>
        None
    }
  }

  def asJson: BspData.AsJson =
    BspData.AsJson(
      targetData = targetDataMap.toSeq.sortBy(_._1.toString).map {
        case (info, data) =>
          (info, data.asJson)
      },
      latestTargetIds = latestTargetIds
    )
}

object BspData {

  /** Given an acyclic graph and a root target, returns the leaf nodes that depend on the root target.
    *
    * For example, returns `[D, E, C]` given the following graph with root A: {{{
    *      A
    *    ^   ^
    *    |   |
    *    B   C
    *   ^ ^
    *   | |
    *   D E
    * }}}
    */
  private def inverseDependencies(
    root: List[b.BuildTargetIdentifier],
    inverseDeps: b.BuildTargetIdentifier => Option[
      collection.Seq[b.BuildTargetIdentifier]
    ]
  ): InverseDependencies = {
    val isVisited = mutable.Set.empty[b.BuildTargetIdentifier]
    val leaves    = mutable.Set.empty[b.BuildTargetIdentifier]
    def loop(toVisit: List[b.BuildTargetIdentifier]): Unit =
      toVisit match {
        case Nil => ()
        case head :: tail =>
          if (!isVisited(head)) {
            isVisited += head
            inverseDeps(head) match {
              case Some(next) =>
                loop(next.toList)
              case None =>
                // Only add leaves of the tree to the result to minimize the number
                // of targets that we compile. If `B` depends on `A`, it's faster
                // in Bloop to compile only `B` than `A+B`.
                leaves += head
            }
          }
          loop(tail)
      }
    loop(root)
    InverseDependencies(isVisited, leaves)
  }

  private case class InverseDependencies(
    visited: collection.Set[b.BuildTargetIdentifier],
    leaves: collection.Set[b.BuildTargetIdentifier]
  )

  private[plasmon] final case class DataSeq(list: List[TargetData]) {
    def iterator: Iterator[TargetData]             = list.iterator
    def writableDataIterator: Iterator[TargetData] = list.iterator
    def iterable: Iterable[TargetData]             = list.toSeq

    def fromIterators[T](f: TargetData => Iterator[T]): Iterator[T] =
      iterator.flatMap(f)
    def fromOptions[T](f: TargetData => Option[T]): Option[T] =
      fromIterators(f(_).iterator).find(_ => true)
  }

  final case class AsJson(
    targetData: Seq[(BuildServerInfo, TargetData.AsJson)],
    latestTargetIds: Seq[b.BuildTargetIdentifier]
  )

  given JsonValueCodec[AsJson] =
    JsonCodecMaker.make
}
