// Originally based on https://github.com/scalameta/metals/blob/0ad0bc184f82dbd178d01f76913ea6bdfa98db14/metals/src/main/scala/scala/meta/internal/metals/TargetData.scala or earlier versions of that file

package plasmon.index

import java.util.concurrent.ConcurrentLinkedQueue

import scala.collection.concurrent.TrieMap
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.{Map => MMap}
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.util.Properties

import scala.meta.inputs.Input
import plasmon.bsp.PlasmonBuildServer

import ch.epfl.scala.{bsp4j => b}
import org.eclipse.{lsp4j => l}
import plasmon.ide.{JavaTarget, ScalaTarget, JvmTarget}
import scala.meta.internal.metals.PackageIndex

import plasmon.PlasmonEnrichments._
import scala.jdk.CollectionConverters._
import plasmon.ide.AdjustLspData

import java.lang.{Boolean => JBoolean}
import java.util.{Collections, HashSet => JHashSet, Set => JSet}
import java.util.concurrent.ConcurrentHashMap
import plasmon.bsp.PlasmonBuildClientImpl

final class TargetData {

  import TargetData.*

  val sourceItemsToBuildTarget
    : MMap[os.Path, ConcurrentLinkedQueue[b.BuildTargetIdentifier]] =
    TrieMap.empty[os.Path, ConcurrentLinkedQueue[b.BuildTargetIdentifier]]
  val buildTargetInfo: MMap[b.BuildTargetIdentifier, b.BuildTarget] =
    TrieMap.empty[b.BuildTargetIdentifier, b.BuildTarget]
  val javaTargetInfo: MMap[b.BuildTargetIdentifier, JavaTarget] =
    TrieMap.empty[b.BuildTargetIdentifier, JavaTarget]
  val scalaTargetInfo: MMap[b.BuildTargetIdentifier, ScalaTarget] =
    TrieMap.empty[b.BuildTargetIdentifier, ScalaTarget]
  val dependencySourcesInfo
    : MMap[b.BuildTargetIdentifier, ConcurrentLinkedQueue[b.DependencySourcesItem]] =
    TrieMap.empty
  val inverseDependencies
    : MMap[b.BuildTargetIdentifier, ListBuffer[b.BuildTargetIdentifier]] =
    TrieMap.empty[b.BuildTargetIdentifier, ListBuffer[b.BuildTargetIdentifier]]
  val buildTargetSources: MMap[b.BuildTargetIdentifier, JSet[os.Path]] =
    TrieMap.empty[b.BuildTargetIdentifier, JSet[os.Path]]
  val buildTargetClasspath: MMap[b.BuildTargetIdentifier, List[String]] =
    TrieMap.empty[b.BuildTargetIdentifier, List[String]]
  val buildTargetDependencyModules
    : MMap[b.BuildTargetIdentifier, List[b.MavenDependencyModule]] =
    TrieMap.empty[b.BuildTargetIdentifier, List[b.MavenDependencyModule]]
  val inverseDependencySources: MMap[os.Path, Set[b.BuildTargetIdentifier]] =
    TrieMap.empty[os.Path, Set[b.BuildTargetIdentifier]]
  val buildTargetGeneratedDirs: MMap[os.Path, Unit] =
    TrieMap.empty[os.Path, Unit]
  val buildTargetGeneratedFiles: MMap[os.Path, Unit] =
    TrieMap.empty[os.Path, Unit]
  val sourceJarNameToJarFile: MMap[String, os.Path] =
    TrieMap.empty[String, os.Path]
  val isSourceRoot: JSet[os.Path] =
    concurrentHashSet[os.Path]()
  // if workspace contains symlinks, original source items are kept here and source items dealiased
  val originalSourceItems: JSet[os.Path] =
    concurrentHashSet[os.Path]()
  val sourceItemFiles: JSet[os.Path] =
    concurrentHashSet[os.Path]()

  val targetToWorkspace: MMap[b.BuildTargetIdentifier, os.Path] =
    new mutable.HashMap[b.BuildTargetIdentifier, os.Path]

  var buildServerOpt               = Option.empty[PlasmonBuildServer]
  var buildClientOpt               = Option.empty[PlasmonBuildClientImpl]
  var workspaceBuildTargetsRespOpt = Option.empty[TargetData.WorkspaceBuildTargets]

  private val sourceBuildTargetsCache =
    new ConcurrentHashMap[os.Path, Option[
      Iterable[b.BuildTargetIdentifier]
    ]]

  val actualSources: MMap[os.Path, MappedSource] =
    TrieMap.empty[os.Path, MappedSource]

  def sourceBuildTargets(
    sourceItem: os.Path
  ): Option[Iterable[b.BuildTargetIdentifier]] = {
    val valueOrNull = sourceBuildTargetsCache.get(sourceItem)
    if (valueOrNull == null) {
      val value = sourceItemsToBuildTarget.collectFirst {
        case (source, buildTargets)
            if sourceItem.toNIO.getFileSystem == source.toNIO.getFileSystem &&
            sourceItem.startsWith(source) =>
          buildTargets.asScala
      }
      val prevOrNull = sourceBuildTargetsCache.putIfAbsent(sourceItem, value)
      if (prevOrNull == null) value
      else prevOrNull
    }
    else valueOrNull
  }

  def allTargetRoots: Iterator[os.Path] = {
    val scalaTargetRoots = scalaTargetInfo.map(_._2.targetroot)
    val javaTargetRoots  = javaTargetInfo.flatMap(_._2.targetroot)
    val allTargetRoots   = scalaTargetRoots.toSet ++ javaTargetRoots.toSet
    allTargetRoots.iterator
  }
  def all: Iterator[b.BuildTarget] =
    buildTargetInfo.values.iterator

  def allBuildTargetIds: Seq[b.BuildTargetIdentifier] =
    buildTargetInfo.keys.toSeq
  def allScala: Iterator[ScalaTarget] =
    scalaTargetInfo.values.iterator
  def allJava: Iterator[JavaTarget] =
    javaTargetInfo.values.iterator
  def scalaTarget(id: b.BuildTargetIdentifier): Option[ScalaTarget] =
    scalaTargetInfo.get(id)
  def javaTarget(id: b.BuildTargetIdentifier): Option[JavaTarget] =
    javaTargetInfo.get(id)
  def jvmTarget(id: b.BuildTargetIdentifier): Option[JvmTarget] =
    scalaTarget(id).orElse(javaTarget(id))
  def jvmTargets(id: b.BuildTargetIdentifier): List[JvmTarget] =
    List(scalaTarget(id), javaTarget(id)).flatten

  def targetRoots(buildTarget: b.BuildTargetIdentifier): List[os.Path] = {
    val javaRoot  = javaTargetRoot(buildTarget).toList
    val scalaRoot = scalaTargetRoot(buildTarget).toList
    (javaRoot ++ scalaRoot).distinct
  }

  def javaTargetRoot(buildTarget: b.BuildTargetIdentifier): Option[os.Path] =
    javaTarget(buildTarget).flatMap(_.targetroot)

  def scalaTargetRoot(
    buildTarget: b.BuildTargetIdentifier
  ): Option[os.Path] =
    scalaTarget(buildTarget).map(_.targetroot)

  def info(id: b.BuildTargetIdentifier): Option[b.BuildTarget] =
    buildTargetInfo.get(id)

  /** Get jars for a specific build target.
    *
    * We first try to use buildTargetDependencyModules request since it should be low cost for build
    * tools like Bazel.
    *
    * We fall back to reading from classpath only if the classpath is read eagerly.
    *
    * @param id
    *   id of the queried target
    * @return
    *   depenendency jar list if available
    */
  def targetJarClasspath(
    id: b.BuildTargetIdentifier
  ): Option[List[os.Path]] = {
    val fromDepModules =
      for {
        module   <- buildTargetDependencyModules.getOrElse(id, Nil)
        artifact <- module.getArtifacts.asScala
        path <- artifact match {
          case artifact: b.MavenDependencyModuleArtifact if artifact.getClassifier == null =>
            Some(artifact.getUri.osPathFromUri)
          case _ => None
        }
      } yield path

    if (fromDepModules.isEmpty)
      jvmTargets(id).flatMap(_.jarClasspath).headOption
    else Some(fromDepModules)
  }

  def targetClasspath(
    id: b.BuildTargetIdentifier
  )(implicit ec: ExecutionContext): Option[Future[List[String]]] =
    buildServerOpt.zip(jvmTarget(id)).map {
      case (bspConnection, jvmTarget) =>
        val classpath =
          jvmTarget.classpath.orElse(buildTargetClasspath.get(id)) match {
            case None =>
              bspConnection
                .buildTargetJvmCompileClasspath(
                  new b.JvmCompileClasspathParams(List(id).asJava)
                  // cancelPromise, // FIXME
                )
                .asScala
                .map { classpathResult =>
                  val classpath = classpathResult
                    .getItems
                    .asScala
                    .map(_.getClasspath.asScala)
                    .flatten
                    .toList
                  buildTargetClasspath.put(id, classpath)
                  classpath
                }
            case Some(classpath) => Future.successful(classpath)
          }

        classpath.map { classes =>
          val outputClasses = jvmTarget.classDirectory.toNIO.toUri.toASCIIString
          if (classes.contains(outputClasses)) classes
          else outputClasses :: classes
        }

    }

  def findConnectedArtifact(
    jar: os.Path,
    targetId: Option[b.BuildTargetIdentifier],
    classifier: String = "sources"
  ): Option[os.Path] = {
    val jarUri = jar.toNIO.toUri.toASCIIString
    def depModules: Iterator[b.MavenDependencyModule] = targetId match {
      case None     => buildTargetDependencyModules.values.flatten.iterator
      case Some(id) => buildTargetDependencyModules.get(id).iterator.flatten
    }

    /** For windows file:///C:/Users/runneradmin/AppData/Local/Coursier/Cache and
      * file:///C:/Users/runneradmin/AppData/Local/Coursier/cache is equivalent
      */
    def isUriEqual(uri: String, otherUri: String) =
      Properties.isWin && uri.toLowerCase() == otherUri
        .toLowerCase() || uri == otherUri
    val allFound = for {
      module <- depModules
      artifacts = module.getArtifacts.asScala
      if artifacts.exists(artifact => isUriEqual(artifact.getUri, jarUri))
      foundJar <- artifacts.find(_.getClassifier == classifier)
    } yield foundJar.getUri.osPathFromUri
    allFound.find(os.exists)
  }

  def targetClassDirectories(id: b.BuildTargetIdentifier): List[String] = {
    val scalacData =
      scalaTarget(id).map(_.scalac.getClassDirectory).filter(_.nonEmpty).toList
    val javacData =
      javaTarget(id).map(_.javac.getClassDirectory).filter(_.nonEmpty).toList
    (scalacData ++ javacData).distinct
  }

  def allWorkspaceJars: Iterator[os.Path] = {
    val isVisited = new JHashSet[os.Path]()

    Iterator(
      for {
        targetId         <- allBuildTargetIds
        classpathEntries <- targetJarClasspath(targetId).toList
        classpathEntry   <- classpathEntries
        if isVisited.add(classpathEntry)
      } yield classpathEntry,
      PackageIndex.bootClasspath.map(os.Path(_)).iterator
    ).flatten
  }

  def addSourceItem(
    sourceItem: os.Path,
    buildTarget: b.BuildTargetIdentifier
  ): Unit = {
    val dealiased = sourceItem.dealias
    if (dealiased != sourceItem)
      originalSourceItems.add(sourceItem)

    val queue = sourceItemsToBuildTarget.getOrElseUpdate(
      dealiased,
      new ConcurrentLinkedQueue()
    )
    queue.add(buildTarget)
    sourceBuildTargetsCache.clear()
  }

  def addSourceItem(
    sourceItem: b.SourceItem,
    buildTarget: b.BuildTargetIdentifier
  ): Unit = {
    val sourceItemPath = sourceItem.getUri.osPathFromUri

    sourceItem.getKind match {
      case b.SourceItemKind.DIRECTORY =>
        if (sourceItem.getGenerated)
          buildTargetGeneratedDirs(sourceItemPath) = ()
      case b.SourceItemKind.FILE =>
        if (sourceItem.getGenerated)
          buildTargetGeneratedFiles(sourceItemPath) = ()
        sourceItemFiles.add(sourceItemPath)
    }
    addSourceItem(sourceItemPath, buildTarget)
  }

  def linkSourceFile(id: b.BuildTargetIdentifier, source: os.Path): Unit = {
    val set = buildTargetSources.getOrElseUpdate(id, concurrentHashSet())
    set.add(source)
  }

  def reset(): Unit = {
    sourceItemsToBuildTarget.values.foreach(_.clear())
    sourceItemsToBuildTarget.clear()
    sourceBuildTargetsCache.clear()
    buildTargetInfo.clear()
    javaTargetInfo.clear()
    scalaTargetInfo.clear()
    dependencySourcesInfo.clear()
    inverseDependencies.clear()
    buildTargetSources.clear()
    buildTargetGeneratedDirs.clear()
    buildTargetGeneratedFiles.clear()
    inverseDependencySources.clear()
    sourceJarNameToJarFile.clear()
    isSourceRoot.clear()
  }

  def addWorkspaceBuildTargets(targets: Seq[b.BuildTarget]): Unit =
    for (target <- targets) {
      buildTargetInfo(target.getId) = target
      for (dependency <- target.getDependencies.asScala) {
        val buf =
          inverseDependencies.getOrElseUpdate(dependency, ListBuffer.empty)
        buf += target.getId
      }
    }

  def isSourceFile(source: os.Path): Boolean =
    sourceItemFiles.contains(source)

  def checkIfGeneratedSource(source: os.Path): Boolean =
    buildTargetGeneratedFiles.contains(source) ||
    buildTargetGeneratedDirs.keys.exists(source.startsWith)

  def checkIfGeneratedDir(path: os.Path): Boolean =
    buildTargetGeneratedDirs.contains(path)

  def addScalacOptions(result: b.ScalacOptionsResult): Unit =
    for {
      scalac           <- result.getItems.asScala
      info0            <- info(scalac.getTarget)
      scalaBuildTarget <- info0.asScalaBuildTarget
    } {
      for (sourceroot <- scalac.sourceroot(scalaBuildTarget.getScalaVersion))
        isSourceRoot.add(os.Path(sourceroot))
      val sbtTarget   = info0.asSbtBuildTarget
      val autoImports = sbtTarget.map(_.getAutoImports.asScala.toSeq)
      scalaTargetInfo(scalac.getTarget) = ScalaTarget(
        info0,
        scalaBuildTarget,
        scalac,
        autoImports,
        sbtTarget.map(_.getSbtVersion)
      )
    }

  def addJavacOptions(result: b.JavacOptionsResult): Unit =
    for (javac <- result.getItems.asScala) {
      for (sourceroot <- javac.sourceroot)
        isSourceRoot.add(os.Path(sourceroot))
      for (info0 <- info(javac.getTarget))
        javaTargetInfo(javac.getTarget) = JavaTarget(info0, javac)
    }

  def addDependencySourceItem(item: b.DependencySourcesItem): Unit = {
    val queue = dependencySourcesInfo.getOrElseUpdate(
      item.getTarget,
      new ConcurrentLinkedQueue
    )
    queue.add(item)
  }

  def addDependencySource(
    sourcesJar: os.Path,
    target: b.BuildTargetIdentifier
  ): Unit = {
    sourceJarNameToJarFile(sourcesJar.last) = sourcesJar
    val acc = inverseDependencySources.getOrElse(sourcesJar, Set.empty)
    inverseDependencySources(sourcesJar) = acc + target
  }

  def addMappedSource(
    path: os.Path,
    mapped: MappedSource
  ): Unit =
    actualSources(path) = mapped

  def resetConnections(
    idToConn: List[(b.BuildTargetIdentifier, os.Path)],
    buildServer: PlasmonBuildServer,
    buildClient: PlasmonBuildClientImpl,
    workspaceBuildTargetsResp: b.WorkspaceBuildTargetsResult
  ): Unit = {
    buildServerOpt = Some(buildServer)
    buildClientOpt = Some(buildClient)
    workspaceBuildTargetsRespOpt = Some(TargetData.WorkspaceBuildTargets(workspaceBuildTargetsResp))
    targetToWorkspace.clear()
    for ((id, workspace) <- idToConn)
      targetToWorkspace.put(id, workspace)
  }

  def onCreate(source: os.Path): Unit =
    for {
      buildTargets <- sourceBuildTargets(source)
      buildTarget  <- buildTargets
    }
      linkSourceFile(buildTarget, source)
}

object TargetData {

  trait MappedSource {
    def path: os.Path
    def lineForServer(line: Int): Option[Int] = None
    def lineForClient(line: Int): Option[Int] = None
    def update(
      content: String
    ): (Input.VirtualFile, l.Position => l.Position, AdjustLspData)
  }

  private def concurrentHashSet[T](): JSet[T] =
    Collections.newSetFromMap(new ConcurrentHashMap[T, JBoolean])

  final case class WorkspaceBuildTargets(
    resp: b.WorkspaceBuildTargetsResult
  ) {
    lazy val baseDirectories = resp
      .getTargets
      .asScala
      .toSeq
      .flatMap(target => Option(target.getBaseDirectory).toSeq)
      .map(_.osPathFromUri)
  }
}
