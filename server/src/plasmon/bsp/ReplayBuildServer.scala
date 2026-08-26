package plasmon.bsp

import ch.epfl.scala.bsp4j as b
import com.google.gson.{GsonBuilder, JsonElement}

import java.util.concurrent.CompletableFuture

import scala.build.bsp.{WrappedSourcesParams, WrappedSourcesResult}
import scala.annotation.nowarn
import scala.jdk.CollectionConverters.*
import scala.reflect.ClassTag

/** Serves BSP responses recorded from a real build tool, with no build tool behind it.
  *
  * Only the requests the indexer needs to bring the presentation compiler up are answered from
  * recordings; the rest return empty results, since nothing drives a build here. Paths are restored
  * from their recorded placeholders on the way out, so a recording made on one machine (or OS)
  * works on another - see [[BspDataPortability]].
  */
final class ReplayBuildServer(
  dataDir: os.Path,
  roots: BspDataPortability.Roots,
  compiler: ReplayCompiler
) extends PlasmonBuildServer {

  private val gson = new GsonBuilder().create()

  private def read[T: ClassTag](name: String): Option[T] = {
    val f = dataDir / s"$name.json"
    if (os.exists(f)) {
      val elem =
        BspDataPortability.mapStrings(gson.fromJson(os.read(f), classOf[JsonElement]))(
          roots.denormalize
        )
      Some(gson.fromJson(elem, implicitly[ClassTag[T]].runtimeClass.asInstanceOf[Class[T]]))
    }
    else
      None
  }

  private def readOrFail[T: ClassTag](name: String): T =
    read[T](name).getOrElse {
      sys.error(
        s"No recorded BSP response '$name' under $dataDir. " +
          "The recording is incomplete - re-record it with PLASMON_RECORD_BSP_DATA=true."
      )
    }

  private def completed[T](value: T): CompletableFuture[T] =
    CompletableFuture.completedFuture(value)

  lazy val initializeBuildResult: b.InitializeBuildResult =
    readOrFail[b.InitializeBuildResult]("initializeBuildResult")

  /** Target ids this recording covers, in the order they were recorded. */
  lazy val roots0: Seq[b.BuildTargetIdentifier] =
    read[Array[b.BuildTargetIdentifier]]("roots").toSeq.flatten

  def buildInitialize(params: b.InitializeBuildParams)
    : CompletableFuture[b.InitializeBuildResult] =
    completed(initializeBuildResult)

  def workspaceBuildTargets: CompletableFuture[b.WorkspaceBuildTargetsResult] =
    completed(readOrFail[b.WorkspaceBuildTargetsResult]("workspaceBuildTargets"))

  // The indexer narrows the target list down before asking for these, so a recording covering more
  // targets than were asked for gets filtered rather than handed back wholesale.
  private def filtered[I, R](
    items: Seq[I],
    requested: Seq[b.BuildTargetIdentifier],
    targetOf: I => b.BuildTargetIdentifier
  ): Seq[I] = {
    val keep = requested.toSet
    items.filter(item => keep.contains(targetOf(item)))
  }

  def buildTargetScalacOptions(params: b.ScalacOptionsParams)
    : CompletableFuture[b.ScalacOptionsResult] = {
    val recorded = readOrFail[b.ScalacOptionsResult]("buildTargetScalacOptions")
    completed(new b.ScalacOptionsResult(
      filtered(recorded.getItems.asScala.toSeq, params.getTargets.asScala.toSeq, _.getTarget).asJava
    ))
  }

  def buildTargetJavacOptions(params: b.JavacOptionsParams)
    : CompletableFuture[b.JavacOptionsResult] = {
    val recorded = read[b.JavacOptionsResult]("buildTargetJavacOptions")
      .getOrElse(new b.JavacOptionsResult(Nil.asJava))
    completed(new b.JavacOptionsResult(
      filtered(recorded.getItems.asScala.toSeq, params.getTargets.asScala.toSeq, _.getTarget).asJava
    ))
  }

  def buildTargetSources(params: b.SourcesParams): CompletableFuture[b.SourcesResult] = {
    val recorded = readOrFail[b.SourcesResult]("buildTargetSources")
    completed(new b.SourcesResult(
      filtered(recorded.getItems.asScala.toSeq, params.getTargets.asScala.toSeq, _.getTarget).asJava
    ))
  }

  def buildTargetDependencySources(params: b.DependencySourcesParams)
    : CompletableFuture[b.DependencySourcesResult] = {
    val recorded = readOrFail[b.DependencySourcesResult]("buildTargetDependencySources")
    completed(new b.DependencySourcesResult(
      filtered(recorded.getItems.asScala.toSeq, params.getTargets.asScala.toSeq, _.getTarget).asJava
    ))
  }

  def buildTargetWrappedSources(params: WrappedSourcesParams)
    : CompletableFuture[WrappedSourcesResult] = {
    val recorded = read[WrappedSourcesResult]("buildTargetWrappedSources")
      .getOrElse(new WrappedSourcesResult(Nil.asJava))
    completed(new WrappedSourcesResult(
      filtered(recorded.getItems.asScala.toSeq, params.getTargets.asScala.toSeq, _.getTarget).asJava
    ))
  }

  // The recording says where each target's classes belong but obviously can't carry them, so we
  // rebuild them from source here - see ReplayCompiler for why that beats storing them.
  def buildTargetCompile(params: b.CompileParams): CompletableFuture[b.CompileResult] =
    completed(new b.CompileResult(
      compiler.compile(
        params.getTargets.asScala.toSeq,
        allTargets,
        allScalacOptions,
        allJavacOptions,
        allSources
      )
    ))

  private lazy val allTargets =
    readOrFail[b.WorkspaceBuildTargetsResult]("workspaceBuildTargets").getTargets.asScala.toSeq
  private lazy val allScalacOptions =
    readOrFail[b.ScalacOptionsResult]("buildTargetScalacOptions").getItems.asScala.toSeq
  private lazy val allJavacOptions =
    read[b.JavacOptionsResult]("buildTargetJavacOptions").toSeq.flatMap(_.getItems.asScala)
  private lazy val allSources =
    readOrFail[b.SourcesResult]("buildTargetSources").getItems.asScala.toSeq

  def buildTargetCleanCache(params: b.CleanCacheParams): CompletableFuture[b.CleanCacheResult] = {
    compiler.reset()
    completed(new b.CleanCacheResult(true))
  }

  def buildTargetDependencyModules(params: b.DependencyModulesParams)
    : CompletableFuture[b.DependencyModulesResult] =
    completed(new b.DependencyModulesResult(Nil.asJava))
  def buildTargetInverseSources(params: b.InverseSourcesParams)
    : CompletableFuture[b.InverseSourcesResult] =
    completed(new b.InverseSourcesResult(Nil.asJava))
  def buildTargetOutputPaths(params: b.OutputPathsParams): CompletableFuture[b.OutputPathsResult] =
    completed(new b.OutputPathsResult(Nil.asJava))
  def buildTargetResources(params: b.ResourcesParams): CompletableFuture[b.ResourcesResult] =
    completed(new b.ResourcesResult(Nil.asJava))
  def buildTargetRun(params: b.RunParams): CompletableFuture[b.RunResult] =
    completed(new b.RunResult(b.StatusCode.ERROR))
  def buildTargetTest(params: b.TestParams): CompletableFuture[b.TestResult] =
    completed(new b.TestResult(b.StatusCode.ERROR))
  def debugSessionStart(params: b.DebugSessionParams): CompletableFuture[b.DebugSessionAddress] =
    CompletableFuture.failedFuture(new UnsupportedOperationException(
      "debugSessionStart is not supported by the replay build server"
    ))

  def buildTargetJvmRunEnvironment(params: b.JvmRunEnvironmentParams)
    : CompletableFuture[b.JvmRunEnvironmentResult] =
    completed(new b.JvmRunEnvironmentResult(Nil.asJava))
  def buildTargetJvmTestEnvironment(params: b.JvmTestEnvironmentParams)
    : CompletableFuture[b.JvmTestEnvironmentResult] =
    completed(new b.JvmTestEnvironmentResult(Nil.asJava))
  def buildTargetJvmCompileClasspath(params: b.JvmCompileClasspathParams)
    : CompletableFuture[b.JvmCompileClasspathResult] =
    completed(new b.JvmCompileClasspathResult(Nil.asJava))

  @nowarn
  def buildTargetScalaMainClasses(params: b.ScalaMainClassesParams)
    : CompletableFuture[b.ScalaMainClassesResult] =
    completed(new b.ScalaMainClassesResult(Nil.asJava))
  @nowarn
  def buildTargetScalaTestClasses(params: b.ScalaTestClassesParams)
    : CompletableFuture[b.ScalaTestClassesResult] =
    completed(new b.ScalaTestClassesResult(Nil.asJava))

  def buildShutdown(): CompletableFuture[Object] =
    completed(null)
  def onBuildExit(): Unit         = ()
  def onBuildInitialized(): Unit  = ()
  def workspaceReload(): CompletableFuture[Object] =
    completed(null)
  def onRunReadStdin(params: b.ReadParams): Unit = ()
}
