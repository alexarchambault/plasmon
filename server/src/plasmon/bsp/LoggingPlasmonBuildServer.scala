// Originally based on https://github.com/VirtusLab/scala-cli/blob/4f86e7996452602f7d8de39671772262e3e7a5a0/modules/build/src/main/scala/scala/build/bsp/LoggingBuildServer.scala or an earlier version of that file

package plasmon.bsp

import ch.epfl.scala.bsp4j as b
import plasmon.Logger
import plasmon.languageclient.PlasmonLanguageClient

import java.util.{Objects, UUID}
import java.util.concurrent.CompletableFuture

import scala.annotation.nowarn
import scala.build.bsp.{WrappedSourcesParams, WrappedSourcesResult}

final class LoggingPlasmonBuildServer(
  underlying: PlasmonBuildServer,
  logger: Logger,
  languageClient: PlasmonLanguageClient,
  buildToolId: String,
  buildToolName: String
) extends PlasmonBuildServer {
  import LoggingPlasmonBuildServer._

  private def request[T](reqName: String)(f: => CompletableFuture[T]): CompletableFuture[T] = {
    val reqId = UUID.randomUUID().toString
    val reqName0 =
      if (reqName.startsWith("buildTarget"))
        reqName.stripPrefix("buildTarget").capitalize
      else
        reqName
    languageClient.progress(
      PlasmonLanguageClient.ProgressDetails(
        buildToolId,
        buildToolName,
        reqId,
        reqName0,
        done = false
      )
    )
    def done(): Unit =
      languageClient.progress(
        PlasmonLanguageClient.ProgressDetails(
          buildToolId,
          buildToolName,
          reqId,
          reqName0,
          done = true
        )
      )
    val f0 =
      try f
      catch {
        case t: Throwable =>
          done()
          throw t
      }
    f0.whenComplete { (_, _) =>
      try done()
      catch {
        case t: Throwable =>
          scribe.warn("Error sending progress done notification", t)
      }
    }
    f0
  }

  def buildInitialize(params: b.InitializeBuildParams): CompletableFuture[b.InitializeBuildResult] =
    logger.logged("buildInitialize", params) {
      request("buildInitialize") {
        underlying.buildInitialize(params)
      }
    }
  def buildShutdown(): CompletableFuture[Object] =
    logger.logged("buildShutdown") {
      request("buildShutdown") {
        underlying.buildShutdown()
      }
    }
  def buildTargetCleanCache(params: b.CleanCacheParams): CompletableFuture[b.CleanCacheResult] =
    logger.logged("buildTargetCleanCache", params) {
      request("buildTargetCleanCache") {
        underlying.buildTargetCleanCache(params)
      }
    }
  def buildTargetCompile(params: b.CompileParams): CompletableFuture[b.CompileResult] =
    logger.logged("buildTargetCompile", params) {
      request("buildTargetCompile") {
        underlying.buildTargetCompile(params)
      }
    }
  def buildTargetDependencyModules(params: b.DependencyModulesParams)
    : CompletableFuture[b.DependencyModulesResult] =
    logger.logged("buildTargetDependencyModules", params) {
      request("buildTargetDependencyModules") {
        underlying.buildTargetDependencyModules(params)
      }
    }
  def buildTargetDependencySources(params: b.DependencySourcesParams)
    : CompletableFuture[b.DependencySourcesResult] =
    logger.logged("buildTargetDependencySources", params) {
      request("buildTargetDependencySources") {
        underlying.buildTargetDependencySources(params)
      }
    }
  def buildTargetInverseSources(params: b.InverseSourcesParams)
    : CompletableFuture[b.InverseSourcesResult] =
    logger.logged("buildTargetInverseSources", params) {
      request("buildTargetInverseSources") {
        underlying.buildTargetInverseSources(params)
      }
    }
  def buildTargetOutputPaths(params: b.OutputPathsParams): CompletableFuture[b.OutputPathsResult] =
    logger.logged("buildTargetInverseSources", params) {
      request("buildTargetInverseSources") {
        underlying.buildTargetOutputPaths(params)
      }
    }
  def buildTargetResources(params: b.ResourcesParams): CompletableFuture[b.ResourcesResult] =
    logger.logged("buildTargetResources", params) {
      request("buildTargetResources") {
        underlying.buildTargetResources(params)
      }
    }
  def buildTargetRun(params: b.RunParams): CompletableFuture[b.RunResult] =
    logger.logged("buildTargetRun", params) {
      request("buildTargetRun") {
        underlying.buildTargetRun(params)
      }
    }
  def buildTargetSources(params: b.SourcesParams): CompletableFuture[b.SourcesResult] =
    logger.logged("buildTargetSources", params) {
      request("buildTargetSources") {
        underlying.buildTargetSources(params)
      }
    }
  def buildTargetTest(params: b.TestParams): CompletableFuture[b.TestResult] =
    logger.logged("buildTargetTest", params) {
      request("buildTargetTest") {
        underlying.buildTargetTest(params)
      }
    }
  def debugSessionStart(params: b.DebugSessionParams): CompletableFuture[b.DebugSessionAddress] =
    logger.logged("debugSessionStart", params) {
      request("debugSessionStart") {
        underlying.debugSessionStart(params)
      }
    }
  def onBuildExit(): Unit =
    underlying.onBuildExit()
  def onBuildInitialized(): Unit =
    underlying.onBuildInitialized()
  def workspaceBuildTargets: CompletableFuture[b.WorkspaceBuildTargetsResult] =
    logger.logged("workspaceBuildTargets") {
      request("workspaceBuildTargets") {
        underlying.workspaceBuildTargets
      }
    }
  def workspaceReload(): CompletableFuture[Object] =
    logger.logged("workspaceReload") {
      request("workspaceReload") {
        underlying.workspaceReload()
      }
    }

  // Members declared in b.JavaBuildServer
  def buildTargetJavacOptions(params: b.JavacOptionsParams)
    : CompletableFuture[b.JavacOptionsResult] =
    logger.logged("buildTargetJavacOptions", params) {
      request("buildTargetJavacOptions") {
        underlying.buildTargetJavacOptions(params)
      }
    }

  // Members declared in b.JvmBuildServer
  def buildTargetJvmRunEnvironment(params: b.JvmRunEnvironmentParams)
    : CompletableFuture[b.JvmRunEnvironmentResult] =
    logger.logged("buildTargetJvmRunEnvironment", params) {
      request("buildTargetJvmRunEnvironment") {
        underlying.buildTargetJvmRunEnvironment(params)
      }
    }
  def buildTargetJvmTestEnvironment(params: b.JvmTestEnvironmentParams)
    : CompletableFuture[b.JvmTestEnvironmentResult] =
    logger.logged("buildTargetJvmTestEnvironment", params) {
      request("buildTargetJvmTestEnvironment") {
        underlying.buildTargetJvmTestEnvironment(params)
      }
    }

  def buildTargetJvmCompileClasspath(params: ch.epfl.scala.bsp4j.JvmCompileClasspathParams)
    : java.util.concurrent.CompletableFuture[ch.epfl.scala.bsp4j.JvmCompileClasspathResult] =
    logger.logged("buildTargetJvmCompileClasspath", params) {
      request("buildTargetJvmCompileClasspath") {
        underlying.buildTargetJvmCompileClasspath(params)
      }
    }

  // Members declared in b.ScalaBuildServer
  @nowarn
  def buildTargetScalaMainClasses(params: b.ScalaMainClassesParams)
    : CompletableFuture[b.ScalaMainClassesResult] =
    logger.logged("buildTargetScalaMainClasses", params) {
      request("buildTargetScalaMainClasses") {
        underlying.buildTargetScalaMainClasses(params)
      }
    }
  @nowarn
  def buildTargetScalaTestClasses(params: b.ScalaTestClassesParams)
    : CompletableFuture[b.ScalaTestClassesResult] =
    logger.logged("buildTargetScalaTestClasses", params) {
      request("buildTargetScalaTestClasses") {
        underlying.buildTargetScalaTestClasses(params)
      }
    }
  def buildTargetScalacOptions(params: b.ScalacOptionsParams)
    : CompletableFuture[b.ScalacOptionsResult] =
    logger.logged("buildTargetScalacOptions", params) {
      request("buildTargetScalacOptions") {
        underlying.buildTargetScalacOptions(params)
      }
    }

  // Members declared in scala.build.bsp.ScalaScriptBuildServer
  def buildTargetWrappedSources(params: WrappedSourcesParams)
    : CompletableFuture[WrappedSourcesResult] =
    logger.logged("buildTargetWrappedSources", params) {
      request("buildTargetWrappedSources") {
        underlying.buildTargetWrappedSources(params)
      }
    }

  def onRunReadStdin(params: ch.epfl.scala.bsp4j.ReadParams): Unit =
    underlying.onRunReadStdin(params)
}

private object LoggingPlasmonBuildServer {
  implicit class LoggerExtras(private val logger: Logger) extends AnyVal {
    def logged[T](
      methodName: String,
      params: Object = null
    )(f: => CompletableFuture[T]): CompletableFuture[T] = {
      val methodCallStr =
        s"$methodName ${if (params == null) "()" else "( " + params.toString + " )"}"
      // logger.log("")
      // logger.log(s"Calling $methodCallStr")
      // logger.log("")
      f.handle {
        (valueOrNull, exOrNull) =>
          if (exOrNull == null)
            // logger.log("")
            // logger.log(s"$methodCallStr =")
            // logger.log(Objects.toString(valueOrNull))
            // logger.log("")
            valueOrNull
          else
            // logger.log("")
            // logger.log(s"Failed: $methodCallStr")
            // logger.log(exOrNull)
            // logger.log("")
            throw exOrNull
      }
    }
  }
}
