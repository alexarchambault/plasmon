package plasmon.bsp

import ch.epfl.scala.bsp4j as b
import com.google.gson.{Gson, JsonElement}
import plasmon.Logger

import java.io.File

import scala.collection.mutable
import scala.jdk.CollectionConverters.*

/** Builds the class directories a recorded build refers to, without the build tool that produced
  * it.
  *
  * A recording describes a build completely enough to redo it: which targets depend on which, the
  * sources of each, the class path to compile against, the compiler options, the output directory,
  * and - via the target's Scala data - the compiler jars themselves. So rather than storing compiled
  * output alongside the recording, we recreate it, which keeps `.class`, `.tasty` and any semanticdb
  * output genuine instead of round-tripped through some other representation.
  *
  * Still far cheaper than the build tool it stands in for: one compiler invocation per target, no
  * build tool JVM, no BSP, no dependency resolution beyond what is already in the coursier cache.
  */
final class ReplayCompiler(
  javaHome: os.Path,
  logger: Logger
) {

  private val gson = new Gson

  /** Last compiled state of each target's sources, so that an unchanged target isn't compiled
    * twice, but one whose sources were edited mid-test is.
    */
  private val compiled = mutable.Map.empty[b.BuildTargetIdentifier, Seq[(os.Path, Long, Long)]]

  /** Compiles `targets` and everything they depend on, dependants last. */
  def compile(
    targets: Seq[b.BuildTargetIdentifier],
    allTargets: Seq[b.BuildTarget],
    scalacOptions: Seq[b.ScalacOptionsItem],
    javacOptions: Seq[b.JavacOptionsItem],
    sources: Seq[b.SourcesItem]
  ): b.StatusCode = {

    val targetById    = allTargets.map(t => t.getId -> t).toMap
    val scalacById    = scalacOptions.map(i => i.getTarget -> i).toMap
    val javacById     = javacOptions.map(i => i.getTarget -> i).toMap
    val sourcesById   = sources.map(i => i.getTarget -> i).toMap

    // Dependencies first, so that a target compiles against class directories that already exist
    val ordered = {
      val seen   = mutable.Set.empty[b.BuildTargetIdentifier]
      val result = mutable.ListBuffer.empty[b.BuildTargetIdentifier]
      def visit(id: b.BuildTargetIdentifier): Unit =
        if (seen.add(id)) {
          for {
            target <- targetById.get(id).toSeq
            dep    <- target.getDependencies.asScala
          } visit(dep)
          result += id
        }
      targets.foreach(visit)
      result.toList
    }

    var status: b.StatusCode = b.StatusCode.OK
    for (id <- ordered) {
      val srcs  = sourceFiles(sourcesById.get(id))
      val stamp = srcs.map(p => (p, os.mtime(p), os.size(p)))
      if (!compiled.get(id).contains(stamp))
        if (compileOne(id, targetById.get(id), scalacById.get(id), javacById.get(id), srcs))
          compiled(id) = stamp
        else
          status = b.StatusCode.ERROR
    }
    status
  }

  /** Forgets what has been compiled, so that edited sources are picked up again. */
  def reset(): Unit =
    compiled.clear()

  private def sourceFiles(sourcesOpt: Option[b.SourcesItem]): Seq[os.Path] =
    sourcesOpt.toSeq.flatMap(_.getSources.asScala).flatMap { item =>
      val path = uriToPath(item.getUri)
      if (os.isDir(path)) os.walk(path).filter(os.isFile)
      else if (os.isFile(path)) Seq(path)
      // Generated source directories a build tool would have produced simply aren't there
      else Nil
    }
      .filter(p => p.last.endsWith(".scala") || p.last.endsWith(".java"))
      .distinct
      .sorted

  private def compileOne(
    id: b.BuildTargetIdentifier,
    targetOpt: Option[b.BuildTarget],
    scalacOpt: Option[b.ScalacOptionsItem],
    javacOpt: Option[b.JavacOptionsItem],
    sourceFiles: Seq[os.Path]
  ): Boolean = {

    val scalaSources = sourceFiles.filter(_.last.endsWith(".scala"))
    val javaSources  = sourceFiles.filter(_.last.endsWith(".java"))

    if (scalaSources.isEmpty && javaSources.isEmpty) true
    else {
      val classDirOpt = scalacOpt.map(i => uriToPath(i.getClassDirectory))
        .orElse(javacOpt.map(i => uriToPath(i.getClassDirectory)))
      classDirOpt match {
        case None =>
          logger.log(s"Replay: no class directory for ${id.getUri}, not compiling it")
          true
        case Some(classDir) =>
          os.makeDir.all(classDir)
          val classPath =
            scalacOpt.map(_.getClasspath.asScala.toSeq)
              .orElse(javacOpt.map(_.getClasspath.asScala.toSeq))
              .getOrElse(Nil)
              .map(uriToPath)
              .filter(os.exists)

          val scalaOk =
            if (scalaSources.isEmpty) true
            else
              scalaTarget(targetOpt) match {
                case None =>
                  logger.log(
                    s"Replay: no Scala compiler jars recorded for ${id.getUri}, not compiling it"
                  )
                  true
                case Some(scalaTarget0) =>
                  // Java sources go to scalac too, so that Scala code can see the Java types;
                  // scalac reads them for signatures without emitting anything for them
                  compileScala(
                    id,
                    scalaTarget0,
                    scalaSources ++ javaSources,
                    classPath,
                    classDir,
                    scalacOpt.map(_.getOptions.asScala.toSeq).getOrElse(Nil)
                  )
              }

          val javaOk =
            if (javaSources.isEmpty) true
            else
              compileJava(
                id,
                javaSources,
                classDir +: classPath,
                classDir,
                javacOpt.map(_.getOptions.asScala.toSeq).getOrElse(Nil)
              )

          scalaOk && javaOk
      }
    }
  }

  private def scalaTarget(targetOpt: Option[b.BuildTarget]): Option[b.ScalaBuildTarget] =
    for {
      target <- targetOpt
      if target.getDataKind == "scala"
      data <- Option(target.getData)
      // Recordings are read with plain gson, so the target's data arrives as raw JSON
      scalaTarget0 <-
        data match {
          case elem: JsonElement =>
            Option(gson.fromJson(elem, classOf[b.ScalaBuildTarget]))
          case already: b.ScalaBuildTarget => Some(already)
          case _                           => None
        }
      if !scalaTarget0.getJars.isEmpty
    } yield scalaTarget0

  private def compileScala(
    id: b.BuildTargetIdentifier,
    scalaTarget: b.ScalaBuildTarget,
    sources: Seq[os.Path],
    classPath: Seq[os.Path],
    classDir: os.Path,
    options: Seq[String]
  ): Boolean = {
    val compilerJars = scalaTarget.getJars.asScala.toSeq.map(uriToPath).filter(os.exists)
    if (compilerJars.isEmpty) {
      logger.log(s"Replay: recorded Scala compiler jars are missing for ${id.getUri}")
      false
    }
    else {
      val mainClass =
        if (scalaTarget.getScalaVersion.startsWith("3.")) "dotty.tools.dotc.Main"
        else "scala.tools.nsc.Main"
      run(
        id,
        Seq[os.Shellable](
          jdkTool("java"),
          "-cp",
          compilerJars.map(_.toString).mkString(File.pathSeparator),
          mainClass
        ),
        options ++ Seq("-classpath", classPath.map(_.toString).mkString(File.pathSeparator)) ++
          Seq("-d", classDir.toString) ++ sources.map(_.toString)
      )
    }
  }

  private def compileJava(
    id: b.BuildTargetIdentifier,
    sources: Seq[os.Path],
    classPath: Seq[os.Path],
    classDir: os.Path,
    options: Seq[String]
  ): Boolean =
    run(
      id,
      Seq[os.Shellable](jdkTool("javac")),
      options ++ Seq("-classpath", classPath.map(_.toString).mkString(File.pathSeparator)) ++
        Seq("-d", classDir.toString) ++ sources.map(_.toString)
    )

  /** Runs a compiler, passing its arguments in a file.
    *
    * Class paths here run to tens of entries, which is past what a command line takes on Windows.
    */
  private def run(
    id: b.BuildTargetIdentifier,
    command: Seq[os.Shellable],
    args: Seq[String]
  ): Boolean = {
    val argsFile = os.temp(
      args.map(quoteArg).mkString(System.lineSeparator()),
      prefix = "plasmon-replay-",
      suffix = ".args"
    )
    try {
      val res = os.proc(command, s"@$argsFile").call(
        check = false,
        stdout = os.Pipe,
        mergeErrIntoOut = true
      )
      val output = res.out.text().trim
      if (res.exitCode != 0) {
        logger.log(s"Replay: compiling ${id.getUri} failed (exit code ${res.exitCode})")
        if (output.nonEmpty) logger.log(output)
        false
      }
      else {
        logger.log(s"Replay: compiled ${id.getUri}")
        if (output.nonEmpty) logger.log(output)
        true
      }
    }
    finally os.remove(argsFile)
  }

  // Argument files split on whitespace unless quoted, and Windows paths are full of backslashes
  private def quoteArg(arg: String): String =
    "\"" + arg.replace("\\", "\\\\").replace("\"", "\\\"") + "\""

  private def jdkTool(name: String): os.Path =
    javaHome / "bin" / (if (scala.util.Properties.isWin) s"$name.exe" else name)

  private def uriToPath(uri: String): os.Path =
    os.Path(java.nio.file.Paths.get(new java.net.URI(uri)))
}
