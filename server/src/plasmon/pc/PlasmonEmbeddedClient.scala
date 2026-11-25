package plasmon.pc

import plasmon.ide.Directories

import java.nio.file.Path

import scala.meta.internal.metals.JdkSources
import scala.meta.pc.EmbeddedClient

/** What the Java presentation compiler needs on disk: the javac plugin that strips the bodies of
  * the sources it reads besides the one it's asked about, a directory for its output (and for the
  * JDK sources it compiles, that need `--patch-module`), and where Plasmon extracts JDK sources.
  *
  * @param workingDir
  *   the server working directory
  * @param targetId
  *   the build target of the compiler using this, which gets its own output directory
  */
final class PlasmonEmbeddedClient(workingDir: os.Path, targetId: String) extends EmbeddedClient {

  override def javaHeaderCompilerPluginJarPath(): Path =
    PlasmonEmbeddedClient.javaHeaderCompilerJar(workingDir).toNIO

  override lazy val targetDir: Path = {
    val dir = workingDir / Directories.state / "java-pc" / PlasmonEmbeddedClient.dirName(targetId)
    os.makeDir.all(dir)
    dir.toNIO
  }

  override def jdkSourcesReadonlyDir(): Path =
    (workingDir / Directories.dependencies / JdkSources.zipFileName).toNIO
}

object PlasmonEmbeddedClient {

  private val resourcePath = "plasmon/java-header-compiler.jar"

  private val extracted = new java.util.concurrent.ConcurrentHashMap[os.Path, os.Path]

  /** Extracts the plugin JAR from the server resources, once per working directory and process */
  def javaHeaderCompilerJar(workingDir: os.Path): os.Path =
    extracted.computeIfAbsent(
      workingDir,
      { workingDir =>
        val content = {
          val is = Option(getClass.getClassLoader.getResourceAsStream(resourcePath)).getOrElse {
            sys.error(s"Resource $resourcePath not found")
          }
          try is.readAllBytes()
          finally is.close()
        }
        val dest = workingDir / Directories.state / "lib" / "java-header-compiler.jar"
        if (!os.isFile(dest) || !java.util.Arrays.equals(os.read.bytes(dest), content)) {
          // Write then move, so that a concurrent server never sees a partial JAR
          val tmp = dest / os.up / s".java-header-compiler-${ProcessHandle.current().pid()}.jar"
          os.write.over(tmp, content, createFolders = true)
          os.move(tmp, dest, replaceExisting = true, atomicMove = true)
        }
        dest
      }
    )

  /** A directory name for a build target id, which is usually a URI */
  def dirName(targetId: String): String = {
    val readable = targetId
      .replaceAll("^[a-z]+:/*", "")
      .replaceAll("[^A-Za-z0-9._-]+", "_")
      .takeRight(60)
    val hash = Integer.toHexString(targetId.hashCode)
    s"$readable-$hash"
  }
}
