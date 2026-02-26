// Originally based on https://github.com/scalameta/metals/blob/2a6f8a437a1ce7c44140673edfe34bb74dfd33be/metals/src/main/scala/scala/meta/internal/builds/SbtBuildTool.scala or an earlier version of that file

package plasmon.bsp

import plasmon.PlasmonEnrichments.*

import java.nio.charset.StandardCharsets

import scala.meta.internal.semver.SemVer

object SbtBuildTool {

  // We remove legacy metals.sbt file that was located in
  // global sbt plugins and which adds the plugin to each projects
  // and creates additional overhead.
  def removeLegacyGlobalPlugin(version: String): (os.Path, Boolean) = {
    def pluginsDirectory(version: String): os.Path =
      os.Path(System.getProperty("user.home")) / ".sbt" / version / "plugins"
    val plugins =
      if (version.startsWith("0.13")) pluginsDirectory("0.13")
      else pluginsDirectory("1.0")

    val metalsFile = plugins / "metals.sbt"
    val exists     = os.exists(metalsFile)
    if (exists) os.remove(metalsFile)
    (metalsFile, exists)
  }

  def writeBloopPlugin(
    projectRoot: os.Path,
    bloopVersion: String,
    version: String
  ): Seq[(os.Path, Boolean)] = {

    def sbtMetaDirs(
      meta: os.Path,
      acc: List[os.Path]
    ): List[os.Path] =
      if (os.exists(meta)) {
        val files     = os.list(meta).toList
        val hasSbtSrc = files.exists(f => f.isSbt && f.last != "metals.sbt")
        if (hasSbtSrc) {
          val forSbtSupport = meta / "project/project"
          sbtMetaDirs(meta / "project", forSbtSupport :: acc)
        }
        else
          acc
      }
      else
        acc

    val pluginVersion =
      // from 1.4.6 Bloop is not compatible with sbt < 1.3.0
      if (SemVer.isLaterVersion(version, "1.3.0"))
        "1.4.6"
      else
        bloopVersion

    val plugin       = bloopPluginDetails(pluginVersion)
    val mainMeta     = projectRoot / "project"
    val metaMeta     = projectRoot / "project/project"
    val sbtMetaDirs0 = sbtMetaDirs(mainMeta, List(metaMeta, mainMeta)).reverse
    sbtMetaDirs0.map(writePlugins(_, plugin))
  }

  private case class PluginDetails(
    description: Seq[String],
    artifact: String,
    resolver: Option[String]
  )

  private def sbtPlugin(plugin: PluginDetails): String = {
    val resolvers   = plugin.resolver.getOrElse("")
    val description = plugin.description.mkString("// ", "\n// ", "")

    s"""|$description
        |$resolvers
        |addSbtPlugin(${plugin.artifact})
        |""".stripMargin
  }

  private def writePlugins(
    projectDir: os.Path,
    plugins: PluginDetails*
  ): (os.Path, Boolean) = {
    val content =
      s"""|// format: off
          |// DO NOT EDIT! This file is auto-generated.
          |
          |${plugins.map(sbtPlugin).mkString("\n")}
          |// format: on
          |""".stripMargin
    val bytes = content.getBytes(StandardCharsets.UTF_8)
    os.makeDir.all(projectDir)
    val metalsPluginFile = projectDir / "metals.sbt"
    val pluginFileShouldChange = !os.isFile(metalsPluginFile) ||
      !os.read.bytes(metalsPluginFile).sameElements(bytes)

    if (pluginFileShouldChange)
      os.write(metalsPluginFile, bytes)
    (metalsPluginFile, pluginFileShouldChange)
  }

  private def sonatypeResolver(version: String): Option[String] =
    if (version.contains("SNAPSHOT"))
      Some(
        """resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots""""
      )
    else None

  private def bloopPluginDetails(version: String): PluginDetails =
    PluginDetails(
      description =
        Seq("This file enables sbt-bloop to create bloop config files."),
      artifact = s""""ch.epfl.scala" % "sbt-bloop" % "$version"""",
      sonatypeResolver(version)
    )
}
