// Adapted from https://github.com/coursier/coursier/blob/631bb31debf28261c781f06f49b234a934dac8ea/mill-build/src/coursierbuild/GitHubReleaseAssets.scala

package plasmonbuild

// from https://github.com/coursier/coursier/blob/382250d4f26b4728400a0546088e27ca0f129e8b/scripts/shared/UploadGhRelease.sc

//import coursierbuild.DocHelpers.gitRepoHasChanges
// import coursierbuild.Launchers.{platformExtension, platformSuffix}

import sttp.client4.quick.*

import java.io.*
import java.nio.ByteBuffer
import java.nio.charset.{MalformedInputException, StandardCharsets}
import java.nio.file.Files
import java.util.zip.{ZipException, ZipFile}

import scala.annotation.tailrec
import scala.util.Properties
import scala.util.control.NonFatal

object GitHubReleaseAssets {
  def ghOrg  = "alexarchambault"
  def ghName = "plasmon"

  private def contentType(path: os.Path): String = {

    val isZipFile = {
      var zf: ZipFile = null
      try { zf = new ZipFile(path.toIO); true }
      catch { case _: ZipException => false }
      finally if (zf != null) zf.close()
    }

    lazy val isTextFile =
      try {
        StandardCharsets.UTF_8
          .newDecoder()
          .decode(ByteBuffer.wrap(os.read.bytes(path)))
        true
      }
      catch { case e: MalformedInputException => false }

    if (isZipFile) "application/zip"
    else if (isTextFile) "text/plain"
    else "application/octet-stream"
  }

  private def releaseId(
    ghOrg: String,
    ghProj: String,
    ghToken: String,
    tag: String
  ): Long = {
    val url = uri"https://api.github.com/repos/$ghOrg/$ghProj/releases"
    val resp = quickRequest
      .header("Accept", "application/vnd.github.v3+json")
      .header("Authorization", s"token $ghToken")
      .get(url)
      .send()

    val json = ujson.read(resp.body)
    val releaseId =
      try json
          .arr
          .find(_("tag_name").str == tag)
          .map(_("id").num.toLong)
          .getOrElse {
            val tags = json.arr.map(_("tag_name").str).toVector
            sys.error(s"Tag $tag not found (found tags: ${tags.mkString(", ")}")
          }
      catch {
        case NonFatal(e) =>
          System.err.println(resp.body)
          throw e
      }

    System.err.println(s"Release id is $releaseId")

    releaseId
  }

  def currentAssets(
    releaseId: Long,
    ghOrg: String,
    ghProj: String,
    ghToken: String
  ): Map[String, Long] = {

    val resp = quickRequest
      .header("Accept", "application/vnd.github.v3+json")
      .header("Authorization", s"token $ghToken")
      .get(uri"https://api.github.com/repos/$ghOrg/$ghProj/releases/$releaseId/assets")
      .send()
    val json = ujson.read(resp.body)
    json
      .arr
      .iterator
      .map { obj =>
        obj("name").str -> obj("id").num.toLong
      }
      .toMap
  }

  /** Uploads files as GitHub release assets.
    *
    * @param uploads
    *   List of local files / asset name to be uploaded
    * @param ghOrg
    *   GitHub organization of the release
    * @param ghProj
    *   GitHub project name of the release
    * @param ghToken
    *   GitHub token
    * @param tag
    *   Tag to upload assets to
    * @param dryRun
    *   Whether to run a dry run (printing the actions that would have been done, but not uploading
    *   anything)
    */
  def upload(
    ghOrg: String,
    ghProj: String,
    ghToken: String,
    tag: String,
    dryRun: Boolean,
    overwrite: Boolean
  )(
    uploads: (os.Path, String)*
  ): Unit = {

    val releaseId0 = releaseId(ghOrg, ghProj, ghToken, tag)

    val currentAssets0 =
      if (overwrite) currentAssets(releaseId0, ghOrg, ghProj, ghToken) else Map.empty[String, Long]

    for ((f0, name) <- uploads) {

      currentAssets0
        .get(name)
        .filter(_ => overwrite)
        .foreach { assetId =>
          val resp = quickRequest
            .header("Accept", "application/vnd.github.v3+json")
            .header("Authorization", s"token $ghToken")
            .delete(uri"https://api.github.com/repos/$ghOrg/$ghProj/releases/assets/$assetId")
            .send()
        }

      val uri =
        uri"https://uploads.github.com/repos/$ghOrg/$ghProj/releases/$releaseId0/assets?name=$name"
      val contentType0 = contentType(f0)
      System.err.println(s"Detected content type of $f0: $contentType0")
      if (dryRun)
        System.err.println(s"Would have uploaded $f0 as $name")
      else {
        System.err.println(s"Uploading $f0 as $name")
        quickRequest
          .header("Accept", "application/vnd.github.v3+json")
          .header("Authorization", s"token $ghToken")
          .body(f0.toNIO)
          .header("Content-Type", contentType0)
          .post(uri)
          .send()
      }
    }
  }

  def readInto(is: InputStream, os: OutputStream): Unit = {
    val buf  = Array.ofDim[Byte](1024 * 1024)
    var read = -1
    while ({
      read = is.read(buf)
      read >= 0
    }) os.write(buf, 0, read)
  }

  def writeInZip(name: String, file: os.Path, zip: os.Path): Unit = {
    import java.nio.file.attribute.FileTime
    import java.util.zip._

    os.makeDir.all(zip / os.up)

    var fis: InputStream     = null
    var fos: OutputStream    = null
    var zos: ZipOutputStream = null

    try {
      fis = os.read.inputStream(file)
      fos = Files.newOutputStream(zip.toNIO)
      zos = new ZipOutputStream(new BufferedOutputStream(fos))

      val ent = new ZipEntry(name)
      ent.setLastModifiedTime(FileTime.fromMillis(os.mtime(file)))
      ent.setSize(os.size(file))
      zos.putNextEntry(ent)
      readInto(fis, zos)
      zos.closeEntry()

      zos.finish()
    }
    finally {
      if (zos != null) zos.close()
      if (fos != null) fos.close()
      if (fis != null) fis.close()
    }
  }

  private def platformSuffix: String = {
    val arch = sys.props("os.arch").toLowerCase(java.util.Locale.ROOT) match {
      case "amd64" => "x86_64"
      case "arm64" => "aarch64"
      case other   => other
    }
    val os = {
      val p = System.getProperty("os.name").toLowerCase(java.util.Locale.ROOT)
      if (p.contains("linux")) "pc-linux"
      else if (p.contains("mac")) "apple-darwin"
      else if (p.contains("windows")) "pc-win32"
      else sys.error(s"Unrecognized OS: $p")
    }

    s"$arch-$os"
  }

  private def platformExtension: String =
    if (Properties.isWin) ".exe"
    else ""

  def copyLauncher(
    nativeLauncher: os.Path,
    directory: os.Path,
    suffix: String = ""
  ): Unit = {
    val name = s"plasmon-$platformSuffix$suffix$platformExtension"
    if (Properties.isWin)
      writeInZip(name, nativeLauncher, directory / s"plasmon-$platformSuffix.zip")
    else {
      val dest = directory / name
      os.copy(nativeLauncher, dest, createFolders = true, replaceExisting = true)
      os.proc("gzip", "-v", dest.toString).call(
        stdin = os.Inherit,
        stdout = os.Inherit,
        stderr = os.Inherit
      )
    }
  }

  def uploadLaunchers(
    version: String,
    directory: os.Path
  ): Unit = {
    val launchers = os.list(directory).filter(os.isFile(_)).map { path =>
      path -> path.last
    }
    val ghToken = Option(System.getenv("UPLOAD_GH_TOKEN")).getOrElse {
      sys.error("UPLOAD_GH_TOKEN not set")
    }
    val (tag, overwriteAssets) =
      if (version.endsWith("-SNAPSHOT")) ("latest", true)
      else ("v" + version, false)
    upload(ghOrg, ghName, ghToken, tag, dryRun = false, overwrite = overwriteAssets)(launchers*)
  }
}
