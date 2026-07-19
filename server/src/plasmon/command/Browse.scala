package plasmon.command

import caseapp.core.RemainingArgs
import io.undertow.Undertow

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, LinkOption, Path}
import java.security.{KeyFactory, KeyStore}
import java.security.cert.{Certificate, CertificateFactory}
import java.security.spec.PKCS8EncodedKeySpec
import java.util.Base64
import javax.net.ssl.{KeyManagerFactory, SSLContext}

import scala.jdk.CollectionConverters.*
import scala.util.Try

object Browse extends caseapp.Command[BrowseOptions] {
  def run(options: BrowseOptions, remainingArgs: RemainingArgs): Unit = {
    val root = remainingArgs.all match {
      case Seq()    => os.pwd
      case Seq(str) => os.Path(str, os.pwd)
      case other =>
        sys.error(s"Expected at most one directory, got ${other.length}: ${other.mkString(", ")}")
    }
    if (!os.isDir(root))
      sys.error(s"Not a directory: $root")
    if (options.port < 0 || options.port > 65535)
      sys.error(s"Invalid port: ${options.port}")

    val tls = (options.cert, options.key) match {
      case (None, None)       => None
      case (Some(c), Some(k)) => Some(sslContext(os.Path(c, os.pwd), os.Path(k, os.pwd)))
      case _                  => sys.error("--cert and --key must be supplied together")
    }

    val app    = new BrowseApp(root.toNIO.toRealPath(), options.host, options.port, tls)
    val scheme = if (tls.isDefined) "https" else "http"
    System.err.println(s"Browsing $root at $scheme://${options.host}:${options.port}/")
    app.main(Array.empty)
  }

  private def sslContext(certPath: os.Path, keyPath: os.Path): SSLContext = {
    val certificates = {
      val in = Files.newInputStream(certPath.toNIO)
      try CertificateFactory.getInstance("X.509").generateCertificates(in).asScala.toSeq
      finally in.close()
    }
    if (certificates.isEmpty) sys.error(s"No certificate found in $certPath")

    val pem = Files.readString(keyPath.toNIO, StandardCharsets.US_ASCII)
    if (pem.contains("ENCRYPTED PRIVATE KEY"))
      sys.error("Encrypted private keys are not supported")
    val encoded = pem
      .replaceAll("-----BEGIN [^-]+-----", "")
      .replaceAll("-----END [^-]+-----", "")
      .replaceAll("\\s", "")
    val spec = new PKCS8EncodedKeySpec(Base64.getDecoder.decode(encoded))
    val privateKey = Seq("RSA", "EC", "DSA", "Ed25519", "Ed448")
      .iterator
      .flatMap(algorithm => Try(KeyFactory.getInstance(algorithm).generatePrivate(spec)).toOption)
      .nextOption()
      .getOrElse(sys.error(s"Could not read PKCS#8 private key $keyPath"))

    val password = Array.emptyCharArray
    val store    = KeyStore.getInstance("PKCS12")
    store.load(null, password)
    store.setKeyEntry("browse", privateKey, password, certificates.toArray[Certificate])
    val managers = KeyManagerFactory.getInstance(KeyManagerFactory.getDefaultAlgorithm)
    managers.init(store, password)
    val context = SSLContext.getInstance("TLS")
    context.init(managers.getKeyManagers, null, null)
    context
  }
}

private final class BrowseApp(
  root: Path,
  override val host: String,
  override val port: Int,
  tls: Option[SSLContext]
) extends cask.MainRoutes {

  override def debugMode = false

  // A single route for every path, so that this works behind proxies that keep a path
  // prefix (`/some/prefix/index.html`, `/some/prefix/api/tree`, …) as well as ones that strip it.
  // The page calls the API with URLs relative to its own. Extra query parameters, that proxies
  // may add, are ignored.
  @cask.get("/", subpath = true)
  def all(request: cask.Request, params: cask.QueryParams) = {
    val path = request.exchange.getRequestPath
    def param(name: String): Option[String] =
      params.value.get(name).flatMap(_.headOption)
    if (path.endsWith("/api/tree")) tree(param("path").getOrElse(""))
    else if (path.endsWith("/api/file")) file(param("path").getOrElse(""))
    else if (path.endsWith("/") || path.endsWith("/index.html"))
      cask.Response(page, headers = Seq("Content-Type" -> "text/html; charset=utf-8"))
    else {
      // `/some/prefix` -> `/some/prefix/`, with a relative redirect so that the prefix is kept
      val last = path.split('/').lastOption.getOrElse("")
      val location =
        java.net.URLEncoder.encode(last, StandardCharsets.UTF_8).replace("+", "%20") + "/"
      cask.Response("", statusCode = 302, headers = Seq("Location" -> location))
    }
  }

  private val attributes = new GitAttributes(root)

  private def json(value: ujson.Value): cask.Response[String] =
    cask.Response(value.render(), headers = Seq("Content-Type" -> "application/json"))

  private def tree(path: String): cask.Response[String] =
    resolve(path).filter(Files.isDirectory(_)) match {
      case None => cask.Response("Invalid directory", statusCode = 404)
      case Some(dir) =>
        val rules  = attributes.rules(dir)
        val stream = Files.list(dir)
        val entries =
          try stream.iterator().asScala.toSeq
              .map { child =>
                val isDir = Files.isDirectory(child)
                val obj = ujson.Obj(
                  "name" -> child.getFileName.toString,
                  "path" -> root.relativize(child).iterator().asScala.mkString("/"),
                  "dir"  -> isDir
                )
                if (!isDir)
                  for (lang <- attributes.value(rules, child, "linguist-language"))
                    obj("language") = lang
                obj
              }
              .sortBy(v => (!v("dir").bool, v("name").str.toLowerCase(java.util.Locale.ROOT)))
          finally stream.close()
        json(ujson.Arr(entries*))
    }

  private def file(path: String): cask.Response[String] =
    resolve(path).filter(Files.isRegularFile(_)) match {
      case None => cask.Response("File not found", statusCode = 404)
      case Some(value) =>
        val size = Files.size(value)
        val obj  = ujson.Obj("size" -> size.toDouble)
        for (lang <- attributes.value(attributes.rules(value.getParent), value, "linguist-language"))
          obj("language") = lang
        if (size > BrowseApp.maxFileSize) obj("tooLarge") = true
        else {
          val bytes = Files.readAllBytes(value)
          if (bytes.iterator.take(8000).contains(0: Byte)) obj("binary") = true
          else obj("content") = new String(bytes, StandardCharsets.UTF_8)
        }
        json(obj)
    }

  private def resolve(relative: String): Option[Path] =
    Try(root.resolve(relative).normalize()).toOption
      .filter(_.startsWith(root))
      .flatMap(path => Try(path.toRealPath()).toOption)
      .filter(_.startsWith(root))

  private lazy val page =
    BrowsePage.render(Option(root.getFileName).fold(root.toString)(_.toString))

  override def main(args: Array[String]): Unit = {
    if (!verbose) cask.main.Main.silenceJboss()
    val builder = Undertow.builder().setHandler(defaultHandler)
    tls.fold(builder.addHttpListener(port, host))(context => builder.addHttpsListener(port, host, context))
    val server = builder.build()
    server.start()
    Runtime.getRuntime.addShutdownHook(new Thread(() => {
      server.stop()
      executionContext.shutdown()
    }))
  }

  initialize()
}

private object BrowseApp {
  private val maxFileSize = 10L * 1024 * 1024
}
