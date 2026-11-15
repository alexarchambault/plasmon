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

    val realRoot = root.toNIO.toRealPath()
    // The plasmon server lives in this process, with the page alongside it
    Server.start(
      serverOptions(os.Path(realRoot)),
      Some { inProcess =>
        val app    = new BrowseApp(realRoot, options.host, options.port, tls, inProcess)
        val stop   = app.start()
        val scheme = if (tls.isDefined) "https" else "http"
        System.err.println(s"Browsing $root at $scheme://${options.host}:${options.port}/")
        try inProcess.awaitExit()
        finally stop()
      }
    )
  }

  /** The options of the plasmon server running in this process.
    *
    * Presentation compilers read modules from their sources, so that nothing needs to be built, and
    * the build tool goes away once it said what the modules are. Its state lives in
    * `.plasmon/browser` rather than `.plasmon`, so that it doesn't get in the way of a server an
    * editor runs in the same directory (they'd wait for each other's lock otherwise). `plasmon`
    * commands don't find it on their own then: they look for `.plasmon/socket`.
    */
  private def serverOptions(root: os.Path): ServerOptions =
    ServerOptions(
      lsp = false,
      workingDir = Some(root.toString),
      logToStderr = Some(false),
      sourcepath = Some(true),
      stopBuildToolAfterLoad = true,
      stateDir = Some(".plasmon/browser")
    )

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
  tls: Option[SSLContext],
  inProcess: Server.InProcess
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
    def position(): Option[(Path, Int, Int)] =
      for {
        file <- resolve(param("path").getOrElse("")).filter(Files.isRegularFile(_))
        line <- param("line").flatMap(_.toIntOption)
        col  <- param("col").flatMap(_.toIntOption)
      } yield (file, line, col)
    if (path.endsWith("/api/tree")) tree(param("path").getOrElse(""))
    else if (path.endsWith("/api/file")) file(param("path").getOrElse(""))
    else if (path.endsWith("/api/status"))
      json(lsp.status.await(
        param("since").flatMap(_.toLongOption).getOrElse(-1L),
        param("fromLine").flatMap(_.toLongOption).getOrElse(0L),
        timeoutMs = 20000L
      ))
    else if (path.endsWith("/api/load"))
      resolve(param("path").getOrElse("")).filter(Files.isRegularFile(_)) match {
        case Some(file) =>
          lsp.load(os.Path(file))
          json(ujson.Obj())
        case None => cask.Response("File not found", statusCode = 404)
      }
    else if (path.endsWith("/api/hover"))
      position() match {
        case Some((file, line, col)) =>
          lsp.hover(os.Path(file), line, col) match {
            case Some(Right(res)) => json(BrowseApp.hoverJson(res))
            case Some(Left(err))  => json(ujson.Obj("error" -> err))
            case None             => json(ujson.Obj("superseded" -> true))
          }
        case None => cask.Response("Invalid position", statusCode = 400)
      }
    else if (path.endsWith("/api/diagnostics"))
      resolve(param("path").getOrElse("")).filter(Files.isRegularFile(_)) match {
        case Some(file) =>
          lsp.diagnostics(os.Path(file)) match {
            case Right(res) => json(ujson.Obj("diagnostics" -> BrowseApp.diagnosticsJson(res)))
            case Left(err)  => json(ujson.Obj("error" -> err))
          }
        case None => cask.Response("File not found", statusCode = 404)
      }
    else if (path.endsWith("/api/definition"))
      position() match {
        case Some((file, line, col)) =>
          lsp.definition(os.Path(file), line, col) match {
            case Right(res) => json(ujson.Obj("locations" -> definitionsJson(res)))
            case Left(err)  => json(ujson.Obj("error" -> err))
          }
        case None => cask.Response("Invalid position", statusCode = 400)
      }
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
  private val lsp        = new BrowseLsp(os.Path(root), inProcess)

  /** LSP locations, with their path relative to `root` when they're under it (`null` otherwise, the
    * page can't show them then)
    */
  private def definitionsJson(locations: ujson.Value): ujson.Arr = {
    val list = locations match {
      case ujson.Arr(values) => values.toSeq
      case _                 => Nil
    }
    ujson.Arr.from {
      list.map { loc =>
        val uri = loc("uri").str
        val relPath = Try(Path.of(new java.net.URI(uri))).toOption
          .flatMap(p => Try(p.toRealPath()).toOption)
          .filter(_.startsWith(root))
          .map(p => root.relativize(p).iterator().asScala.mkString("/"))
        val start = loc("range")("start")
        ujson.Obj(
          "uri"  -> uri,
          "path" -> relPath.fold[ujson.Value](ujson.Null)(ujson.Str(_)),
          "line" -> start("line").num,
          "col"  -> start("character").num
        )
      }
    }
  }

  private def json(value: ujson.Value): cask.Response[String] =
    cask.Response(value.render(), headers = Seq("Content-Type" -> "application/json"))

  private def tree(path: String): cask.Response[String] =
    resolve(path).filter(Files.isDirectory(_)) match {
      case None => cask.Response("Invalid directory", statusCode = 404)
      case Some(dir) =>
        val rules  = attributes.rules(dir)
        val stream = Files.list(dir)
        val children =
          try stream.iterator().asScala.toVector
          finally stream.close()
        val ignored = BrowseApp.gitIgnored(dir, children.map(_.getFileName.toString))
        val entries =
          children
            .map { child =>
              val name  = child.getFileName.toString
              val isDir = Files.isDirectory(child)
              val obj = ujson.Obj(
                "name" -> name,
                "path" -> root.relativize(child).iterator().asScala.mkString("/"),
                "dir"  -> isDir
              )
              if (ignored(name)) obj("ignored") = true
              if (!isDir)
                for (lang <- attributes.value(rules, child, "linguist-language"))
                  obj("language") = lang
              obj
            }
            .sortBy(v => (!v("dir").bool, v("name").str.toLowerCase(java.util.Locale.ROOT)))
        json(ujson.Arr(entries*))
    }

  private def file(path: String): cask.Response[String] =
    resolve(path).filter(Files.isRegularFile(_)) match {
      case None => cask.Response("File not found", statusCode = 404)
      case Some(value) =>
        val size = Files.size(value)
        val obj  = ujson.Obj("size" -> size.toDouble)
        for (
          lang <- attributes.value(attributes.rules(value.getParent), value, "linguist-language")
        )
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

  /** Starts the web server, returns what stops it */
  def start(): () => Unit = {
    if (!verbose) cask.main.Main.silenceJboss()
    val builder = Undertow.builder().setHandler(defaultHandler)
    tls.fold(builder.addHttpListener(port, host))(context =>
      builder.addHttpsListener(port, host, context)
    )
    val server = builder.build()
    server.start()
    () => {
      server.stop()
      executionContext.shutdown()
    }
  }

  initialize()
}

private object BrowseApp {
  private val maxFileSize = 10L * 1024 * 1024

  /** The names among `names`, entries of `dir`, that Git ignores (and `.git` itself)
    *
    * Asks `git check-ignore`, that knows about all the ignore files and settings, and reports the
    * content of ignored directories too, but not tracked files. Nothing is ignored when `dir` isn't
    * in a Git repository, or if `git` can't be run.
    */
  def gitIgnored(dir: Path, names: Seq[String]): Set[String] = {
    val fromGit =
      if (names.isEmpty) Set.empty[String]
      else
        Try {
          val res = os.proc("git", "check-ignore", "-z", "--stdin").call(
            cwd = os.Path(dir),
            stdin = names.map(_ + "\u0000").mkString,
            stderr = os.Pipe,
            check = false
          )
          // 0: some paths are ignored, 1: none are, anything else: error (not a repository, …)
          if (res.exitCode == 0) res.out.text().split('\u0000').iterator.filter(_.nonEmpty).toSet
          else Set.empty[String]
        }.getOrElse(Set.empty[String])
    fromGit ++ names.filter(_ == ".git")
  }

  /** LSP diagnostics, flattened: `{"line", "col", "endLine", "endCol", "message", "severity"}`
    *
    * Their message is an lsp4j `Either`, that the command writes as `{"left": …}` / `{"right": …}`.
    */
  def diagnosticsJson(diagnostics: ujson.Value): ujson.Arr =
    ujson.Arr.from {
      diagnostics.arrOpt.toSeq.flatten.map { diag =>
        val message = diag.obj.get("message") match {
          case Some(ujson.Str(s)) => s
          case Some(o: ujson.Obj) =>
            o.value.get("left").map(_.str)
              .orElse(o.value.get("right").flatMap(_.obj.get("value")).map(_.str))
              .getOrElse("")
          case _ => ""
        }
        val range = diag("range")
        ujson.Obj(
          "line"     -> range("start")("line").num,
          "col"      -> range("start")("character").num,
          "endLine"  -> range("end")("line").num,
          "endCol"   -> range("end")("character").num,
          "message"  -> message,
          "severity" -> diag.obj.get("severity").map(_.str.toLowerCase).getOrElse("error")
        )
      }
    }

  /** An LSP hover as `{"markdown": …, "range": …}`
    *
    * The command prints the hover with a plain Gson, which writes the lsp4j `Either`s of its
    * contents as `{"left": …}` / `{"right": …}`. Takes the other shapes the LSP allows too.
    */
  def hoverJson(hover: ujson.Value): ujson.Value =
    hover match {
      case obj: ujson.Obj if obj.value.contains("contents") =>
        def either(v: ujson.Value): ujson.Value = v match {
          case o: ujson.Obj if o.value.contains("left")  => o("left")
          case o: ujson.Obj if o.value.contains("right") => o("right")
          case other                                     => other
        }
        def markedString(v: ujson.Value): String = either(v) match {
          case ujson.Str(s) => s
          case o: ujson.Obj =>
            val value = o.value.get("value").map(_.str).getOrElse("")
            o.value.get("language") match {
              case Some(lang) => s"```${lang.str}\n$value\n```"
              case None       => value
            }
          case _ => ""
        }
        val markdown = either(obj("contents")) match {
          case ujson.Arr(values) => values.map(markedString).mkString("\n\n---\n\n")
          case other             => markedString(other)
        }
        ujson.Obj(
          "markdown" -> markdown,
          "range"    -> obj.value.getOrElse("range", ujson.Null)
        )
      case _ => ujson.Obj("markdown" -> ujson.Null)
    }
}
