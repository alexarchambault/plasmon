package plasmon.command

import caseapp.core.{Indexed, RemainingArgs}

import java.net.URI
import java.net.http.{HttpClient, HttpRequest, HttpResponse}
import java.security.KeyStore
import java.util.Base64
import javax.net.ssl.{SSLContext, TrustManagerFactory}

/** The options of `plasmon browse`, and serving over HTTPS */
class BrowseTests extends munit.FunSuite {
  import BrowseAppTests.{Fake, Reply, freePort}

  private val tmpDir = FunFixture[os.Path](
    setup = _ => os.temp.dir(prefix = "plasmon-browse-tests"),
    teardown = dir => os.remove.all(dir)
  )

  private def runError(options: BrowseOptions, args: String*): String = {
    val remaining = RemainingArgs(args.map(Indexed(0, 1, _)), Nil)
    intercept[RuntimeException](Browse.run(options, remaining)).getMessage
  }

  tmpDir.test("invalid arguments") { dir =>
    os.write(dir / "file", "")
    os.makeDir(dir / "a")
    os.makeDir(dir / "b")
    val opts = BrowseOptions()
    assert(runError(
      opts,
      (dir / "a").toString,
      (dir / "b").toString
    ).startsWith("Expected at most one directory, got 2"))
    assertEquals(runError(opts, (dir / "nope").toString), s"Not a directory: ${dir / "nope"}")
    assertEquals(runError(opts, (dir / "file").toString), s"Not a directory: ${dir / "file"}")
    for (port <- Seq(-1, 65536))
      assertEquals(runError(opts.copy(port = port), dir.toString), s"Invalid port: $port")
    assertEquals(
      runError(opts.copy(cert = Some((dir / "file").toString)), dir.toString),
      "--cert and --key must be supplied together"
    )
    assertEquals(
      runError(opts.copy(key = Some((dir / "file").toString)), dir.toString),
      "--cert and --key must be supplied together"
    )
  }

  /** A self-signed certificate for 127.0.0.1 and its private key, as PEM files */
  private def certificate(dir: os.Path, keyAlg: Seq[String]): (os.Path, os.Path) = {
    val keytool  = os.Path(sys.props("java.home")) / "bin" / "keytool"
    val store    = dir / "store.p12"
    val password = "changeit"
    os.proc(
      keytool,
      "-genkeypair",
      "-alias",
      "browse",
      keyAlg,
      "-dname",
      "CN=localhost",
      "-ext",
      "san=ip:127.0.0.1",
      "-validity",
      "2",
      "-storetype",
      "PKCS12",
      "-keystore",
      store,
      "-storepass",
      password,
      "-keypass",
      password
    ).call(stderr = os.Pipe)
    val ks = KeyStore.getInstance("PKCS12")
    ks.load(os.read.inputStream(store), password.toCharArray)
    def pem(kind: String, bytes: Array[Byte]) =
      s"-----BEGIN $kind-----\n" +
        Base64.getMimeEncoder(64, "\n".getBytes).encodeToString(bytes) +
        s"\n-----END $kind-----\n"
    val cert = dir / "cert.pem"
    val key  = dir / "key.pem"
    os.write(cert, pem("CERTIFICATE", ks.getCertificate("browse").getEncoded))
    os.write(key, pem("PRIVATE KEY", ks.getKey("browse", password.toCharArray).getEncoded))
    (cert, key)
  }

  private def trusting(cert: os.Path): SSLContext = {
    val ks = KeyStore.getInstance("PKCS12")
    ks.load(null, null)
    ks.setCertificateEntry(
      "browse",
      java.security.cert.CertificateFactory.getInstance("X.509")
        .generateCertificate(os.read.inputStream(cert))
    )
    val tmf = TrustManagerFactory.getInstance(TrustManagerFactory.getDefaultAlgorithm)
    tmf.init(ks)
    val context = SSLContext.getInstance("TLS")
    context.init(null, tmf.getTrustManagers, null)
    context
  }

  for ((name, keyAlg) <- Seq("RSA" -> Seq("-keyalg", "RSA"), "EC" -> Seq("-keyalg", "EC")))
    tmpDir.test(s"HTTPS ($name key)") { dir =>
      val (cert, key) = certificate(dir, keyAlg)
      val root        = dir / "project"
      os.write(root / "Foo.scala", "object Foo\n", createFolders = true)
      val tls  = Browse.sslContext(cert, key)
      val port = freePort()
      val app = new BrowseApp(
        root.toNIO.toRealPath(),
        "127.0.0.1",
        port,
        Some(tls),
        new Fake(_ => Reply()).inProcess
      )
      val stop = app.start()
      try {
        val client = HttpClient.newBuilder().sslContext(trusting(cert)).build()
        def get(path: String) =
          client.send(
            HttpRequest.newBuilder(URI.create(s"https://127.0.0.1:$port$path")).build(),
            HttpResponse.BodyHandlers.ofString()
          )
        val page = get("/")
        assertEquals(page.statusCode(), 200)
        assert(page.body().contains("<title>project</title>"))
        assertEquals(
          ujson.read(get("/api/file?path=Foo.scala").body())("content").str,
          "object Foo\n"
        )
      }
      finally stop()
    }

  tmpDir.test("invalid TLS files") { dir =>
    val (cert, key) = certificate(dir, Seq("-keyalg", "EC"))
    def error(cert: os.Path, key: os.Path) =
      intercept[Exception](Browse.sslContext(cert, key)).getMessage

    val encrypted = dir / "encrypted.pem"
    os.write(
      encrypted,
      "-----BEGIN ENCRYPTED PRIVATE KEY-----\nAAAA\n-----END ENCRYPTED PRIVATE KEY-----\n"
    )
    assertEquals(error(cert, encrypted), "Encrypted private keys are not supported")

    val notAKey = dir / "not-a-key.pem"
    os.write(notAKey, "-----BEGIN PRIVATE KEY-----\nAAAA\n-----END PRIVATE KEY-----\n")
    assertEquals(error(cert, notAKey), s"Could not read PKCS#8 private key $notAKey")

    val empty = dir / "empty.pem"
    os.write(empty, "")
    assertEquals(error(empty, key), s"No certificate found in $empty")
  }
}
