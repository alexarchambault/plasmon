package plasmon.pc

import org.eclipse.lsp4j as l

import java.net.URI
import java.util.concurrent.{ConcurrentLinkedQueue, TimeUnit}
import java.util.zip.{ZipEntry, ZipOutputStream}
import javax.tools.{JavaFileObject, StandardLocation}

import scala.concurrent.ExecutionContext
import scala.jdk.CollectionConverters.*
import scala.meta.internal.metals.{CompilerOffsetParams, CtSym, PackageIndex}
import scala.meta.internal.mtags.{GlobalSymbolIndex, SourcePath}
import scala.meta.internal.jpc.{
  CustomFileManager,
  JavaMetalsCompiler,
  JavaPresentationCompiler,
  ModuleLocation
}

/** Which JDK files [[JdkClasses]] picks, and presentation compilers reading the JDK classes from
  * each kind of them
  */
class JdkClassesTests extends munit.FunSuite {

  override def munitTimeout = scala.concurrent.duration.Duration(2, "min")

  private def javaHome = os.Path(sys.props("java.home"))

  private def jdkCp(jdkFiles: TestJdkFiles): Seq[os.Path] = {
    val cp = jdkFiles.files(javaHome)
    assert(cp.nonEmpty && cp.forall(os.isFile(_)), s"No ${jdkFiles.id} in $javaHome")
    cp
  }

  private lazy val scalaLibrary: os.Path =
    sys.props("java.class.path")
      .split(java.io.File.pathSeparator)
      .iterator
      .map(os.Path(_, os.pwd))
      .find(p => p.last.startsWith("scala-library-") && p.last.endsWith(".jar"))
      .getOrElse(sys.error("scala-library not found in the class path of the tests"))

  test("ct.sym layout") {
    val layout  = CtSym.layout((javaHome / "lib/ct.sym").toNIO)
    val feature = Runtime.version().feature()
    // ct.sym has the signatures of the JDK's own release since JDK 22
    val expectedRelease = CtSym.releaseChar(if (feature >= 22) feature else feature - 1)
    assertEquals(layout.release, expectedRelease)
    assert(layout.modules.contains("java.base"))
    assert(layout.modules.contains("java.sql"))
    assert(layout.modules("java.base").forall(_.endsWith("/java.base/")))
    assert(layout.modules("java.base").forall(_.takeWhile(_ != '/').contains(layout.release)))
    assertEquals(
      layout.relativePath(layout.modules("java.base").head + "java/lang/String.sig"),
      Some("java/lang/String.sig")
    )
  }

  /** A fake JDK home, whose ct.sym (if any) has signatures up to `ctSymRelease` */
  private def withFakeJdk[T](
    major: Int,
    ctSymRelease: Option[Int],
    jmods: Boolean
  )(f: os.Path => T): T = {
    val home = os.temp.dir(prefix = "plasmon-fake-jdk")
    try {
      os.write(home / "release", s"JAVA_VERSION=\"$major.0.1\"\n")
      for (release <- ctSymRelease) {
        val zos = new ZipOutputStream(
          os.write.outputStream(home / "lib/ct.sym", createFolders = true)
        )
        try {
          val dir = s"${CtSym.releaseChar(release - 1)}${CtSym.releaseChar(release)}"
          zos.putNextEntry(new ZipEntry(s"$dir/java.base/java/lang/Object.sig"))
          zos.closeEntry()
          // what JDKs up to 21 have for their own release
          zos.putNextEntry(new ZipEntry(s"${CtSym.releaseChar(release + 1)}/system-modules"))
          zos.closeEntry()
        }
        finally zos.close()
      }
      if (jmods)
        os.write(home / "jmods/java.base.jmod", Array.emptyByteArray, createFolders = true)
      f(home)
    }
    finally os.remove.all(home)
  }

  test("ct-sym picks ct.sym when it has the JDK's own release") {
    withFakeJdk(25, ctSymRelease = Some(25), jmods = true) { home =>
      assertEquals(JdkClasses.CtSym.classPath(home), Seq(home / "lib/ct.sym"))
      assertEquals(JdkClasses.Jmods.classPath(home), Seq(home / "jmods/java.base.jmod"))
    }
  }

  test("ct-sym falls back to jmods when ct.sym lacks the JDK's own release") {
    withFakeJdk(21, ctSymRelease = Some(20), jmods = true) { home =>
      assertEquals(JdkClasses.CtSym.classPath(home), Seq(home / "jmods/java.base.jmod"))
    }
  }

  test("ct-sym uses an older ct.sym anyway without jmods") {
    withFakeJdk(21, ctSymRelease = Some(20), jmods = false) { home =>
      assertEquals(JdkClasses.CtSym.classPath(home), Seq(home / "lib/ct.sym"))
      assertEquals(JdkClasses.Jmods.classPath(home), Nil)
    }
  }

  test("JDK without jmods") {
    withFakeJdk(24, ctSymRelease = Some(24), jmods = false) { home =>
      assertEquals(JdkClasses.CtSym.classPath(home), Seq(home / "lib/ct.sym"))
      assertEquals(JdkClasses.Jmods.classPath(home), Nil)
    }
  }

  test("Java 8") {
    val home = os.temp.dir(prefix = "plasmon-fake-jdk")
    try {
      os.write(home / "jre/lib/rt.jar", Array.emptyByteArray, createFolders = true)
      for (jdkClasses <- JdkClasses.all)
        assertEquals(jdkClasses.classPath(home), Seq(home / "jre/lib/rt.jar"))
    }
    finally os.remove.all(home)
  }

  for (jdkFiles <- TestJdkFiles.all) {

    test(s"package index (${jdkFiles.id})") {
      val packages = new PackageIndex
      for (entry <- jdkCp(jdkFiles))
        packages.visit(entry.toNIO, _ => false)
      val javaLang = Option(packages.packages.get("java/lang/")).map(_.asScala.toSet)
      assert(javaLang.exists(_.contains("String.class")), s"java/lang/: $javaLang")
      val javaSql = Option(packages.packages.get("java/sql/")).map(_.asScala.toSet)
      assert(javaSql.exists(_.contains("Connection.class")), s"java/sql/: $javaSql")
    }

    test(s"Scala 3 presentation compiler (${jdkFiles.id})") {
      val logs   = new ConcurrentLinkedQueue[String]
      val module = "buildTarget:file:///test/?id=test"
      val pc = new dotty.tools.pc.ScalaPresentationCompiler(
        javaHome.toNIO,
        () => logs.add(_),
        _ => (),
        GlobalSymbolIndex.Module.fromString(module)
      ).newInstance(
        module,
        (scalaLibrary +: jdkCp(jdkFiles)).map(_.toNIO).asJava,
        Nil.asJava
      )
      try {
        val code =
          "package a\nobject A {\n  val s = \"a\".trim\n  def c: java.sql.Connection = ???\n}\n"
        def definition(word: String) = {
          val res = pc.definition(
            CompilerOffsetParams(URI.create("file:///test/A.scala"), code, code.indexOf(word) + 1)
          ).get(1, TimeUnit.MINUTES)
          (res.symbol(), res.locations().asScala.toList.map(_.getUri))
        }
        val (trimSym, trimLocs) = definition("trim")
        assertEquals(trimSym, "java/lang/String#trim().", clue(logs.asScala.mkString("\n")))
        assert(trimLocs.exists(_.contains(jdkFiles.locationMarker)), clue(trimLocs))
        // a class from a module other than java.base
        assertEquals(definition("Connection")._1, "java/sql/Connection#")
      }
      finally pc.shutdown()
    }

    test(s"Java file manager (${jdkFiles.id})") {
      val ctx = new SourcePath.Context
      try {
        val fileManager = new CustomFileManager(
          jdkCp(jdkFiles).map(_.toNIO),
          JavaMetalsCompiler.COMPILER.getStandardFileManager(null, null, null),
          ctx
        )
        val modules = fileManager
          .listLocationsForModules(StandardLocation.SYSTEM_MODULES)
          .asScala
          .flatMap(_.asScala)
          .toList
        val names = modules.collect { case loc: ModuleLocation => loc.moduleName }.toSet
        assert(names.contains("java.base"), clue(names))
        assert(names.contains("java.sql"), clue(names))

        val javaBase =
          fileManager.getLocationForModule(StandardLocation.SYSTEM_MODULES, "java.base")
        val string =
          fileManager.getJavaFileForInput(javaBase, "java.lang.String", JavaFileObject.Kind.CLASS)
        assert(string != null)
        assert(string.toUri.toString.contains(jdkFiles.locationMarker), clue(string.toUri))
        assertEquals(fileManager.inferBinaryName(javaBase, string), "java.lang.String")
        val bytes = string.openInputStream().readAllBytes()
        // class file magic number (ct.sym signatures are class files too)
        assertEquals(bytes.take(4).toSeq.map(_ & 0xff), Seq(0xca, 0xfe, 0xba, 0xbe))

        val javaLang = fileManager
          .list(javaBase, "java.lang", Set(JavaFileObject.Kind.CLASS).asJava, false)
          .asScala
          .map(fileManager.inferBinaryName(javaBase, _))
          .toSet
        assert(javaLang.contains("java.lang.String"))
        assert(!javaLang.exists(_.startsWith("java.lang.invoke.")))
      }
      finally ctx.close()
    }

    test(s"Java presentation compiler (${jdkFiles.id})") {
      val ctx     = new SourcePath.Context
      val workDir = os.temp.dir(prefix = "plasmon-jdk-classes-tests")
      try {
        val cp = jdkCp(jdkFiles).map(_.toNIO)
        val fileManager = new CustomFileManager(
          cp,
          JavaMetalsCompiler.COMPILER.getStandardFileManager(null, null, null),
          ctx
        )
        val logs     = new ConcurrentLinkedQueue[String]
        val targetId = "buildTarget:file:///test/?id=test"
        val pc = JavaPresentationCompiler(
          logs.add(_),
          buildTargetId = targetId,
          classpath = cp,
          ec = ExecutionContext.global,
          javaFileManagerFactory = (_, _) => fileManager,
          embedded = new PlasmonEmbeddedClient(workDir, targetId)
        )
        try {
          val code =
            "package a;\nclass A {\n  String s = \"a\".trim();\n  java.sql.Connection c;\n}\n"
          def hover(word: String): String = {
            val res = pc.hover(
              CompilerOffsetParams(URI.create("file:///test/A.java"), code, code.indexOf(word) + 1)
            ).get(1, TimeUnit.MINUTES)
            assert(res.isPresent, s"No hover for $word\n${logs.asScala.mkString("\n")}")
            res.get.toLsp().getContents.getRight.asInstanceOf[l.MarkupContent].getValue
          }
          assert(hover("trim").contains("trim()"), clue(hover("trim")))
          assert(hover("Connection").contains("Connection"), clue(hover("Connection")))
        }
        finally pc.shutdown()
      }
      finally {
        ctx.close()
        os.remove.all(workDir)
      }
    }
  }
}
