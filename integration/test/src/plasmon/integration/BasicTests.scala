package plasmon.integration

import com.eed3si9n.expecty.Expecty.expect
import io.github.alexarchambault.testutil.TestUtil.*
import org.eclipse.{lsp4j => l}
import plasmon.integration.TestUtil.*

import java.io.OutputStream
import java.net.URI
import java.nio.file.Paths
import java.util.concurrent.{CompletableFuture, CountDownLatch, TimeUnit}

import scala.jdk.CollectionConverters.*
import scala.language.reflectiveCalls

class BasicTests extends PlasmonSuite {

  private lazy val (defaultScalaVersion, defaultServerOpt) =
    if (disableScala2Pc)
      (scala213Compat, compatServerOpt)
    else
      (scala213, Nil)

  private lazy val scalaVersions = {
    val scala213Values =
      if (disableScala2Pc) Nil
      else Seq((scala213, Nil))
    scala213Values ++ Seq(
      (scala213Compat, compatServerOpt)
    )
  }

  private def fileContentFor(jvm: String, scalaVersionOpt: Option[String]): String = {
    val scalaVersion = scalaVersionOpt.getOrElse(defaultScalaVersion.value)
    if (scalaVersion.startsWith("2."))
      s"""//> using scala "$scalaVersion"
         |//> using jvm "$jvm"
         |//> using lib "com.lihaoyi::os-lib:0.9.1"
         |
         |import sca<1>la.colle<2>ction.mut<3>able.List<4>Buffer
         |import scala.collection.mut<46>able
         |object B<5>ar {
         |  sca<6>la.colle<7>ction.m<8>utable.ListBuf<9>fer
         |  Syst<10>em.er<11>r.pri<12>ntln("")
         |  Runtim<13>e.g<14>etRuntime.addSh<15>utdownHook(???)
         |  o<16>s.Pa<17>th
         |  o<18>s.p<19>wd
         |}
         |
         |object Thi<20>ng {
         |  import sc<21>ala.jd<22>k.Col<23>lectionConverters._
         |  Th<24>read.cur<25>rentThread().get<26>ContextClassLoader().get<27>Resources("").as<28>Scala.to<29>Vector
         |    .ma<30>p(_.to<31>URI.to<32>ASCIIString)
         |  Runt<33>ime.get<34>Runtime().avai<35>lableProcessors()
         |}
         |
         |object F<36>oo {
         |  def f<37>oo = {
         |    ListBu<38>ffer.em<39>pty[St<40>ring]
         |    Sys<41>tem.e<42>rr.pri<43>ntln("")
         |  }
         |  Syst<44>em.er<45>r
         |}
         |""".stripMargin
    else
      s"""//> using scala "$scalaVersion"
         |//> using jvm "$jvm"
         |//> using lib "com.lihaoyi::os-lib:0.9.1"
         |
         |import sca<1>la.colle<2>ction.mut<3>able.List<4>Buffer
         |import scala.collection.mut<46>able
         |object B<5>ar:
         |  sca<6>la.colle<7>ction.m<8>utable.ListBuf<9>fer
         |  Syst<10>em.er<11>r.pri<12>ntln("")
         |  Runtim<13>e.g<14>etRuntime.addSh<15>utdownHook(???)
         |  o<16>s.Pa<17>th
         |  o<18>s.p<19>wd
         |
         |
         |object Thi<20>ng:
         |  import sc<21>ala.jd<22>k.Col<23>lectionConverters._
         |  Th<24>read.cur<25>rentThread().get<26>ContextClassLoader().get<27>Resources("").as<28>Scala.to<29>Vector
         |    .ma<30>p(_.to<31>URI.to<32>ASCIIString)
         |  Runt<33>ime.get<34>Runtime().avai<35>lableProcessors()
         |
         |
         |object F<36>oo:
         |  def f<37>oo =
         |    ListBu<38>ffer.em<39>pty[St<40>ring]
         |    Sys<41>tem.e<42>rr.pri<43>ntln("")
         |
         |  Syst<44>em.er<45>r
         |
         |""".stripMargin
  }

  for (
    (scalaVersionOpt, serverOpt, buildTool, jvm, testNameSuffix) <- scalaVersionBuildToolJvmValues
  )
    test("test" + testNameSuffix) {
      mainTest(scalaVersionOpt, buildTool, jvm, serverOpt)
    }

  for ((scalaVersion, serverOpt) <- scalaVersions)
    test(s"test Scala CLI Scala ${scalaVersion.label} Java ${jvmValues.head.label} twice") {
      mainTest(
        Some(scalaVersion),
        SingleModuleBuildTool.ScalaCli,
        jvmValues.head,
        serverOpt,
        count = 2
      )
    }

  private def mainTest(
    scalaVersionOpt: Option[Labelled[String]],
    buildTool: SingleModuleBuildTool,
    jvm: Labelled[String],
    serverOpt: Seq[String],
    count: Int = 1
  ): Unit = {
    val header = (
      scalaVersionOpt.map(_.value).map(sv => s"""//> using scala "$sv"""") ++
        Seq(s"""//> using jvm "${jvm.value}"""")
    ).mkString(System.lineSeparator())
    val (actualPath, files) = buildTool.singleModule(
      "test-mod",
      Map(
        os.sub / "Foo.scala" -> fileContentFor(jvm.value, scalaVersionOpt.map(_.value)),
        os.sub / "SigHelp.scala" ->
          s"""$header
             |object SigHelp {
             |  List(<1>)
             |}
             |""".stripMargin,
        os.sub / "CodeLensStuff.scala" ->
          s"""
             |object CodeLensStuff {
             |  val a: Iterator[String] =
             |    new Iterator[String] {
             |      def <1>hasNext<2> = true
             |      def <3>next<4>(): String = "a"
             |    }
             |}
             |""".stripMargin,
        os.sub / "foo/Definitions.scala" ->
          """package foo
            |
            |object Definitions {
            |  def count = 2
            |}
            |""".stripMargin,
        os.sub / "foo/Foo.scala" ->
          s"""package foo
             |
             |object Foo {
             |  def apply(): Int = Defin<1>itions.cou<2>nt + 1
             |}
             |""".stripMargin
      )
    )
    val updated = new CountDownLatch(1)
    val languageClient = new MockLanguageClient {
      var osOpt = Option.empty[OutputStream]
      override def applyEdit(applyEditParams: l.ApplyWorkspaceEditParams)
        : CompletableFuture[l.ApplyWorkspaceEditResponse] = {
        osOpt.getOrElse(System.err).pprint(applyEditParams)
        val fromChanges = applyEditParams.getEdit.getChanges.asScala
        val fromDocumentChanges = applyEditParams
          .getEdit
          .getDocumentChanges
          .asScala
          .filter(_.isLeft)
          .map(_.getLeft)
          .map(e => (e.getTextDocument.getUri, e.getEdits))
        for ((uri, edits) <- fromChanges ++ fromDocumentChanges) {
          val path           = os.Path(Paths.get(new URI(uri)))
          val currentContent = os.read(path)
          var updatedContent = currentContent
          for (
            edit <- edits.asScala.toVector.sortBy(e =>
              (e.getRange.getStart.getLine, e.getRange.getStart.getCharacter)
            ).reverse
          ) {
            // we only support edits with a very specific shape here
            assert(updatedContent.isEmpty)
            assert(edit.getRange.getStart.getLine == 0)
            assert(edit.getRange.getStart.getCharacter == 0)
            assert(edit.getRange.getEnd.getLine == 0)
            assert(edit.getRange.getEnd.getCharacter == 0)
            updatedContent = edit.getNewText
          }
          os.write.over(path, updatedContent)
          updated.countDown()
        }
        CompletableFuture.completedFuture(new l.ApplyWorkspaceEditResponse(true))
      }
    }
    val clientCapabilities = new l.ClientCapabilities
    clientCapabilities.setWorkspace {
      val cap = new l.WorkspaceClientCapabilities
      cap.setApplyEdit(true)
      cap
    }
    withWorkspaceServerPositionsCount(
      client = languageClient,
      clientCapabilities = clientCapabilities,
      extraServerOpts = Seq("--jvm", jvm.value, "--import-persisted-targets=false") ++ serverOpt,
      count = count,
      timeout = Some(buildTool.defaultTimeout)
    )(files: _*) {
      (workspace, server, positions, osOpt, runCount) =>
        languageClient.osOpt = osOpt

        buildTool.setup(workspace, osOpt, readOnlyToplevelSymbolsCache = runCount > 0)

        val mainSourceFile = actualPath(os.sub / "Foo.scala")

        def hoverAt(pos: Int): Unit =
          checkTextFixture(
            fixtureDir / "plasmon/integration/single-file-tests/hover" / buildTool.id /
              s"scala-${scalaVersionOpt.map(_.label).getOrElse("default")}" / s"jvm-${jvm.label}" / s"pos-$pos.txt",
            hoverMarkdown(
              server,
              workspace / mainSourceFile,
              positions.lspPos(mainSourceFile, pos)
            ),
            osOpt
          )

        for (i <- (1 to 15) ++ (20 to 46)) {
          osOpt.getOrElse(System.err).write(s"Hover $i${System.lineSeparator()}".getBytes)
          hoverAt(i)
        }

        def goToDefAt(pos: Int): Unit =
          checkJsoniterFixture(
            fixtureDir / "plasmon/integration/single-file-tests/definition" / buildTool.id /
              s"scala-${scalaVersionOpt.map(_.label).getOrElse("default")}" / s"jvm-${jvm.label}" / s"definition-$pos.txt",
            goToDef(
              server,
              workspace,
              workspace / mainSourceFile,
              positions.lspPos(mainSourceFile, pos)
            ),
            osOpt
          )

        val outputStream = osOpt.getOrElse(System.err)
        val nl           = "\n"
        for (i <- Seq(4) ++ (9 to 15) ++ (23 to 35)) {
          outputStream.flush()
          outputStream.write(s"Definition $i$nl".getBytes)
          outputStream.flush()
          goToDefAt(i)
        }

        def goToDefInDefAt(
          pos: Either[(String, l.Position), Int],
          actualSourceFile: os.SubPath = mainSourceFile
        ): DefinitionResult = {
          val res = goToDef(
            server,
            workspace,
            workspace / actualSourceFile,
            pos match {
              case Left((_, pos0)) => pos0
              case Right(pos0)     => positions.lspPos(actualSourceFile, pos0)
            }
          )
          checkJsoniterFixture(
            fixtureDir / "plasmon/integration/single-file-tests/definition-in-dependencies" / buildTool.id /
              s"scala-${scalaVersionOpt.map(_.label).getOrElse("default")}" / s"jvm-${jvm.label}" / pos.left.map(
                _._1
              ).map(pos0 => s"definition-$pos0.txt").merge,
            res,
            osOpt
          )
          res
        }

        val goToDefInDefPrintlnRes = goToDefInDefAt(Right(12))
        goToDefInDefAt(
          Left((
            "println-27.txt",
            new l.Position(goToDefInDefPrintlnRes.line, goToDefInDefPrintlnRes.colAverage)
          )),
          actualSourceFile = os.SubPath(goToDefInDefPrintlnRes.path)
        )

        val signatureHelpSourceFile = actualPath(os.sub / "SigHelp.scala")
        val signatureHelp = server
          .getTextDocumentService
          .signatureHelp(
            new l.SignatureHelpParams(
              new l.TextDocumentIdentifier(
                (workspace / signatureHelpSourceFile).toNIO.toUri.toASCIIString
              ),
              positions.lspPos(signatureHelpSourceFile, 1)
            )
          )
          .get()

        checkGsonFixture(
          fixtureDir / "plasmon/integration/single-file-tests/signature-help" / buildTool.id /
            s"scala-${scalaVersionOpt.map(_.label).getOrElse("default")}" / s"jvm-${jvm.label}" / (signatureHelpSourceFile / os.up) / s"${signatureHelpSourceFile.last}.lenses",
          signatureHelp,
          osOpt
        )

        for (scalaVersion <- scalaVersionOpt) {
          val lensesSourceFile = actualPath(os.sub / "CodeLensStuff.scala")
          val lenses = server
            .getTextDocumentService
            .codeLens(
              new l.CodeLensParams(
                new l.TextDocumentIdentifier(
                  (workspace / lensesSourceFile).toNIO.toUri.toASCIIString
                )
              )
            )
            .get()

          checkGsonFixture(
            fixtureDir / "plasmon/integration/single-file-tests/code-lens-go-to-parent" / buildTool.id /
              s"scala-${scalaVersion.label}" / s"jvm-${jvm.label}" / (lensesSourceFile / os.up) / s"${lensesSourceFile.last}.lenses",
            lenses,
            osOpt,
            replaceAll = standardReplacements(workspace),
            roundTrip = true
          )

          if (count == 1) {
            val packageInNewFileSourceFile = actualPath(os.sub / "foo/Foo.scala")
            val newSourceFile              = packageInNewFileSourceFile / os.up / "bar/Bar.scala"
            os.write(workspace / newSourceFile, Array.emptyByteArray, createFolders = true)
            server.getTextDocumentService.didOpen(
              new l.DidOpenTextDocumentParams(
                new l.TextDocumentItem(
                  (workspace / newSourceFile).toNIO.toUri.toASCIIString,
                  "scala",
                  0,
                  ""
                )
              )
            )
            val appliedEdit = updated.await(5L, TimeUnit.SECONDS)
            expect(appliedEdit)
            val content = os.read(workspace / newSourceFile)
            expect(content.startsWith("package foo.bar"))

            buildTool.compile(workspace, osOpt)
            TestUtil.runServerCommand(workspace, osOpt)("index")
            TestUtil.runServerCommand(workspace, osOpt)("index", "--await")
            buildTool.compile(workspace, osOpt)
          }
        }

        val sameModuleGoToDefPath = actualPath(os.sub / "foo/Foo.scala")

        val sameModuleGoToDefDir = fixtureDir / "plasmon/integration/single-module/go-to-definition" /
          buildTool.id / s"scala-${scalaVersionOpt.map(_.label).getOrElse("default")}" / s"jvm-${jvm.label}"
        checkJsoniterFixture(
          sameModuleGoToDefDir / "obj-definition.json",
          goToDef(
            server,
            workspace,
            workspace / sameModuleGoToDefPath,
            positions.lspPos(sameModuleGoToDefPath, 1)
          ),
          osOpt
        )
        checkJsoniterFixture(
          sameModuleGoToDefDir / "method-definition.json",
          goToDef(
            server,
            workspace,
            workspace / sameModuleGoToDefPath,
            positions.lspPos(sameModuleGoToDefPath, 2)
          ),
          osOpt
        )

        // TODO Strip margin helper
        // TODO Pick up serialized build server and targets
        // TODO Status for individual files
        // TODO Index command
        // TODO Find local references
        // TODO Find references
        // TODO Handles didFocus notification
        // TODO Semanticdb in status bar
    }
  }
}
