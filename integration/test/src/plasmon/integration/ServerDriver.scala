package plasmon.integration

import com.google.gson.Gson
import org.eclipse.lsp4j as l
import org.eclipse.lsp4j.services.LanguageServer

import java.io.OutputStream

import scala.concurrent.duration.FiniteDuration
import scala.jdk.CollectionConverters.*

/** Everything a test asks a running plasmon server to do.
  *
  * There are two implementations, one per [[TestMode]]: [[ServerDriver.Lsp]] sends LSP requests the
  * way the editor extension does, [[ServerDriver.Cli]] runs `plasmon <command>` in the workspace
  * the way a terminal does. Tests are written against this interface alone, so the same test body -
  * checked against the same fixtures - covers both entry points.
  *
  * That includes telling the server about the editor's copy of a file: `did-open` and `did-change`
  * exist as commands too, and hand back the edits the server asks for in reaction rather than
  * sending them off to a client that isn't there.
  */
trait ServerDriver {
  def mode: TestMode
  def workspace: os.Path

  /** The LSP connection the harness holds open.
    *
    * The server under test is an LSP server whatever the mode - it needs an `initialize` to know
    * its workspace at all - so the connection is always there. Only the tests that are about the
    * protocol itself (shutting the server down) should reach for it; everything else goes through
    * the methods below, so that it runs both ways.
    */
  def lsp: LanguageServer

  /** Starts the build tool `toolId`, discovered under `discoverId` from `currentFile`. */
  def loadBuildTool(discoverId: String, toolId: String, currentFile: os.Path): Unit

  /** Loads every module of every loaded build tool, and indexes them. */
  def loadAllModules(toplevelCacheOnly: Boolean): Unit

  /** Loads the module `currentFile` belongs to. */
  def loadModuleOf(currentFile: os.Path): Unit

  /** Compiles the module `currentFile` belongs to, whether or not compilation succeeds. */
  def compile(currentFile: os.Path): Unit

  /** Re-indexes the loaded modules. */
  def index(): Unit

  /** Tells the server a file was opened in the editor, holding `content`.
    *
    * Returns the workspace edits the server asks for in reaction - a package clause for a new,
    * empty file, typically - once they have been applied to disk. `expectedEdits` is how many to
    * wait for: over LSP they come back on their own as `workspace/applyEdit` requests, so with 0
    * there is nothing to wait for.
    */
  def didOpen(
    path: os.Path,
    version: Int,
    content: String,
    expectedEdits: Int = 0,
    timeout: FiniteDuration = TestUtil.baseTimeout
  ): Seq[l.WorkspaceEdit]

  /** Tells the server the editor's copy of a file now holds `content`. */
  def didChange(path: os.Path, version: Int, content: String): Unit

  /** `null` when there is nothing to show. */
  def hover(path: os.Path, pos: l.Position): l.Hover
  def definition(path: os.Path, pos: l.Position): Seq[l.Location]
  def completion(path: os.Path, pos: l.Position): l.CompletionList
  def signatureHelp(path: os.Path, pos: l.Position): l.SignatureHelp
  def codeLens(path: os.Path): Seq[l.CodeLens]

  /** Waits for a build to report diagnostics for `path`, as the client would see them published. */
  def awaitDiagnostics(path: os.Path, timeout: FiniteDuration): l.PublishDiagnosticsParams
}

object ServerDriver {

  def apply(
    mode: TestMode,
    lsp: LanguageServer,
    workspace: os.Path,
    client: l.services.LanguageClient,
    errOpt: Option[OutputStream]
  ): ServerDriver =
    mode match {
      case TestMode.Lsp => Lsp(lsp, workspace, client)
      case TestMode.Cli => Cli(lsp, workspace, errOpt)
    }

  private def identifier(path: os.Path): l.TextDocumentIdentifier =
    new l.TextDocumentIdentifier(path.toNIO.toUri.toASCIIString)

  private def languageIdOf(path: os.Path): String =
    if (path.last.endsWith(".java")) "java" else "scala"

  final case class Lsp(
    lsp: LanguageServer,
    workspace: os.Path,
    client: l.services.LanguageClient
  ) extends ServerDriver {

    def mode = TestMode.Lsp

    private def executeCommand(command: String, arguments: Object*): Object = {
      val params = new l.ExecuteCommandParams
      params.setCommand(command)
      params.setArguments(arguments.toList.asJava)
      lsp.getWorkspaceService.executeCommand(params).get()
    }

    def loadBuildTool(discoverId: String, toolId: String, currentFile: os.Path): Unit = {
      val res = executeCommand(
        "plasmon/loadBuildTool",
        discoverId,
        toolId,
        currentFile.toNIO.toUri.toASCIIString
      )
      val obj     = new Gson().toJsonTree(res).getAsJsonObject
      val success = obj.get("success").getAsBoolean
      if (!success) {
        val error = Option(obj.get("error")).filter(!_.isJsonNull).map(_.getAsString)
        sys.error(s"Error loading build tool $toolId / $discoverId: ${error.getOrElse("")}")
      }
    }

    def loadAllModules(toplevelCacheOnly: Boolean): Unit = {
      executeCommand("plasmon/loadAllModules", Boolean.box(toplevelCacheOnly))
    }

    def loadModuleOf(currentFile: os.Path): Unit = {
      val currentFileUri = currentFile.toNIO.toUri.toASCIIString
      val res            = executeCommand("plasmon/listModulesOf", currentFileUri)
      val modules = new Gson()
        .toJsonTree(res)
        .getAsJsonArray
        .asScala
        .map(_.getAsJsonObject)
        .filter(_.has("uri"))
        .toVector
      val module = modules
        .find(obj => !obj.get("alreadyLoaded").getAsBoolean)
        .orElse(modules.headOption)
        .getOrElse(sys.error(s"No module found for $currentFile"))

      val loadRes = executeCommand(
        "plasmon/loadModule",
        module.get("workspace").getAsString,
        module.get("server").getAsString,
        module.get("uri").getAsString
      )
      val loadObj = new Gson().toJsonTree(loadRes).getAsJsonObject
      if (loadObj.has("error") && !loadObj.get("error").getAsString.isEmpty)
        sys.error(
          s"Error loading module ${module.get("label").getAsString}: " +
            loadObj.get("error").getAsString
        )
    }

    def compile(currentFile: os.Path): Unit = {
      executeCommand("plasmon/compile", currentFile.toNIO.toUri.toASCIIString)
    }

    def index(): Unit = {
      executeCommand("plasmon/index")
    }

    def didOpen(
      path: os.Path,
      version: Int,
      content: String,
      expectedEdits: Int,
      timeout: FiniteDuration
    ): Seq[l.WorkspaceEdit] = {
      val mockClientOpt = client match {
        case client0: MockLanguageClient => Some(client0)
        case _ =>
          if (expectedEdits > 0)
            sys.error(s"Cannot wait for workspace edits with language client $client")
          None
      }
      // Marked before the notification goes out: the edits come back on their own, and could
      // already have landed by the time we look
      val seenBefore = mockClientOpt.fold(0)(_.appliedEditCount)

      lsp.getTextDocumentService.didOpen(
        new l.DidOpenTextDocumentParams(
          new l.TextDocumentItem(
            path.toNIO.toUri.toASCIIString,
            languageIdOf(path),
            version,
            content
          )
        )
      )

      if (expectedEdits <= 0) Nil
      else {
        val edits = mockClientOpt.get.awaitAppliedEdits(seenBefore, expectedEdits, timeout)
        if (edits.length < expectedEdits)
          sys.error(
            s"Expected $expectedEdits workspace edit(s) after opening $path, got ${edits.length}"
          )
        edits
      }
    }

    def didChange(path: os.Path, version: Int, content: String): Unit =
      lsp.getTextDocumentService.didChange(
        new l.DidChangeTextDocumentParams(
          new l.VersionedTextDocumentIdentifier(path.toNIO.toUri.toASCIIString, version),
          List(new l.TextDocumentContentChangeEvent(content)).asJava
        )
      )

    def hover(path: os.Path, pos: l.Position): l.Hover =
      lsp.getTextDocumentService
        .hover(new l.HoverParams(identifier(path), pos))
        .get()

    def definition(path: os.Path, pos: l.Position): Seq[l.Location] = {
      val resp = lsp.getTextDocumentService
        .definition(new l.DefinitionParams(identifier(path), pos))
        .get()
      if (resp == null) Nil
      else {
        assert(resp.isLeft, s"Expected locations, got location links ($resp)")
        resp.getLeft.asScala.toSeq
      }
    }

    def completion(path: os.Path, pos: l.Position): l.CompletionList = {
      val resp = lsp.getTextDocumentService
        .completion(
          new l.CompletionParams(
            identifier(path),
            pos,
            new l.CompletionContext(l.CompletionTriggerKind.Invoked)
          )
        )
        .get()
      assert(resp != null, "Expected a completion response")
      assert(resp.isRight, s"Expected a completion list, got a list of items ($resp)")
      resp.getRight
    }

    def signatureHelp(path: os.Path, pos: l.Position): l.SignatureHelp =
      lsp.getTextDocumentService
        .signatureHelp(new l.SignatureHelpParams(identifier(path), pos))
        .get()

    def codeLens(path: os.Path): Seq[l.CodeLens] =
      Option(
        lsp.getTextDocumentService
          .codeLens(new l.CodeLensParams(identifier(path)))
          .get()
      ).toSeq.flatMap(_.asScala.toSeq)

    def awaitDiagnostics(path: os.Path, timeout: FiniteDuration): l.PublishDiagnosticsParams =
      client match {
        case client0: MockLanguageClient =>
          client0.awaitDiagnostics(path, timeout).getOrElse {
            sys.error(s"No diagnostics published for $path after $timeout")
          }
        case other =>
          sys.error(s"Cannot wait for diagnostics with language client $other")
      }
  }

  final case class Cli(
    // Carried for the harness, never used here: everything below goes through `plasmon <command>`
    lsp: LanguageServer,
    workspace: os.Path,
    errOpt: Option[OutputStream]
  ) extends ServerDriver {

    def mode = TestMode.Cli

    private def commandLine(command: Seq[os.Shellable]): String =
      "plasmon " + command.flatMap(_.value).mkString(" ")

    /** Failures come back as a bare exit code, so name the command that produced it - what went
      * wrong is in the server output the test log already carries.
      */
    private def failing[T](command: Seq[os.Shellable])(f: => T): T =
      try f
      catch {
        case e: os.SubprocessException =>
          throw new Exception(
            s"${commandLine(command)} failed (exit code ${e.result.exitCode})",
            e
          )
      }

    private def run(command: os.Shellable*): Unit =
      failing(command)(TestUtil.runServerCommand(workspace, errOpt)(command*))

    private def output(command: os.Shellable*): String =
      failing(command)(TestUtil.serverCommandOutput(workspace, errOpt)(command*))

    private def json[T](cls: Class[T])(command: os.Shellable*): T = {
      val out = output(command*)
      if (out.trim.isEmpty)
        sys.error(s"No JSON output from ${commandLine(command)}")
      new Gson().fromJson(out, cls)
    }

    private def position(pos: l.Position): Seq[os.Shellable] = Seq[os.Shellable](
      "--line",
      pos.getLine.toString,
      "--col",
      pos.getCharacter.toString
    )

    def loadBuildTool(discoverId: String, toolId: String, currentFile: os.Path): Unit =
      run("build-tool", "load", "--discover-id", discoverId, "--id", toolId, currentFile)

    def loadAllModules(toplevelCacheOnly: Boolean): Unit =
      run("module", "load-all", s"--toplevel-cache-only=$toplevelCacheOnly")

    def loadModuleOf(currentFile: os.Path): Unit =
      run("module", "load", currentFile)

    // Matches what plasmon/compile does: it reports a failed build, it doesn't raise it
    def compile(currentFile: os.Path): Unit =
      run("compile", "--fail-on-error=false", currentFile)

    def index(): Unit =
      run("index")

    // The server reads the editor's copy off a file rather than being handed it as an argument,
    // which is also how a terminal would pass an unsaved draft around
    private def withContentFile[T](path: os.Path, content: String)(f: os.Path => T): T = {
      val contentFile = os.temp(content, suffix = "-" + path.last)
      try f(contentFile)
      finally os.remove(contentFile)
    }

    def didOpen(
      path: os.Path,
      version: Int,
      content: String,
      expectedEdits: Int,
      timeout: FiniteDuration
    ): Seq[l.WorkspaceEdit] = {
      val edits = withContentFile(path, content) { contentFile =>
        json(classOf[Array[l.WorkspaceEdit]])(
          "lsp",
          "did-open",
          "--json",
          "--content-file",
          contentFile,
          "--version",
          version.toString,
          path
        ).toSeq
      }
      if (edits.length < expectedEdits)
        sys.error(
          s"Expected $expectedEdits workspace edit(s) after opening $path, got ${edits.length}"
        )
      edits
    }

    def didChange(path: os.Path, version: Int, content: String): Unit =
      withContentFile(path, content) { contentFile =>
        run("lsp", "did-change", "--content-file", contentFile, path)
      }

    def hover(path: os.Path, pos: l.Position): l.Hover =
      json(classOf[l.Hover])(Seq[os.Shellable]("lsp", "hover", "--json") ++ position(pos) :+ path*)

    def definition(path: os.Path, pos: l.Position): Seq[l.Location] =
      json(classOf[Array[l.Location]])(
        Seq[os.Shellable]("lsp", "definition", "--json") ++ position(pos) :+ path*
      ).toSeq

    def completion(path: os.Path, pos: l.Position): l.CompletionList =
      json(classOf[l.CompletionList])(
        Seq[os.Shellable]("lsp", "completion", "--json") ++ position(pos) :+ path*
      )

    def signatureHelp(path: os.Path, pos: l.Position): l.SignatureHelp =
      json(classOf[l.SignatureHelp])(
        Seq[os.Shellable]("lsp", "signature-help", "--json") ++ position(pos) :+ path*
      )

    def codeLens(path: os.Path): Seq[l.CodeLens] =
      json(classOf[Array[l.CodeLens]])("lsp", "code-lens", "--json", path).toSeq

    def awaitDiagnostics(path: os.Path, timeout: FiniteDuration): l.PublishDiagnosticsParams = {
      val deadline = System.currentTimeMillis() + timeout.toMillis
      def attempt(): Option[l.PublishDiagnosticsParams] = {
        // --adjust=false: the raw diagnostics the build reported, which is what gets published
        val out = output("diagnostics", "--json", "--adjust=false", path)
        Option(new Gson().fromJson(out, classOf[Array[l.Diagnostic]]))
          .filter(_.nonEmpty)
          .map { diagnostics =>
            new l.PublishDiagnosticsParams(
              path.toNIO.toUri.toASCIIString,
              diagnostics.toList.asJava
            )
          }
      }
      def helper(): l.PublishDiagnosticsParams =
        attempt() match {
          case Some(params) => params
          case None =>
            if (System.currentTimeMillis() >= deadline)
              sys.error(s"No diagnostics reported for $path after $timeout")
            Thread.sleep(500L)
            helper()
        }
      helper()
    }
  }
}
