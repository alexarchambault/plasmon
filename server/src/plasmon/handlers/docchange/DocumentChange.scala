package plasmon.handlers.docchange

import org.eclipse.lsp4j as l
import plasmon.Server
import plasmon.PlasmonEnrichments.*
import plasmon.ide.PackageProvider
import plasmon.jsonrpc.{Handlers, NotificationHandler}

import scala.concurrent.Future
import scala.jdk.CollectionConverters.*
import scala.meta.internal.mtags.SourcePath
import scala.util.{Failure, Success}

object DocumentChange {

  /** What the server asks the editor to do in reaction to a document event.
    *
    * Over LSP these go out on their own as `workspace/applyEdit` requests, since the notification
    * that caused them cannot answer anything. Returning them instead is what lets a caller that
    * isn't an editor - `plasmon lsp did-open` and friends - see them, and carry them out itself.
    */
  final case class Reactions(edits: Seq[l.WorkspaceEdit] = Nil)

  /** Everything the server does when a document is opened in the editor.
    *
    * `content` is what the editor holds, which for a file it has just created differs from what is
    * on disk. The work this kicks off - loading a presentation compiler, compiling, computing
    * synthetics - is left running in the background, the way the LSP notification leaves it.
    */
  def didOpen(
    server: Server,
    path: os.Path,
    content: String,
    version: Int
  ): Reactions = {

    server.editorState.updateFocusedDocument(path, os.read(path), content)
    server.refreshStatus()

    def interactive =
      server.bspData.inverseSources(path).foreach { target =>
        server.interactiveSemanticdbs.textDocument(path, target.module)
      }
    // }
    // We need both parser and semanticdb for synthetic decorations
    val publishSynthetics = {
      implicit val ec = server.pools.documentChangeEc
      val checks = for {
        targetId    <- server.bspData.inverseSources0(path).merge
        buildClient <- server.bspData.buildClientOf(targetId).toSeq
        dialect     <- server.bspData.getDialect(path.ext, path.isMill, targetId).toSeq
      } yield server.parserQueue.check(targetId.module, path, buildClient, dialect)
      val f = for {
        _ <- Future.sequence(checks ++ Seq(Future(interactive)))
        _ <- Future.sequence(
          List[Future[?]](
            // publishSynthetics0(path, server, cancelTokensEces, dummyEc)
            // testProvider.didOpen(path),
          )
        )
      } yield ()
      f.onComplete {
        case Success(()) =>
        case Failure(ex) =>
          scribe.warn(s"Error while publishing synthetics upon opening $path", ex)
      }
      f
    }

    if (!path.isDependencySource(server.workspace())) {
      implicit val ec = server.pools.documentChangeEc
      Future
        .sequence(
          List(
            server.presentationCompilers.load(List(path)),
            publishSynthetics
          ) ++
            server.compilations.compileFile(path).toSeq
        )
        .onComplete {
          case Success(_) =>
          case Failure(ex) =>
            scribe.error(s"Error loading $path", ex)
        }
    }

    val packageEdit = SourcePath.withContext { implicit ctx =>
      new PackageProvider(server.bspData, server.editorState.trees)
        .workspaceEdit(
          path,
          content,
          Some(version)
        )
    }

    Reactions(packageEdit.toSeq)
  }

  /** Everything the server does when the editor's copy of a document changes. */
  def didChange(
    server: Server,
    path: os.Path,
    content: String
  ): Reactions = {

    server.editorState.buffers.put(path, content)

    server.refreshStatus()
    for {
      targetId    <- server.bspData.inverseSources0(path).merge
      buildClient <- server.bspData.buildClientOf(targetId)
    } {
      buildClient.diagDidChange(path)

      for (dialect <- server.bspData.getDialect(path.ext, path.isMill, targetId))
        server.parserQueue
          .check(targetId.module, path, buildClient, dialect)
          .onComplete {
            case Success(()) =>
            case Failure(ex) => scribe.error(s"Error parsing $path", ex)
          }(using server.pools.documentChangeEc)
    }
    //   .flatMap(_ => publishSynthetics0(path, server, cancelTokensEces, dummyEc))(using
    //     server.pools.documentChangeEc
    //   )
    //   .ignoreValue(using server.pools.documentChangeEc)

    Reactions()
  }

  /** Everything the server does when a document is closed in the editor. */
  def didClose(server: Server, path: os.Path): Reactions = {
    server.editorState.closed(path)
    server.presentationCompilers.didClose(path)
    for {
      targetId    <- server.bspData.inverseSources0(path).merge
      buildClient <- server.bspData.buildClientOf(targetId)
    }
      buildClient.onClose(targetId.module, path)
    Reactions()
  }

  /** Everything the server does when a document is saved in the editor. */
  def didSave(server: Server, path: os.Path): Reactions = {
    server.refreshStatus()
    // savedFiles.add(path)
    // read file from disk, we only remove files from buffers on didClose.
    server.editorState.buffers.put(path, os.read(path))
    server.reindexSource(path)
    implicit val ec = server.pools.documentChangeEc
    val checks = for {
      targetId    <- server.bspData.inverseSources0(path).merge
      buildClient <- server.bspData.buildClientOf(targetId).toSeq
      dialect     <- server.bspData.getDialect(path.ext, path.isMill, targetId).toSeq
    } yield server.parserQueue.check(targetId.module, path, buildClient, dialect)
    Future
      .sequence(
        checks ++ List(
          server.compilations.compileFiles(Seq(path)),
          // onBuildChanged(paths).ignoreValue,
          // Future.sequence(paths.map(onBuildToolAdded)),
          server.bspData
            .inverseSources(path)
            .map { targetId =>
              Future(server.interactiveSemanticdbs.textDocument(
                path,
                targetId.module
              ))(using server.pools.documentChangeEc)
            }
            .getOrElse(Future.successful(()))
        )
        // renameProvider.runSave(),
        // ++ // if we fixed the script, we might need to retry connection
        // maybeImportScript(
        //   path
        // )
      )
      .ignoreValue
      .onComplete {
        case Success(()) =>
        case Failure(ex) =>
          scribe.error(s"Error handling save of $path", ex)
      }
    Reactions()
  }

  private def sendToClient(server: Server, reactions: Reactions): Unit =
    for (edit <- reactions.edits)
      server.languageClient.applyEdit(new l.ApplyWorkspaceEditParams(edit))

  private def didOpenHandler(server: Server) =
    NotificationHandler.of[l.DidOpenTextDocumentParams]("textDocument/didOpen") { (params, _) =>
      sendToClient(
        server,
        didOpen(
          server,
          params.getTextDocument.getUri.osPathFromUri,
          params.getTextDocument.getText,
          params.getTextDocument.getVersion
        )
      )
    }

  private def didChangeHandler(server: Server) =
    NotificationHandler.of[l.DidChangeTextDocumentParams]("textDocument/didChange") { (params, _) =>
      for (change <- params.getContentChanges.asScala)
        sendToClient(
          server,
          didChange(
            server,
            params.getTextDocument.getUri.osPathFromUri,
            change.getText
          )
        )
    }

  private def didCloseHandler(server: Server) =
    NotificationHandler.of[l.DidCloseTextDocumentParams]("textDocument/didClose") { (params, _) =>
      sendToClient(server, didClose(server, params.getTextDocument.getUri.osPathFromUri))
    }

  private def didSaveHandler(server: Server) =
    NotificationHandler.of[l.DidSaveTextDocumentParams]("textDocument/didSave") { (params, _) =>
      sendToClient(server, didSave(server, params.getTextDocument.getUri.osPathFromUri))
    }

  def handlers(server: Server): Handlers =
    Handlers(
      Seq(
        didOpenHandler(server),
        didChangeHandler(server),
        didCloseHandler(server),
        didSaveHandler(server)
      ),
      Nil,
      Nil
    )
}
