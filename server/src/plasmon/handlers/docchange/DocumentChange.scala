package plasmon.handlers.docchange

import org.eclipse.lsp4j as l
import plasmon.Server
import plasmon.PlasmonEnrichments.*
import plasmon.jsonrpc.{Handlers, NotificationHandler}

import scala.concurrent.Future
import scala.jdk.CollectionConverters.*
import scala.meta.internal.mtags.SourcePath
import scala.util.{Failure, Success}
import plasmon.ide.PackageProvider

object DocumentChange {

  private def didOpenHandler(server: Server) =
    NotificationHandler.of[l.DidOpenTextDocumentParams]("textDocument/didOpen") { (params, _) =>
      val path           = params.getTextDocument.getUri.osPathFromUri
      val currentContent = params.getTextDocument.getText
      val contentVersion = params.getTextDocument.getVersion

      server.editorState.updateFocusedDocument(path, os.read(path), currentContent)
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

      SourcePath.withContext { implicit ctx =>
        new PackageProvider(server.bspData, server.editorState.trees)
          .workspaceEdit(
            path,
            currentContent,
            Some(contentVersion)
          )
          .map(new l.ApplyWorkspaceEditParams(_))
          .foreach(server.languageClient.applyEdit)
      }
    }

  private def didChangeHandler(server: Server) =
    NotificationHandler.of[l.DidChangeTextDocumentParams]("textDocument/didChange") { (params, _) =>
      for (change <- params.getContentChanges.asScala) {
        val path           = params.getTextDocument.getUri.osPathFromUri
        val updatedContent = change.getText

        server.editorState.buffers.put(path, updatedContent)

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
      }
    }

  private def didCloseHandler(server: Server) =
    NotificationHandler.of[l.DidCloseTextDocumentParams]("textDocument/didClose") { (params, _) =>
      val path = params.getTextDocument.getUri.osPathFromUri

      server.editorState.closed(path)
      server.presentationCompilers.didClose(path)
      for {
        targetId    <- server.bspData.inverseSources0(path).merge
        buildClient <- server.bspData.buildClientOf(targetId)
      }
        buildClient.onClose(targetId.module, path)
    }

  private def didSaveHandler(server: Server) =
    NotificationHandler.of[l.DidSaveTextDocumentParams]("textDocument/didSave") { (params, _) =>
      val path = params.getTextDocument.getUri.osPathFromUri

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
