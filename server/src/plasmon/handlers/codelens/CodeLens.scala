package plasmon.handlers.codelens

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import org.eclipse.lsp4j as l
import plasmon.Server
import plasmon.PlasmonEnrichments.*
import plasmon.ide.ClientCommands
import plasmon.jsonrpc.{CommandHandler, Handlers, RequestHandler}
import plasmon.jsonrpc.CommandHandler.ParamsHelpers.*
import plasmon.semdb.Semanticdbs

import java.util.List as JList

import scala.concurrent.{ExecutionContext, Future}
import scala.jdk.CollectionConverters.*
import scala.meta.internal.mtags.{GlobalSymbolIndex, SourcePath}
import plasmon.ide.ClientCommands.WindowLocation

object CodeLens {

  private def findLenses(
    semdbCodeLensProviders: Seq[CodeLensProvider],
    semanticdbs: Semanticdbs,
    module: GlobalSymbolIndex.Module,
    path: os.Path,
    dummyEc: ExecutionContext
  ): Future[Seq[l.CodeLens]] =
    if (semdbCodeLensProviders.isEmpty) Future.successful(Nil)
    else
      semanticdbs
        .textDocument(path, module)
        .toOption
        .flatMap(_.documentIncludingStale)
        .map { textDocument =>
          semdbCodeLensProviders
            .filter(_.isEnabled)
            .map { provider =>
              provider.codeLenses(module, SourcePath.Standard(path.toNIO), textDocument)
            }
        }
        .map { semanticdbCodeLenses =>
          implicit val ec0: ExecutionContext = dummyEc
          Future.sequence(semanticdbCodeLenses).map(_.flatten)
        }
        .getOrElse(Future.successful(Nil))

  def handler(server: Server) =
    RequestHandler.of[l.CodeLensParams, JList[l.CodeLens]]("textDocument/codeLens") {

      val semdbCodeLensProviders = Seq[CodeLensProvider](
        new SuperMethodCodeLens(
          server.editorState.buffers,
          server.editorState.trees
        )(using server.pools.codeLensEc)
      )

      (params, _) =>
        val f = Future {
          val path = params.getTextDocument.getUri.osPathFromUri
          server.bspData.inverseSources(path) match {
            case Some(target) =>

              val mappedToLenses =
                server.bspData.mappedTo(target, path).toSeq.flatMap { mappedSource =>
                  val content = server.editorState.buffers.get(path).getOrElse(os.read(path))
                  val (lastLine, lastLineIdx) = {
                    val it = content.linesIterator.zip(content.linesWithSeparators).zipWithIndex
                    val ((lastLine, lastLineWithSep), lastLineIdx) =
                      if (it.hasNext)
                        it.foldLeft(it.next())((_, l) => l) // it.last()
                      else
                        (("", ""), 0)
                    if (lastLineWithSep.drop(lastLine.length).nonEmpty)
                      ("", lastLineIdx + 1)
                    else
                      (lastLine, lastLineIdx)
                  }
                  val cmd = ClientCommands.GotoLocation.toLsp {
                    val pos = new l.Position(mappedSource.toCompilerLine(0).getOrElse(0), 0)
                    WindowLocation(
                      mappedSource.compilerPath.toNIO.toUri.toASCIIString,
                      new l.Range(pos, pos)
                    )
                  }
                  cmd.setTitle("$(arrow-left) Go to compiled source")
                  Seq(
                    new l.CodeLens(
                      new l.Range(
                        new l.Position(lastLineIdx, 0),
                        new l.Position(lastLineIdx, lastLine.length)
                      ),
                      cmd,
                      null
                    )
                  )
                }

              val mappedFromLenses =
                server.bspData.mappedFrom(target, path).toSeq.flatMap { mappedSource =>
                  val content = server.editorState.buffers.get(path).getOrElse(os.read(path))
                  val lineIdx = mappedSource.toCompilerLine(0).getOrElse(0)
                  val firstLineContent = content.linesIterator.find(_ => true).getOrElse("")
                  val lineContent =
                    content.linesIterator.drop(lineIdx).find(_ => true).getOrElse("")
                  val cmd = ClientCommands.GotoLocation.toLsp {
                    WindowLocation(
                      mappedSource.userPath.toNIO.toUri.toASCIIString,
                      new l.Range(l.Position(0, 0), l.Position(0, 0))
                    )
                  }
                  cmd.setTitle("Go to original source $(arrow-right)")
                  Seq(
                    new l.CodeLens(
                      new l.Range(
                        new l.Position(0, 0),
                        new l.Position(0, firstLineContent.length)
                      ),
                      cmd,
                      null
                    ),
                    new l.CodeLens(
                      new l.Range(
                        new l.Position(lineIdx, 0),
                        new l.Position(lineIdx, lineContent.length)
                      ),
                      cmd,
                      null
                    )
                  )
                }

              val futureSemdbLenses = findLenses(
                semdbCodeLensProviders,
                server.semanticdbs,
                target.module,
                path,
                server.pools.dummyEc
              )

              futureSemdbLenses.map { semdbLenses =>
                val lenses = semdbLenses.toList.sortBy(_.getRange.getStart.getLine) ++
                  mappedToLenses ++
                  mappedFromLenses
                lenses.asJava
              }(using server.pools.codeLensEc)
            case None =>
              scribe.info(s"textDocument/codeLens: no build target found for $path")
              Future.successful(Nil.asJava)
          }
        }(using server.pools.requestsEces)

        val f0 = {
          implicit val ec = server.pools.requestsEces
          f.flatten
        }

        f0.asJava
    }

  final case class GotoCommandParams(symbol: String, module: String)
  object GotoCommandParams {
    implicit val codec: JsonValueCodec[GotoCommandParams] = JsonCodecMaker.make
  }
  def commandHandlers(server: Server) =
    Seq(
      CommandHandler.of("goto") { (params, logger) =>
        params.as[GsonValue[GotoCommandParams]]("goto") { params =>
          val module = GlobalSymbolIndex.Module.fromString(params.value.module)
          Future {
            val locations = SourcePath.withContext { implicit ctx =>
              server.patchedSymbolIndex
                .fromSymbol(module, params.value.symbol)
                .asScala
            }
            for (location <- locations.headOption)
              server.languageClient.executeClientCommand(
                ClientCommands.GotoLocation.toExecuteCommandParams(
                  ClientCommands.WindowLocation(location.getUri, location.getRange)
                )
              )
          }(using server.pools.definitionProviderEc).asJavaObject
        }(using ArgParser.gson[GotoCommandParams])
      }
    )

  def handlers(server: Server): Handlers =
    Handlers(Nil, Seq(handler(server)), commandHandlers(server))
}
