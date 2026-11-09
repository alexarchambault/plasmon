package plasmon.servercommand

import ch.epfl.scala.bsp4j as b
import plasmon.Server

import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.util.{Failure, Success, Try}

/** Compiling a module right after the command line loaded it.
  *
  * A presentation compiler answers from class directories: those of the module's dependencies, and
  * the module's own for the files around the one being looked at. Until something builds them,
  * hovers and definitions on anything defined elsewhere come back empty, or worse, plausibly wrong -
  * a `Module` from another file resolving to `java.lang.Module`.
  *
  * In an editor, that build comes for free: the files it has open get compiled once indexing is
  * done (see `IndexerActor`), and again on every save. From a terminal nothing is ever opened -
  * `lsp hover` and friends just ask - so a module loaded with `module load` or `--auto` would stay
  * unbuilt until someone thought of running `plasmon compile`. The file given to those commands is
  * the closest thing to an opened file there is, so its module is compiled there and then, and the
  * command returns once it is built - the next request gets right answers rather than racing the
  * build.
  *
  * A build that fails is not a failed load: the module is loaded, and what did compile is there to
  * answer from. It is said on `log`, so that partial answers that follow aren't a mystery.
  */
object CompileAfterLoad {

  def apply(
    server: Server,
    moduleUri: String,
    progress: String => Unit,
    log: String => Unit
  ): Unit = {
    progress(s"Compiling module $moduleUri")
    val f = ProjectOps.compileModule(server, new b.BuildTargetIdentifier(moduleUri))
    Try(Await.result(f, Duration.Inf)) match {
      case Success(res) =>
        res.getStatusCode match {
          case b.StatusCode.OK =>
            progress(s"Compiled module $moduleUri")
          case b.StatusCode.ERROR =>
            log(
              s"Warning: compilation of module $moduleUri failed, " +
                "hovers and definitions may be incomplete (see `plasmon diagnostics <file>`)"
            )
          case b.StatusCode.CANCELLED =>
            log(s"Warning: compilation of module $moduleUri cancelled")
        }
      case Failure(ex) =>
        log(s"Warning: error compiling module $moduleUri: $ex")
    }
  }
}
