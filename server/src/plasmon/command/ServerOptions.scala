package plasmon.command

import caseapp.{HelpMessage, Hidden, Name, ValueDescription}
import caseapp.core.help.Help
import caseapp.core.parser.Parser

// format: off
final case class ServerOptions(
  logToStderr: Option[Boolean] = None,
  logJsonrpcInput: Option[Boolean] = None,
  socket: Option[String] = None,
  javaHome: Option[String] = None,
  bloopJavaHome: Option[String] = None,
  jvm: Option[String] = None,
  bloopJvm: Option[String] = None,
  @HelpMessage("Speak LSP over stdin / stdout (default: true). With --lsp=false the server is driven by `plasmon` commands alone: it initializes itself on the working directory and stays up until `plasmon exit`")
    lsp: Boolean = true,
  @HelpMessage("Initialize on the working directory at start-up, rather than waiting for an LSP client to say what to work on (default: true when --lsp=false)")
    autoInit: Option[Boolean] = None,
  @HelpMessage("Stop when the process that started this one does. Useful with --lsp=false, where no client going away can be noticed")
    exitWithParentProc: Boolean = false,
  @Name("heartbeat")
    heartBeat: Option[String] = None,
  workingDir: Option[String] = None,
  importPersistedTargets: Boolean = true,
  @Hidden
    ignoreBuildTargetDidChange: Boolean = false,
  scalaCli: Option[String] = None,
  bestEffort: Option[Boolean] = None,
  suspendWatcher: Boolean = true,
  @HelpMessage("Handle Scala 2 using the Scala 3 PC, rather than with the Scala 2 PC, run in a process of its own (default: false)")
    scala2Compat: Option[Boolean] = None,
  @HelpMessage("Have presentation compilers read the workspace's own modules from their sources, rather than from the class directories the build tool reports: nothing needs to be built for a module to see the ones it depends on (default: false)")
    sourcepath: Option[Boolean] = None,
  @HelpMessage("Start in automatic mode, where editors get a build tool and a module loaded for each file they focus, rather than having users pick them (--auto=false: start in manual mode). Persisted under .plasmon/, like what is loaded (default: the mode the server in this directory was last left in, else manual)")
    auto: Option[Boolean] = None,
  @HelpMessage("Connect on start-up to the BSP server listening on this UNIX domain socket, and add it as a build tool. Can be repeated. Such build tools aren't persisted under .plasmon/: pass --bsp again on the next start")
  @ValueDescription("/path/to/bsp/socket")
    bsp: List[String] = Nil,
  @Hidden
  @HelpMessage("How sources are handed to the compiler with --sourcepath: logical (default, files listed one by one, packages read from package clauses) or classic (source roots laid out by package)")
    sourcepathMode: Option[String] = None
)
// format: on

object ServerOptions {
  implicit lazy val parser: Parser[ServerOptions] = Parser.derive
  implicit lazy val help: Help[ServerOptions]     = Help.derive
}
