package plasmon.command

import caseapp.core.help.Help
import caseapp.core.parser.Parser
import caseapp.{HelpMessage, Hidden}
import caseapp.Name

// format: off
final case class ServerOptions(
  logToStderr: Option[Boolean] = None,
  logJsonrpcInput: Option[Boolean] = None,
  socket: Option[String] = None,
  javaHome: Option[String] = None,
  bloopJavaHome: Option[String] = None,
  jvm: Option[String] = None,
  bloopJvm: Option[String] = None,
  autoInit: Option[Boolean] = None,
  @Name("heartbeat")
    heartBeat: Option[String] = None,
  workingDir: Option[String] = None,
  importPersistedTargets: Boolean = true,
  @Hidden
    ignoreBuildTargetDidChange: Boolean = false,
  scalaCli: Option[String] = None,
  bestEffort: Option[Boolean] = None,
  suspendWatcher: Boolean = true
)
// format: on

object ServerOptions {
  implicit lazy val parser: Parser[ServerOptions] = Parser.derive
  implicit lazy val help: Help[ServerOptions]     = Help.derive
}
