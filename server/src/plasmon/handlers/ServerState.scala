package plasmon.handlers

import ch.epfl.scala.{bsp4j => b}
import plasmon.Server
import plasmon.languageclient.PlasmonConfiguredLanguageClient
import plasmon.bsp.BuildTool
import plasmon.bsp.BspConnection
import pprint.PPrinter

object ServerState {

  private def connectionsState(
    connections: Seq[(BuildTool, Seq[BspConnection])]
  ): String = {
    connections
      .map {
        case (buildTool, buildToolConns) =>

      }

    ???
  }

  def state(server: Server): String = {
    s"""# Plasmon server state

### pendingShowMessage
${
        server.underlyingLanguageClient() match {
          case c: PlasmonConfiguredLanguageClient =>
            c.pendingShowMessage.get().toString
          case _ => ""
        }
      }

## JDK source path context
${server.jdkContext}

## JDK file manager
${server.javaFileManager}

## symbolIndex
${server.symbolIndex}

## symbolDocs
${server.symbolDocs}
"""

    server.debugJson
  }
}
