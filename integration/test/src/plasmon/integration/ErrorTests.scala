package plasmon.integration

import com.eed3si9n.expecty.Expecty.expect
import org.eclipse.lsp4j as l
import org.eclipse.lsp4j.jsonrpc.ResponseErrorException
import org.eclipse.lsp4j.jsonrpc.messages.{ResponseError, ResponseErrorCode}
import plasmon.integration.TestUtil.*

import java.util.concurrent.{ExecutionException, TimeUnit}

import scala.concurrent.duration.*
import scala.jdk.CollectionConverters.*

/** Errors make it back to whoever asked, and don't take the server down with them.
  *
  * No build tool is loaded in any of these: they're about what happens when there's nothing that
  * could answer.
  */
class ErrorTests extends PlasmonSuite {

  private val files = Seq[(os.SubPath, os.Source)](
    os.sub / "Foo.scala" -> "object Foo\n"
  )

  private def executeCommand(
    driver: ServerDriver.Lsp,
    command: String,
    arguments: Object*
  ): Either[ResponseError, Object] = {
    val params = new l.ExecuteCommandParams
    params.setCommand(command)
    params.setArguments(arguments.toList.asJava)
    try Right(driver.lsp.getWorkspaceService.executeCommand(params).get(30L, TimeUnit.SECONDS))
    catch {
      case e: ExecutionException =>
        e.getCause match {
          case e0: ResponseErrorException => Left(e0.getResponseError)
          case _                          => throw e
        }
    }
  }

  // Out of the expect calls: referencing a Java enum value from within them compiles to a
  // ResponseErrorCode$ module that doesn't exist (NoClassDefFoundError at runtime)
  private val invalidParams = ResponseErrorCode.InvalidParams.getValue
  private val requestFailed = ResponseErrorCode.RequestFailed.getValue

  private def errorCode(res: Either[ResponseError, Object]): Option[Int] =
    res.left.toOption.map(_.getCode)

  test("failing commands answer an error, and the server keeps answering") {
    withLspServer()(files*) {
      (_, driver, _) =>
        // used to be `???`, which ended the LSP connection
        val cleanAll = executeCommand(driver, "plasmon/cleanAll")
        expect(errorCode(cleanAll) == Some(requestFailed))

        val unknown = executeCommand(driver, "plasmon/doesNotExist")
        expect(errorCode(unknown) == Some(invalidParams))

        val badArgs = executeCommand(driver, "plasmon/compile", Int.box(2))
        expect(errorCode(badArgs) == Some(invalidParams))
    }
  }

  test("compiling a file that's in no module fails") {
    withLspServer()(files*) {
      (workspace, driver, _) =>
        val res = executeCommand(
          driver,
          "plasmon/compile",
          (workspace / "Foo.scala").toNIO.toUri.toASCIIString
        )
        expect(errorCode(res) == Some(requestFailed))
        expect(res.left.exists(_.getMessage.contains("No module found")))
    }
  }

  test("failing to restore the saved build tools is shown to the user") {
    val client = new MockLanguageClient {}
    withWorkspaceAndServer(TestMode.Lsp, client = client)(
      (files :+ (os.sub / ".plasmon/build-servers.json" -> ("{": os.Source)))*
    ) {
      (_, _, _, _) =>
        val msg = client.awaitShowMessage(30.seconds) { params =>
          params.getType == l.MessageType.Error &&
          params.getMessage.contains("Error restoring the previously loaded build tools")
        }
        expect(msg.nonEmpty)
    }
  }

  for (mode <- cliOnlyModes)
    test("command errors are printed" + mode.testNameSuffix) {
      withWorkspaceAndServer(mode)(files*) {
        (workspace, _, osOpt, _) =>
          val ex = intercept[os.SubprocessException] {
            serverCommandOutputs(workspace, osOpt)("bsp", "request", "workspace/buildTargets")
          }
          expect(ex.result.exitCode == 1)
          expect(ex.result.err.text().contains("No build server found"))
      }
    }

  for (mode <- cliOnlyModes)
    test("a missing file gets an error message, not a stack trace" + mode.testNameSuffix) {
      withWorkspaceAndServer(mode)(files*) {
        (workspace, _, osOpt, _) =>
          val ex = intercept[os.SubprocessException] {
            serverCommandOutputs(workspace, osOpt)("lsp", "did-open", workspace / "Nope.scala")
          }
          expect(ex.result.exitCode == 1)
          val err = ex.result.err.text()
          // Out of the expect call: Expecty can't rewrite an interpolated os.Path
          val expectedError = s"Error: ${workspace / "Nope.scala"}: no such file"
          expect(err.contains(expectedError))
          expect(!err.contains("NoSuchFileException"))
          expect(!err.contains("\tat "))
      }
    }
}
