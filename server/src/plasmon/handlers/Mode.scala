package plasmon.handlers

import com.google.gson.{JsonElement, JsonObject}
import plasmon.AutoMode
import plasmon.jsonrpc.{CommandHandler, Handlers}
import plasmon.jsonrpc.CommandHandler.ParamsHelpers.*

import java.util.concurrent.CompletableFuture

/** Commands to query and change the mode of the server, manual or automatic (see [[AutoMode]]).
  *
  *   - `plasmon/mode`: the current mode
  *   - `plasmon/setMode` with `"automatic"` or `"manual"`: switches to that mode
  *
  * Both answer `{ "mode": "automatic" | "manual" }`, the mode the server is in once done.
  */
object Mode {

  private def response(autoMode: AutoMode): JsonElement = {
    val obj = new JsonObject
    obj.addProperty("mode", autoMode.modeName)
    obj
  }

  def handlers(autoMode: AutoMode): Handlers =
    Handlers(
      Nil,
      Nil,
      Seq(
        CommandHandler.of("plasmon/mode") { (_, _) =>
          CompletableFuture.completedFuture(response(autoMode))
        },
        CommandHandler.of("plasmon/setMode", refreshStatus = true) { (params, _) =>
          params.as[String]("plasmon/setMode") { mode =>
            AutoMode.parse(mode) match {
              case Some(enabled) =>
                autoMode.set(enabled)
                CompletableFuture.completedFuture(response(autoMode))
              case None =>
                CommandHandler.invalidParams(
                  s"Unrecognized mode '$mode' (expected ${AutoMode.automatic} or ${AutoMode.manual})"
                )
            }
          }
        }
      )
    )
}
