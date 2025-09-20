package plasmon.ide

import com.google.gson.{Gson, JsonElement, JsonObject}

private object JsonParser {
  private val gson = new Gson

  implicit class XtensionSerializableToJson(data: Any) {
    def toJson: JsonElement =
      gson.toJsonTree(data)
    def toJsonObject: JsonObject =
      data.toJson.getAsJsonObject
  }
}
