package plasmon.protocol;

import com.google.gson.JsonElement;

/**
 * A command to run in the server, as the client parsed it.
 *
 * <p>The arguments are parsed client-side (see plasmon.command.RemoteCommand), so what travels
 * is the command that was meant rather than the words that were typed: the name it was found
 * under, its options as JSON, and whatever arguments were left over.
 */
public class Command {
  private String[] name = null;
  private JsonElement options = null;
  private JsonElement remainingArgs = null;

  public Command() {}

  /** The name the command was found under, one element per word ({@code ["lsp", "hover"]}). */
  public String[] getName() {
    return name;
  }
  public void setName(String[] name) {
    this.name = name;
  }

  /** The options of the command - the JSON of its {@code *Options} class. */
  public JsonElement getOptions() {
    return options;
  }
  public void setOptions(JsonElement options) {
    this.options = options;
  }

  /** The arguments that aren't options - the JSON of {@link RemainingArgsJson}. */
  public JsonElement getRemainingArgs() {
    return remainingArgs;
  }
  public void setRemainingArgs(JsonElement remainingArgs) {
    this.remainingArgs = remainingArgs;
  }
}
