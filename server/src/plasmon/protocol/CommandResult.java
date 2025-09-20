package plasmon.protocol;

public class CommandResult {
  private int exitCode = 0;
  public CommandResult() {}

  public int getExitCode() {
    return exitCode;
  }
  public void setExitCode(int exitCode) {
    this.exitCode = exitCode;
  }
}
