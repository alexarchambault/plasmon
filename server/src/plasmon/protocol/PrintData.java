package plasmon.protocol;

public class PrintData {
  private String line = "";
  private boolean isStderr = false;
  public PrintData() {}
  public PrintData(String line) {
    this.line = line;
  }
  public PrintData(String line, boolean isStderr) {
    this.line = line;
    this.isStderr = isStderr;
  }

  public String getLine() {
    return line;
  }
  public void setLine(String line) {
    this.line = line;
  }
  public boolean getIsStderr() {
    return isStderr;
  }
  public void setIsStderr(boolean isStderr) {
    this.isStderr = isStderr;
  }
}
