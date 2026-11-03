package plasmon

/** Discards everything, the tests check what gets back to the clients instead */
object TestLogger extends Logger {
  val channel: Logger.Channel = Logger.Channel("test", "Test")
  def log(line: String): Unit = ()
}
