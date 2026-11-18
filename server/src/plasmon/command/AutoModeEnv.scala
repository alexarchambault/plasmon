package plasmon.command

/** How `plasmon code --auto` and `plasmon nvim --auto` tell the editor they start which mode its
  * Plasmon server should start in (see [[plasmon.AutoMode]]).
  *
  * The editor reads `PLASMON_AUTO_MODE`, and passes `--auto` (`true`) or `--auto=false` (`false`)
  * to the first server it starts for a workspace. Only the first one: users may switch modes from
  * the editor afterwards, and a server restarted later should keep the mode they picked, which the
  * server persists.
  */
object AutoModeEnv {
  def name = "PLASMON_AUTO_MODE"

  def apply(auto: Option[Boolean]): Map[String, String] =
    auto.map(value => name -> value.toString).toMap
}
