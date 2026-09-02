package plasmon.servercommand

/** The options of a command that takes `--auto`.
  *
  * What `--auto` means is spread over both sides of the wire - [[plasmon.command.AutoServer]]
  * starts a server if there is none, [[AutoLoad]] loads what the file needs once there is one - and
  * the client has to know, from the options it just parsed, whether this command was asked for it.
  * Hence a type rather than looking for `--auto` among the arguments: a command that takes the flag
  * says so here, and one that doesn't cannot be mistaken for one that does.
  */
trait HasAutoOption {
  def auto: Boolean
}
