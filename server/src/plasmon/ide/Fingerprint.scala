package plasmon.ide

case class Fingerprint(text: String, md5: String) {
  def isEmpty: Boolean = md5.isEmpty
}
object Fingerprint {
  def empty: Fingerprint = Fingerprint("", "")
}
