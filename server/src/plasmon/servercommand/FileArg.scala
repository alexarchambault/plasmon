package plasmon.servercommand

import plasmon.PlasmonEnrichments.StringThingExtensions

/** Resolving the file a command acts on, the same way for every command that takes one. */
object FileArg {

  /** Exactly one file, passed either as a path argument or as `--uri`. */
  def single(
    args: Seq[String],
    uriOpt: Option[String],
    workingDir: os.Path
  ): (os.Path, String) =
    (args, uriOpt) match {
      case (Seq(), None) =>
        sys.error("No file specified")
      case (Seq(strPath), None) =>
        val path = os.Path(strPath, workingDir)
        (path, path.toNIO.toUri.toASCIIString)
      case (Seq(), Some(uri)) =>
        (uri.osPathFromUri, uri)
      case (Seq(_), Some(_)) =>
        sys.error("Cannot specify both a file and a URI")
      case (other, _) =>
        assert(other.length > 1)
        sys.error("Too many files specified (only one file accepted)")
    }

  /** At most one file, passed either as a path argument or as `--uri`. */
  def optional(
    args: Seq[String],
    uriOpt: Option[String],
    workingDir: os.Path
  ): Option[os.Path] =
    (args, uriOpt) match {
      case (Seq(), None) => None
      case _             => Some(single(args, uriOpt, workingDir)._1)
    }
}
