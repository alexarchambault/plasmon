package plasmon.internal

import org.graalvm.nativeimage.ProcessProperties

object BinaryName {

  private def isGraalvmNativeImage: Boolean =
    sys.props.contains("org.graalvm.nativeimage.imagecode")
  lazy val pathOpt: Option[os.Path] =
    if (isGraalvmNativeImage)
      Some(os.Path(ProcessProperties.getExecutableName))
    else
      None

}
