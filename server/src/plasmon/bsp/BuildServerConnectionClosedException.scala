package plasmon.bsp

/** Fails the requests sent to a build server whose connection went away before it answered. */
final class BuildServerConnectionClosedException(buildToolName: String, cause: Throwable = null)
    extends Exception(s"Connection to build server $buildToolName closed", cause)
