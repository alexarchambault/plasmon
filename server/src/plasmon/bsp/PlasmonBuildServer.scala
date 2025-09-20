package plasmon.bsp

import scala.build.bsp.ScalaScriptBuildServer

import ch.epfl.scala.{bsp4j => b}

trait PlasmonBuildServer
    extends b.BuildServer
    with b.ScalaBuildServer
    with b.JavaBuildServer
    with b.JvmBuildServer
    with ScalaScriptBuildServer
