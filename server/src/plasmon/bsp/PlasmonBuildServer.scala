package plasmon.bsp

import ch.epfl.scala.bsp4j as b

import scala.build.bsp.ScalaScriptBuildServer

trait PlasmonBuildServer
    extends b.BuildServer
    with b.ScalaBuildServer
    with b.JavaBuildServer
    with b.JvmBuildServer
    with ScalaScriptBuildServer
