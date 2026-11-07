package plasmon.servercommand

import plasmon.bsp.BuildTool

class ScalaCliDiscoverTests extends munit.FunSuite {

  private def withWorkspace[T](f: os.Path => T): T = {
    val dir = os.temp.dir(prefix = "plasmon-scala-cli-discover")
    try f(dir)
    finally os.remove.all(dir)
  }

  private def discovered(workspace: os.Path, file: os.Path): Seq[(Seq[os.SubPath], os.SubPath)] =
    BspUtil.BuildToolDiscover.ScalaCli.check(workspace, Some(file), Set.empty).map { tool =>
      tool.buildTool match {
        case sc: BuildTool.ScalaCli =>
          (sc.sources.map(_.subRelativeTo(workspace)), sc.workspace.subRelativeTo(workspace))
        case other => fail(s"Unexpected build tool $other")
      }
    }

  test("file with directives: the file and its directory") {
    withWorkspace { workspace =>
      val dir = workspace / "src" / "foo"
      os.write(dir / "Main.scala", "//> using scala 3.10.0\nobject Main\n", createFolders = true)
      os.write(dir / "Other.scala", "object Other\n")
      assertEquals(
        discovered(workspace, dir / "Main.scala"),
        Seq(
          (Seq(os.sub / "src/foo/Main.scala"), os.sub),
          (Nil, os.sub / "src/foo")
        )
      )
    }
  }

  test("file with no directives: the closest directory with files with directives") {
    withWorkspace { workspace =>
      val dir = workspace / "src" / "foo"
      os.write(dir / "Main.scala", "//> using scala 3.10.0\nobject Main\n", createFolders = true)
      os.write(dir / "sub" / "Other.scala", "object Other\n", createFolders = true)
      assertEquals(
        discovered(workspace, dir / "sub" / "Other.scala"),
        Seq((Nil, os.sub / "src/foo"))
      )
    }
  }

  test("file with no directives, and none around") {
    withWorkspace { workspace =>
      os.write(workspace / "build.mill", "")
      os.write(workspace / "foo" / "src" / "A.scala", "object A\n", createFolders = true)
      assertEquals(discovered(workspace, workspace / "foo" / "src" / "A.scala"), Nil)
    }
  }
}
