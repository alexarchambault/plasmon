package plasmon.integration

import java.io.RandomAccessFile

import scala.util.Using

/** The third-party projects some integration tests run on.
  *
  * These used to be Git submodules of this repository. They are now checked out on-demand, at the
  * commits pinned in `Versions.Test` in `build.mill`, under a directory that is kept around across
  * test runs - both the checkout itself, and the build output the tests write in it.
  */
object TestProjects {

  /** Where checkouts are kept, `out/test-projects` under the repository root (see `sharedForkArgs`
    * in `build.mill`).
    */
  lazy val cacheDir: os.Path = {
    val path = sys.props.getOrElse(
      "plasmon.integration.projects",
      sys.error("plasmon.integration.projects not set")
    )
    os.Path(path, os.pwd)
  }

  /** Checks out `repository` at `commit`, and returns the checkout directory.
    *
    * The checkout is only done once per commit: later runs re-use it as-is, along with whatever the
    * tests wrote in it (`.bsp`, `.plasmon`, `out`, …). The marker file is written last, so a
    * checkout interrupted half-way is redone from scratch rather than used.
    *
    * `submodules` are the submodules of `repository` to check out too, if any. They are listed
    * explicitly rather than checked out all at once, as some repositories have large submodules
    * that only matter to run their own tests (which we don't).
    */
  private def checkout(
    name: String,
    repository: String,
    commit: String,
    submodules: Seq[String] = Nil
  ): os.Path = {
    val base   = cacheDir / name
    val dir    = base / commit
    val marker = base / s"$commit.checked-out"
    if (!os.exists(marker)) {
      os.makeDir.all(base)
      // Test suites can be forked in several JVMs, see testParallelism in build.mill
      withLock(base / s"$commit.lock") {
        if (!os.exists(marker)) {
          System.err.println(s"Checking out $repository @ $commit under $dir")
          os.remove.all(dir)
          os.makeDir.all(dir)
          def git(args: String*): Unit =
            os.proc("git", args)
              .call(cwd = dir, stdin = os.Inherit, stdout = os.Inherit, stderr = os.Inherit)
          git("init", "--quiet")
          git("remote", "add", "origin", repository)
          // Fetching a commit by hash works on GitHub, which all of these repositories are on
          git("fetch", "--quiet", "--depth", "1", "origin", commit)
          git("-c", "advice.detachedHead=false", "checkout", "--quiet", "--detach", "FETCH_HEAD")
          if (submodules.nonEmpty)
            git(Seq("submodule", "update", "--init", "--depth", "1", "--") ++ submodules*)
          os.write(marker, commit + System.lineSeparator())
        }
      }
    }
    dir
  }

  private def withLock[T](lockFile: os.Path)(f: => T): T =
    Using.resource(new RandomAccessFile(lockFile.toIO, "rw")) { raf =>
      val lock = raf.getChannel.lock()
      try f
      finally lock.release()
    }

  lazy val coursier: os.Path =
    checkout(
      "coursier",
      IntegrationConstants.coursierProjectRepository,
      IntegrationConstants.coursierProjectCommit,
      // Sources of these two end up in the modules the tests compile. The test metadata submodules
      // are only needed to run coursier's own tests, and are big, so we leave them out.
      submodules = Seq("modules/directories", "modules/windows-ansi")
    )

  lazy val mill: os.Path =
    checkout(
      "mill",
      IntegrationConstants.millProjectRepository,
      IntegrationConstants.millProjectCommit
    )
}
