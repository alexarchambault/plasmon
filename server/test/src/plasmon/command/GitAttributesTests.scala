package plasmon.command

import java.nio.file.Path

/** What `plasmon browse` reads from `.gitattributes` files (`linguist-language`, for now) */
class GitAttributesTests extends munit.FunSuite {

  private val tmpDir = FunFixture[os.Path](
    setup = _ => os.temp.dir(prefix = "plasmon-git-attributes-tests"),
    teardown = dir => os.remove.all(dir)
  )

  private def language(attributes: GitAttributes, file: os.Path): Option[String] =
    attributes.value(attributes.rules(file.toNIO.getParent), file.toNIO, "linguist-language")

  private def matches(glob: String, path: String): Boolean =
    GitAttributes.globRegex(glob).r.matches(path)

  test("glob patterns") {
    assert(matches("*.scala", "Foo.scala"))
    assert(!matches("*.scala", "a/Foo.scala"))
    assert(!matches("*.scala", "Foo.scalax"))
    assert(matches("Foo.?", "Foo.c"))
    assert(!matches("Foo.?", "Foo.cc"))
    assert(!matches("a?b", "a/b"))
    assert(matches("[Bb]uild.sc", "build.sc"))
    assert(matches("[Bb]uild.sc", "Build.sc"))
    assert(!matches("[Bb]uild.sc", "cuild.sc"))
    assert(matches("[!a]x", "bx"))
    assert(!matches("[!a]x", "ax"))
    assert(!matches("[!a]x", "/x"))
    assert(matches("**/foo", "foo"))
    assert(matches("**/foo", "a/b/foo"))
    assert(matches("a/**", "a/b/c"))
    assert(!matches("a/**", "b/c"))
    assert(matches("a/**/b", "a/b"))
    assert(matches("a/**/b", "a/x/y/b"))
    assert(matches("\\*.txt", "*.txt"))
    assert(!matches("\\*.txt", "a.txt"))
    // Regex characters are literal
    assert(matches("a.b(c)+", "a.b(c)+"))
    assert(!matches("a.b", "axb"))
  }

  tmpDir.test("unanchored patterns match file names at any depth, anchored ones paths") { dir =>
    os.makeDir(dir / ".git")
    os.write(
      dir / ".gitattributes",
      """*.sc linguist-language=Scala
        |/top.txt linguist-language=Top
        |docs/*.txt linguist-language=Docs
        |""".stripMargin
    )
    val attributes = new GitAttributes(dir.toNIO)
    assertEquals(language(attributes, dir / "build.sc"), Some("Scala"))
    assertEquals(language(attributes, dir / "a" / "b" / "build.sc"), Some("Scala"))
    assertEquals(language(attributes, dir / "top.txt"), Some("Top"))
    assertEquals(language(attributes, dir / "a" / "top.txt"), None)
    assertEquals(language(attributes, dir / "docs" / "a.txt"), Some("Docs"))
    assertEquals(language(attributes, dir / "a" / "docs" / "a.txt"), None)
    assertEquals(language(attributes, dir / "docs" / "a" / "b.txt"), None)
  }

  tmpDir.test("deeper files and later lines win") { dir =>
    os.makeDir(dir / ".git")
    os.write(
      dir / ".gitattributes",
      """# a comment
        |*.sc linguist-language=First
        |*.sc linguist-language=Second
        |*.mill linguist-language=Scala
        |""".stripMargin
    )
    os.write(
      dir / "sub" / ".gitattributes",
      """*.sc linguist-language=Sub
        |""".stripMargin,
      createFolders = true
    )
    val attributes = new GitAttributes(dir.toNIO)
    assertEquals(language(attributes, dir / "a.sc"), Some("Second"))
    assertEquals(language(attributes, dir / "sub" / "a.sc"), Some("Sub"))
    assertEquals(language(attributes, dir / "sub" / "deeper" / "a.sc"), Some("Sub"))
    assertEquals(language(attributes, dir / "sub" / "build.mill"), Some("Scala"))
    // Patterns of a .gitattributes file below don't apply above it
    assertEquals(language(attributes, dir / "other" / "a.sc"), Some("Second"))
  }

  tmpDir.test("unset attributes, unsupported lines") { dir =>
    os.makeDir(dir / ".git")
    os.write(
      dir / ".gitattributes",
      """*.sc linguist-language=Scala
        |no.sc -linguist-language
        |also-no.sc !linguist-language
        |[attr]scala linguist-language=Macro
        |dir/ linguist-language=Dir
        |flag.sc linguist-language
        |several.sc text linguist-language=Several eol=lf
        |""".stripMargin
    )
    val attributes = new GitAttributes(dir.toNIO)
    assertEquals(language(attributes, dir / "a.sc"), Some("Scala"))
    assertEquals(language(attributes, dir / "no.sc"), None)
    assertEquals(language(attributes, dir / "also-no.sc"), None)
    // Set, but with no value
    assertEquals(language(attributes, dir / "flag.sc"), None)
    assertEquals(language(attributes, dir / "several.sc"), Some("Several"))
    assertEquals(language(attributes, dir / "dir"), None)
    // `[attr]scala` would match it, were it read as a pattern
    assertEquals(language(attributes, dir / "ascala"), None)
  }

  tmpDir.test("files from the top of the repository apply to a sub-directory") { dir =>
    os.makeDir(dir / ".git")
    os.write(dir / ".gitattributes", "*.sc linguist-language=Scala\n")
    os.write(
      dir / "project" / ".gitattributes",
      "*.kts linguist-language=Kotlin\n",
      createFolders = true
    )
    val root       = dir / "project"
    val attributes = new GitAttributes(root.toNIO)
    assertEquals(language(attributes, root / "build.sc"), Some("Scala"))
    assertEquals(language(attributes, root / "build.kts"), Some("Kotlin"))
    // Outside of the repository, nothing applies
    val elsewhere = os.temp.dir(prefix = "plasmon-git-attributes-tests-elsewhere")
    try assertEquals(attributes.rules(elsewhere.toNIO), Nil)
    finally os.remove.all(elsewhere)
  }

  tmpDir.test("outside of a repository, the root directory is the top") { dir =>
    val root = dir / "project"
    os.write(dir / ".gitattributes", "*.sc linguist-language=Above\n")
    os.write(root / ".gitattributes", "*.sc linguist-language=Root\n", createFolders = true)
    val attributes = new GitAttributes(root.toNIO)
    assertEquals(language(attributes, root / "build.sc"), Some("Root"))
  }

  tmpDir.test("info/attributes wins") { dir =>
    os.write(
      dir / ".git" / "info" / "attributes",
      "*.sc linguist-language=Info\n",
      createFolders = true
    )
    os.write(dir / ".gitattributes", "*.sc linguist-language=Scala\n*.txt linguist-language=Text\n")
    val attributes = new GitAttributes(dir.toNIO)
    assertEquals(language(attributes, dir / "a.sc"), Some("Info"))
    assertEquals(language(attributes, dir / "a" / "b.sc"), Some("Info"))
    assertEquals(language(attributes, dir / "a.txt"), Some("Text"))
  }

  tmpDir.test("info/attributes of worktrees, in the common Git directory") { dir =>
    val common = dir / "repo" / ".git"
    os.write(common / "info" / "attributes", "*.sc linguist-language=Info\n", createFolders = true)
    os.write(common / "worktrees" / "wt" / "commondir", "../..\n", createFolders = true)
    val worktree = dir / "wt"
    os.write(worktree / ".git", s"gitdir: ${common / "worktrees" / "wt"}\n", createFolders = true)
    val attributes = new GitAttributes((worktree / "src").toNIO)
    assertEquals(language(attributes, worktree / "src" / "a.sc"), Some("Info"))
  }

  tmpDir.test("info/attributes of submodules, with a relative gitdir") { dir =>
    val gitDir = dir / "repo" / ".git" / "modules" / "sub"
    os.write(gitDir / "info" / "attributes", "*.sc linguist-language=Info\n", createFolders = true)
    val sub = dir / "repo" / "sub"
    os.write(sub / ".git", "gitdir: ../.git/modules/sub\n", createFolders = true)
    val attributes = new GitAttributes(sub.toNIO)
    assertEquals(language(attributes, sub / "a.sc"), Some("Info"))
  }

  test("rules only match under their directory") {
    val rule = GitAttributes.Rule(Path.of("/a/b"), "*.sc", Seq("x" -> Some("y")))
    assert(rule.matches(Path.of("/a/b/c.sc")))
    assert(rule.matches(Path.of("/a/b/c/d.sc")))
    assert(!rule.matches(Path.of("/a/c.sc")))
    assert(!rule.matches(Path.of("/a/bc/d.sc")))
  }
}
