package plasmon.ide

class IconsTests extends munit.FunSuite {

  test("strip VS Code icon markers") {
    assertEquals(Icons.stripCodicons("$(arrow-up) Greeter.greeting"), "Greeter.greeting")
    assertEquals(
      Icons.stripCodicons("Go to original source $(arrow-right)"),
      "Go to original source"
    )
    assertEquals(Icons.stripCodicons("Indexing $(loading~spin)"), "Indexing")
    assertEquals(Icons.stripCodicons("no icons, $ (here) or $x"), "no icons, $ (here) or $x")
  }
}
