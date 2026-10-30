import { expect, test } from '../lib/fixtures.ts'
import { millProject } from '../lib/workspace.ts'

test.use({ workspace: millProject() })
test.describe.configure({ mode: 'serial' })

test('Mill project gets loaded from the explorer', async ({ vscode }) => {
  // The Plasmon status bar item only shows up with a file opened
  await vscode.openFile('foo/src/Main.scala')
  expect(await vscode.waitForPlasmonStatus(/No build tool/)).toBe('No build tool')

  await vscode.explorerContextMenu('build.mill', 'Load Mill project')
  // From build.mill, only the meta-build module is offered
  expect(await vscode.quickPickItems()).toEqual([expect.stringMatching(/^mill-build\//)])
  await vscode.pickQuickPickItem(/^mill-build\//)

  await vscode.runCommand('Plasmon: Load or unload build tool or module')
  expect(await vscode.quickPickItems()).toContainEqual(expect.stringMatching(/^foo\b/))
  await vscode.pickQuickPickItem(/^foo\b/)

  await vscode.waitForPlasmonStatus(/^Compiled$/)
  expect(await vscode.languageStatus()).toContain('Module foo')
})

test('hover', async ({ vscode }) => {
  // `message` in `println(g.message)`
  expect(await vscode.hover(10, 16)).toBe('def message: String')
})

test('go to definition', async ({ vscode }) => {
  // `Greeting` in `val g = Greeting("plasmon")`
  expect(await vscode.definition(9, 14)).toEqual({ file: 'Main.scala', line: 3, col: 12 })
})

test('completions', async ({ vscode }) => {
  await vscode.insertLineAfter(10, '    ')
  expect(await vscode.completions('g.mes')).toContain('message: String')
  await vscode.deleteLine(11)
  await vscode.save()
})

test('diagnostics follow compilation errors', async ({ vscode }) => {
  await vscode.insertLineAfter(10, '    val n: Int = "foo"')
  await vscode.save()
  await vscode.waitForPlasmonStatus(/Compilation failed/)
  expect((await vscode.waitForProblems(p => p.errors > 0)).errors).toBe(1)

  await vscode.deleteLine(11)
  await vscode.save()
  await vscode.waitForPlasmonStatus(/^Compiled$/)
  expect(await vscode.waitForProblems(p => p.errors === 0)).toEqual({ errors: 0, warnings: 0 })
})
