# VSCode end-to-end tests

These check the Plasmon extension and server together, in a real VSCode workbench: they run
[code-server](https://github.com/coder/code-server) with the extension built from `extension/`
and the server built from `server/`, and drive it from a headless Chromium with
[Playwright](https://playwright.dev).

Requirements: Node ≥ 22.18 (the TypeScript sources run as is) and [pnpm](https://pnpm.io)
(`corepack enable pnpm` for example), on Linux or macOS. Mill fetches code-server
(`Versions.Test.codeServer` in `build.mill`) and Playwright's Chromium itself.

## Running the tests

```text
./mill -i extension.e2e.test                  # all specs
./mill -i extension.e2e.test tests/mill.spec.ts -g hover   # Playwright arguments are passed as is
```

This builds `server.launcher` and `extension.vsix`, then runs `pnpm exec playwright test` here.
Each Playwright worker starts its own code-server (on a free port, with its own profile under
`test-results/`), on a fresh copy of the workspace its spec file asks for, and the tests of a spec
file share that workbench, running serially. On failure, a screenshot of the workbench and the end
of the Plasmon log get attached to the test, and the code-server logs stay in `test-results/`.

`server.launcher` loads the classes from Mill's `out` directory lazily: don't rebuild it while
tests (or `serve` below) run from it, or use a separate out directory
(`MILL_OUTPUT_DIR=.container/out2` for example).

### Writing tests

`lib/vscode.ts` has the helpers to drive the workbench: open files, run commands, pick quick pick
items, use the explorer context menu, get hovers, completions, definition locations, problem
counts, or read the Plasmon status bar items. Positions are 1-based line / column, like in the
status bar.

Prefer the keyboard to the mouse in the editor: Monaco doesn't render one DOM node per token,
so the helpers move the cursor with "Go to Line/Column" (Ctrl+G) and then trigger hovers (Ctrl+K
Ctrl+I), definitions (F12), or completions (Ctrl+Space).

Test projects are defined in `lib/workspace.ts`, and picked by a spec with
`test.use({ workspace: … })`.

## Driving VSCode interactively

To look at how something behaves in VSCode, like an agent would with a browser:

```text
./mill -i extension.e2e.serve [--port 8080] [workspace]
```

starts code-server with the local extension and server, opens the workspace (a sample Mill
project in `.serve/workspace` if none is passed) in a headless Chromium, prints the code-server
URL, and keeps running until interrupted. The code-server URL can also be opened in a regular
browser.

Then run snippets against that workbench (the page and editor state persist between runs):

```text
node extension/e2e/dev/drive.ts '<async JS function body>'
node extension/e2e/dev/drive.ts --file script.js
```

with `vscode` (a `lib/vscode.ts` `VSCode`), `page` (the Playwright page), `state` (URLs, log
paths, see `dev/state.ts`) and `screenshot(name?)` (saves a PNG under `.serve/screenshots/`,
returns its path) in scope. A returned value gets printed. For example

```text
node extension/e2e/dev/drive.ts '
  await vscode.explorerContextMenu("build.mill", "Load Mill project")
  return vscode.quickPickItems()'
node extension/e2e/dev/drive.ts '
  await vscode.pickQuickPickItem(/^mill-build/)
  await vscode.openFile("foo/src/Main.scala")
  await vscode.runCommand("Plasmon: Load or unload build tool or module")
  await vscode.pickQuickPickItem(/^foo/)
  await vscode.waitForPlasmonStatus(/^Compiled$/)
  return { hover: await vscode.hover(10, 16), shot: await screenshot("hover") }'
```

The Plasmon server log is at `<workspace>/.plasmon/log`, the code-server one at
`.serve/code-server/code-server.log`.
