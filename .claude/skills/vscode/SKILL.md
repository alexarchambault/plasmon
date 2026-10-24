---
name: vscode
description: Check how Plasmon behaves in VSCode itself (status bar, quick picks, hovers, completions, diagnostics, commands of the extension), by driving code-server running the locally built extension and server from a headless browser. Use to see a change in the editor rather than only through LSP-level tests, or to write end-to-end tests under extension/e2e.
---

# Driving VSCode with the local Plasmon

Everything lives in `extension/e2e` (read its `README.md` for details): code-server runs the
extension built from `extension/` and the JVM launcher of the server (`server.launcher`), and
Playwright drives it.

## Interactive session

1. Start it in the background (it keeps running until killed), and wait for its `Ready` line:

   ```text
   MILL_OUTPUT_DIR=.container/out2 ./mill -i extension.e2e.serve [workspace]
   ```

   Without a workspace, a small Mill project gets written to `extension/e2e/.serve/workspace`
   (module `foo`, `foo/src/Main.scala`). A separate Mill out dir keeps the launcher in use from
   being rebuilt under the server's feet; after rebuilding it, restart `serve`.

2. Run snippets against the workbench, state persists between them:

   ```text
   node extension/e2e/dev/drive.ts '
     await vscode.openFile("foo/src/Main.scala")
     await vscode.explorerContextMenu("build.mill", "Load Mill project")
     return vscode.quickPickItems()'
   ```

   In scope: `vscode` (helpers from `extension/e2e/lib/vscode.ts`: `openFile`, `runCommand`,
   `quickPickItems`, `pickQuickPickItem`, `explorerContextMenu`, `goTo`, `hover`, `definition`,
   `completions`, `insertLineAfter`, `deleteLine`, `save`, `plasmonStatus`,
   `waitForPlasmonStatus`, `languageStatus`, `problems`, …), `page` (Playwright), `state`, and
   `screenshot(name)`, which returns the path of a PNG to look at with the Read tool.

3. Stop it by sending SIGINT / SIGTERM to the `node dev/serve.ts` process (by PID, `pkill -f`
   would match the shell running it too).

Things to know:

- Positions are 1-based line / column. Reach them with `vscode.goTo` (Ctrl+G) rather than by
  clicking: Monaco merges tokens like `g.message` into a single DOM node.
- The Plasmon status bar item only shows while a file is focused.
- Builds aren't loaded automatically: use "Load Mill project" on `build.mill` in the explorer
  (offers the `mill-build/` module), or the "Plasmon: Load or unload build tool or module"
  command from a source file.
- Server log: `<workspace>/.plasmon/log`. code-server log: `extension/e2e/.serve/code-server/`.

## Tests

`./mill -i extension.e2e.test [playwright args]` runs `extension/e2e/tests/*.spec.ts` (one
code-server per spec file, tests of a file run serially on the same workbench). Failures leave
`workbench.png` and `plasmon-log-tail.txt` under `extension/e2e/test-results/<test>/`.
