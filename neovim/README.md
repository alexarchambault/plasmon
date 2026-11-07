# Plasmon for Neovim

Neovim support for Plasmon, the Scala language server. Requires Neovim 0.11 or later.

Plasmon speaks standard LSP, so Neovim's built-in client handles hovers, definitions,
completions, diagnostics, etc. on its own. This plugin adds what Plasmon needs on top of that:

- the server starts in the workspace root, where Plasmon keeps its state, its log and the socket
  the `plasmon` command-line uses (`.plasmon/`),
- workspace root detection for Mill, sbt and Scala CLI projects, with the dependency sources
  Plasmon extracts under `.plasmon/readonly/` handled by the server of their workspace,
- `:Plasmon` commands, to load build tools and modules, compile, re-index, etc.,
- Plasmon's status (statusline component, `:Plasmon status`), progress (as LSP progress, so it
  shows up in `vim.lsp.status()` and progress plugins), logs (`:Plasmon log`), and build change
  prompts,
- the client-side commands Plasmon's code lenses and status items refer to,
- tells the server which file is being looked at (Plasmon's status and several commands rely on
  it), and a warning when another Plasmon server already runs in the same workspace.

## Install

You need the `plasmon` launcher on your `PATH` (or pass its path, see below).

The plugin lives in the `neovim/` directory of the Plasmon repository. With
[lazy.nvim](https://github.com/folke/lazy.nvim):

```lua
{
  "alexarchambault/plasmon",
  submodules = false, -- the repository has big submodules the plugin doesn't need
  config = function(plugin)
    vim.opt.rtp:append(plugin.dir .. "/neovim")
    require("plasmon").setup({})
  end,
}
```

From a local checkout of the repository:

```lua
vim.opt.rtp:prepend("/path/to/plasmon/neovim")
require("plasmon").setup({})
```

Don't enable [nvim-metals](https://github.com/scalameta/nvim-metals) (or any other Scala
language server) for the same files.

`setup()` configures and enables the `plasmon` LSP config. Without calling it, the plugin
still provides that config (`lsp/plasmon.lua`), so `vim.lsp.enable("plasmon")` works too, and
`vim.lsp.config("plasmon", { ... })` can tweak it.

## Options

```lua
require("plasmon").setup({
  binary = "plasmon",          -- Plasmon launcher
  server_args = {},            -- extra arguments, after `plasmon server`
  cmd = nil,                   -- full command, replacing binary / server_args
  cmd_env = nil,               -- extra environment variables for the server
  on_build_change = "ask",     -- "ask", "reindex" or "ignore"
  codelens = true,             -- show code lenses
  on_type_formatting = true,   -- on-type formatting (Neovim >= 0.12)
  notify_progress = true,      -- report Plasmon progress as LSP progress
  max_log_lines = 10000,       -- lines kept per log
  lsp = {},                    -- merged into the LSP config (capabilities, settings, …)
})
```

If you override `cmd` via `vim.lsp.config()` rather than `setup()`, use a function, or set
`cmd_cwd`, so that the server still starts in the workspace root.

## Getting started

Open a Scala or Java file, then load its build tool and modules:

```
:Plasmon load-build-tool
```

This lists the build tools Plasmon can load for the current file (Mill, sbt, Scala CLI, …).
Once one is loaded, a second list offers its modules. Plasmon remembers what's loaded, and
restores it the next time it starts in that workspace.

`:Plasmon` alone shows the current status, and lets you run the actions it offers. To show the
status in your statusline:

```lua
vim.o.statusline = "%f %= %{v:lua.require'plasmon'.statusline()} "
```

`require("plasmon").statusline()` is empty in buffers no Plasmon server handles, and a
`User PlasmonStatus` autocmd fires when the status changes.

## Commands

| Command | |
| --- | --- |
| `:Plasmon` / `:Plasmon status` | Show the status, run one of its actions |
| `:Plasmon load-build-tool` | Load or unload a build tool, then its modules |
| `:Plasmon load` | Load or unload a build tool or a module |
| `:Plasmon load-module` | Load or unload a module of the current file |
| `:Plasmon unload-all-modules` / `unload-all-build-tools` | |
| `:Plasmon build-tool-restart` / `build-tool-restart-or-reconnect` | |
| `:Plasmon re-index` / `re-import` | |
| `:Plasmon compile` / `compile-all` / `clean` / `clean-all` / `cancel-all-compilations` | |
| `:Plasmon organize-imports` / `organize-imports-in-module` | Needs `scalafix-native` on `PATH` |
| `:Plasmon toggle-pc-diagnostics` / `enable-pc-diagnostics` / `disable-pc-diagnostics` | Presentation compiler diagnostics |
| `:Plasmon module-actions` / `interactive-compiler` | |
| `:Plasmon log [name]` | Show a Plasmon log (requests, build tools, compilers, …) |
| `:Plasmon server-log` | Open the server log (`.plasmon/log`) |
| `:Plasmon restart` | Restart the server |
| `:Plasmon check-concurrent-server` | Find, and stop, another server running in the workspace |
| `:Plasmon dump-…` | Debugging: server state, BSP data, typed tree, symbol index, semanticdb |

`:checkhealth plasmon` checks the set-up.

## Development

Tests run in headless Neovim, with no dependency beyond Neovim itself:

```
./mill -i neovim.test             # against a fake server (neovim/tests/fake_server.lua)
./mill -i neovim.integrationTest  # against the server built from sources, on a Scala CLI project
```

Both download Neovim, unless `PLASMON_NVIM` points to an `nvim` binary, and accept
`--filter <part of a test name>`. The integration tests need `scala-cli` on `PATH`. To run the
tests with a given Neovim directly:

```
nvim --clean --headless -l neovim/tests/run.lua unit
```

To try the plugin and the server built from sources:

```
./mill -i neovim.run <directory or file> [--config lazyvim|minimal] [nvim arguments…]
```

This builds the server, then runs Neovim (the version the tests use) in that directory, or on
that file, with the plugin from this checkout, and a config of this repository rather than
yours:

- `--config lazyvim` (the default): [LazyVim](https://www.lazyvim.org), set up in
  [`dev/lazyvim.lua`](dev/lazyvim.lua), for an IDE-like set-up: file explorer (`<leader>e`),
  statusline (with Plasmon's status), buffer tabs, pickers, completion, LazyVim's LSP
  mappings, etc. The first run downloads LazyVim and its plugins, at the versions of
  [`dev/lazy-lock.json`](dev/lazy-lock.json), under `~/.local/share/plasmon-dev-lazyvim` (it
  runs with `NVIM_APPNAME=plasmon-dev-lazyvim`, so it doesn't touch your own Neovim set-up).
- `--config minimal`: a minimal config, [`dev/init.lua`](dev/init.lua), with no other plugin:
  a Plasmon statusline, diagnostics as virtual text, completion as you type, and `gd`,
  `<leader>cl` (run a code lens) and `<leader>f` (format) on top of Neovim's default LSP
  mappings (`:help lsp-defaults`).

`-i` is needed so that Neovim gets the terminal. Relative paths are relative to the
repository root.
