-- Plugin tests against a fake server (tests/fake_server.lua)

local h = require("helpers")
local plasmon = require("plasmon")
local config = require("plasmon.config")
local handlers = require("plasmon.handlers")

local nvim = vim.v.progpath
local fake_server_cmd = { nvim, "--clean", "--headless", "-l", vim.fs.joinpath(h.tests_dir, "fake_server.lua") }

local function received(client, method)
  local all = h.request(client, "fake/received")
  if not method then
    return all
  end
  return vim.tbl_filter(function(msg)
    return msg.method == method
  end, all)
end

local function executed(client, command)
  local res = {}
  for _, msg in ipairs(received(client, "workspace/executeCommand")) do
    if msg.params.command == command then
      table.insert(res, msg.params.arguments)
    end
  end
  return res
end

local function notify_from_server(client, method, params)
  h.request(client, "fake/notify", { method = method, params = params })
end

--- Scala CLI-like project with a Main.scala, opened in the current window.
local function open_project()
  local root = h.tmpdir()
  h.write(root .. "/project.scala", "//> using scala 3.8.3\n")
  h.write(root .. "/Main.scala", "object Main {\n  def foo = 2\n}\n")
  h.write(root .. "/Other.scala", "object Other {\n  val a = 1\n  val b = 2\n  val c = 3\n}\n")
  vim.cmd.edit(vim.fn.fnameescape(root .. "/Main.scala"))
  local client = h.wait_for_client()
  return root, client
end

describe("root detection", function()
  after_each(h.cleanup)

  it("prefers a directory Plasmon already worked in", function()
    local root = h.tmpdir()
    h.write(root .. "/build.mill", "")
    h.write(root .. "/sub/.plasmon/log", "")
    h.write(root .. "/sub/build.mill", "")
    h.write(root .. "/sub/src/A.scala", "")
    h.eq(root .. "/sub", config.find_root(root .. "/sub/src/A.scala"))
  end)

  it("maps dependency sources extracted by Plasmon to their workspace", function()
    local root = h.tmpdir()
    h.write(root .. "/.plasmon/readonly/dependencies/scala-library-sources.jar/scala/Predef.scala", "")
    h.write(root .. "/.plasmon/readonly/dependencies/scala-library-sources.jar/build.sbt", "")
    h.eq(root, config.find_root(root .. "/.plasmon/readonly/dependencies/scala-library-sources.jar/scala/Predef.scala"))
  end)

  it("uses the closest Mill build", function()
    local root = h.tmpdir()
    h.write(root .. "/.git/HEAD", "")
    h.write(root .. "/mill-project/build.mill", "")
    h.write(root .. "/mill-project/foo/package.mill", "")
    h.write(root .. "/mill-project/foo/src/A.scala", "")
    h.eq(root .. "/mill-project", config.find_root(root .. "/mill-project/foo/src/A.scala"))
  end)

  it("climbs up to the outermost sbt build", function()
    local root = h.tmpdir()
    h.write(root .. "/build.sbt", "")
    h.write(root .. "/core/build.sbt", "")
    h.write(root .. "/core/src/main/scala/A.scala", "")
    h.eq(root, config.find_root(root .. "/core/src/main/scala/A.scala"))
  end)

  it("uses Scala CLI markers, then git, then the file's directory", function()
    local root = h.tmpdir()
    h.write(root .. "/.git/HEAD", "")
    h.write(root .. "/cli/project.scala", "")
    h.write(root .. "/cli/sub/A.scala", "")
    h.write(root .. "/other/B.scala", "")
    h.eq(root .. "/cli", config.find_root(root .. "/cli/sub/A.scala"))
    h.eq(root, config.find_root(root .. "/other/B.scala"))

    local lone = h.tmpdir()
    h.write(lone .. "/dir/C.scala", "")
    h.eq(lone .. "/dir", config.find_root(lone .. "/dir/C.scala"))
  end)

  it("doesn't climb out of a git repository cloned in another build", function()
    local root = h.tmpdir()
    h.write(root .. "/build.mill", "")
    h.write(root .. "/.plasmon/log", "")
    h.write(root .. "/work/repo/.git/HEAD", "")
    h.write(root .. "/work/repo/src/A.scala", "")
    h.eq(root .. "/work/repo", config.find_root(root .. "/work/repo/src/A.scala"))

    -- same for a worktree, whose .git is a file
    h.write(root .. "/work/wt/.git", "gitdir: /elsewhere/.git/worktrees/wt\n")
    h.write(root .. "/work/wt/src/A.scala", "")
    h.eq(root .. "/work/wt", config.find_root(root .. "/work/wt/src/A.scala"))
  end)

  it("keeps submodule files in the enclosing build", function()
    local root = h.tmpdir()
    h.write(root .. "/.git/HEAD", "")
    h.write(root .. "/build.mill", "")
    h.write(root .. "/submodules/lib/.git", "gitdir: ../../.git/modules/submodules/lib\n")
    h.write(root .. "/submodules/lib/src/A.scala", "")
    h.eq(root, config.find_root(root .. "/submodules/lib/src/A.scala"))
  end)
end)

describe("helpers", function()
  it("strips VS Code icons", function()
    h.eq("proj_123", handlers.strip_icons("$(tag) proj_123"))
    h.eq("Indexing", handlers.strip_icons("Indexing $(loading~spin)"))
    h.eq("Go to compiled source", handlers.strip_icons("$(arrow-left) Go to compiled source"))
  end)

  it("formats picker entries", function()
    local commands = require("plasmon.commands")
    h.eq(
      "[loaded] Module: [recommended] proj_123 (Scala CLI) - Unload module",
      commands.format_entry({
        type = "Module",
        label = "$(tag) proj_123",
        description = "Scala CLI",
        detail = "Unload module",
        alreadyLoaded = true,
      })
    )
  end)
end)

describe("with a fake server", function()
  before_each(function()
    h.capture_notifications()
    plasmon.setup({ cmd = fake_server_cmd, on_build_change = "ask" })
  end)
  after_each(function()
    handlers.build_change_choice = nil
    h.cleanup()
  end)

  it("starts the server in the workspace root", function()
    local root, client = open_project()
    h.eq(root, client.config.root_dir)
    h.eq(root, h.request(client, "fake/info").cwd)
    local init = received(client, "initialize")[1].params
    h.eq(vim.uri_from_fname(root), init.rootUri)
    h.eq(vim.uri_from_fname(root), init.workspaceFolders[1].uri)
    h.eq("none", init.initializationOptions.icons)
  end)

  it("tells the server which file is focused", function()
    local root, client = open_project()
    local main_uri = vim.uri_from_fname(root .. "/Main.scala")
    h.wait_for("focus notification", function()
      return #received(client, "metals/didFocusTextDocument") > 0
    end)
    h.eq({ main_uri }, received(client, "metals/didFocusTextDocument")[1].params)

    vim.cmd.edit(vim.fn.fnameescape(root .. "/Other.scala"))
    h.wait_for_client()
    local other_uri = vim.uri_from_fname(root .. "/Other.scala")
    h.wait_for("focus notification for Other.scala", function()
      local focus = received(client, "metals/didFocusTextDocument")
      return focus[#focus].params[1] == other_uri
    end)
    h.eq(1, #vim.lsp.get_clients({ name = "plasmon" }), "both files share one server")

    vim.cmd.buffer(vim.fn.bufnr(root .. "/Main.scala"))
    h.wait_for("focus notification back on Main.scala", function()
      local focus = received(client, "metals/didFocusTextDocument")
      return focus[#focus].params[1] == main_uri
    end)
  end)

  it("keeps the status sent by the server", function()
    local root, client = open_project()
    local uri = vim.uri_from_fname(root .. "/Main.scala")
    local events = 0
    vim.api.nvim_create_autocmd("User", {
      pattern = "PlasmonStatus",
      callback = function()
        events = events + 1
      end,
    })
    notify_from_server(client, "plasmon/statusUpdate", {
      uri,
      {
        { id = "plasmon.summary", text = "$(sync) Indexing", severity = 1, busy = true },
        {
          id = "plasmon.build-tool",
          text = "Scala CLI",
          severity = 0,
          busy = false,
          detail = "BSP 2.1.1",
          command = { title = "Show log", command = "plasmon.show-log", arguments = { "bsp-scala-cli" } },
        },
      },
    })
    h.wait_for("status update", function()
      return #plasmon.status() == 2
    end)
    h.truthy(events > 0, "PlasmonStatus autocmd fired")
    h.eq("Indexing …", plasmon.statusline())
    h.eq("Scala CLI", plasmon.status()[2].text)

    notify_from_server(client, "plasmon/statusUpdate", {
      uri,
      {
        { id = "plasmon.summary", text = "Scala CLI", severity = 0, busy = false },
        { id = "plasmon.build-tool", text = "", severity = 0, busy = false },
      },
    })
    h.wait_for("status item removal", function()
      return #plasmon.status() == 1
    end)
    h.eq("Scala CLI", plasmon.statusline())

    -- Running a status item's command, from :Plasmon status
    notify_from_server(client, "plasmon/log", { channelId = "bsp-scala-cli", channelLabel = "Scala CLI", lines = { "hello" } })
    notify_from_server(client, "plasmon/statusUpdate", {
      uri,
      {
        {
          id = "plasmon.build-tool",
          text = "Scala CLI",
          severity = 0,
          busy = false,
          command = { title = "Show log", command = "plasmon.show-log", arguments = { "bsp-scala-cli" } },
        },
      },
    })
    h.wait_for("status update", function()
      return #plasmon.status() == 2
    end)
    h.stub_select(function(items)
      for i, item in ipairs(items) do
        if item.command then
          return i
        end
      end
    end)
    vim.cmd("Plasmon status")
    h.wait_for("log buffer", function()
      return vim.api.nvim_buf_get_name(0):find("plasmon://log/bsp-scala-cli", 1, true)
    end)
    h.eq({ "hello" }, vim.api.nvim_buf_get_lines(0, 0, -1, false))
  end)

  it("collects logs, and shows them in buffers that follow them", function()
    local _, client = open_project()
    notify_from_server(client, "plasmon/log", {
      channelId = "lsp-requests",
      channelLabel = "Requests",
      lines = { "first", "\27[32msecond\27[0m\nthird" },
    })
    h.wait_for("log lines", function()
      return handlers.logs["lsp-requests"] and #handlers.logs["lsp-requests"].lines == 3
    end)
    vim.cmd("Plasmon log lsp-requests")
    local bufnr = vim.api.nvim_get_current_buf()
    h.eq({ "first", "second", "third" }, vim.api.nvim_buf_get_lines(bufnr, 0, -1, false))
    notify_from_server(client, "plasmon/log", { channelId = "lsp-requests", channelLabel = "Requests", lines = { "fourth" } })
    h.wait_for("log buffer update", function()
      return vim.api.nvim_buf_line_count(bufnr) == 4
    end)
    h.eq({ "lsp-requests" }, require("plasmon.commands").complete("lsp", "Plasmon log lsp", 0))
  end)

  it("reports progress as LSP progress", function()
    local _, client = open_project()
    local progress = {}
    vim.api.nvim_create_autocmd("LspProgress", {
      callback = function(ev)
        table.insert(progress, ev.data.params.value)
      end,
    })
    local details = {
      buildToolId = "scala-cli",
      buildToolName = "Scala CLI",
      requestId = "42",
      request = "compile",
      done = false,
    }
    notify_from_server(client, "plasmon/progress", details)
    notify_from_server(client, "plasmon/progress", vim.tbl_extend("force", details, { progress = 0.5 }))
    notify_from_server(client, "plasmon/progress", vim.tbl_extend("force", details, { done = true }))
    h.wait_for("progress end", function()
      return #progress == 3
    end)
    h.eq("begin", progress[1].kind)
    h.eq("Scala CLI: compile", progress[1].title)
    h.eq("report", progress[2].kind)
    h.eq(50, progress[2].percentage)
    h.eq("end", progress[3].kind)
  end)

  it("asks whether to re-index on build changes", function()
    local _, client = open_project()
    h.stub_select(function(items)
      return vim.fn.index(items, "Re-index") + 1
    end)
    notify_from_server(client, "plasmon/buildChangeDetected", vim.empty_dict())
    h.wait_for("plasmon/index", function()
      return #executed(client, "plasmon/index") == 1
    end)
    h.eq("Plasmon: build change detected", h.selects[1].prompt)

    h.stub_select(function(items)
      return vim.fn.index(items, "Dismiss all") + 1
    end)
    notify_from_server(client, "plasmon/buildChangeDetected", vim.empty_dict())
    h.wait_for("prompt", function()
      return #h.selects == 1
    end)
    vim.wait(100)
    notify_from_server(client, "plasmon/buildChangeDetected", vim.empty_dict())
    vim.wait(200)
    h.eq(1, #h.selects, "no more prompts after 'Dismiss all'")
    h.eq(1, #executed(client, "plasmon/index"))
  end)

  it("runs plasmon-goto-location from the server", function()
    local root, client = open_project()
    local other = root .. "/Other.scala"
    notify_from_server(client, "plasmon/executeClientCommand", {
      command = "plasmon-goto-location",
      arguments = {
        {
          uri = vim.uri_from_fname(other),
          range = { start = { line = 2, character = 6 }, ["end"] = { line = 2, character = 7 } },
        },
      },
    })
    h.wait_for("jump to Other.scala", function()
      return vim.api.nvim_buf_get_name(0) == other
    end)
    h.eq({ 3, 6 }, vim.api.nvim_win_get_cursor(0))
  end)

  it("answers heart beats", function()
    local _, client = open_project()
    notify_from_server(client, "plasmon/heartBeat", vim.NIL)
    h.wait_for("plasmon/clientHeartBeat", function()
      return #executed(client, "plasmon/clientHeartBeat") == 1
    end)
  end)

  it("loads a build tool, then a module, from :Plasmon load-build-tool", function()
    local root, client = open_project()
    local uri = vim.uri_from_fname(root .. "/Main.scala")
    h.stub_select(function(items)
      for i, item in ipairs(items) do
        if item.id == "scala-cli" or item.type == "Module" then
          return i
        end
      end
    end)
    vim.cmd("Plasmon load-build-tool")
    h.wait_for("plasmon/loadModule", function()
      return #executed(client, "plasmon/loadModule") == 1
    end)
    h.eq({ { uri } }, executed(client, "plasmon/listBuildTools"))
    h.eq({ { "scala-cli", "scala-cli", uri } }, executed(client, "plasmon/loadBuildTool"))
    h.eq({ { vim.uri_from_fname(root), "scala-cli", "file:/proj/.scala-build/?id=proj_123" } }, executed(client, "plasmon/loadModule"))
    h.eq("Build tool: Scala CLI in this workspace - Load build tool", h.selects[1].formatted[2])
    h.eq("Module: [recommended] proj_123 (Scala CLI, Scala 3.8.3, JVM) - Load module", h.selects[2].formatted[1])
  end)

  it("runs commands on the current file", function()
    local root, client = open_project()
    local uri = vim.uri_from_fname(root .. "/Main.scala")
    vim.cmd("Plasmon compile")
    vim.cmd("Plasmon organize-imports")
    vim.cmd("Plasmon reindex")
    h.wait_for("commands", function()
      return #executed(client, "plasmon/index") == 1
    end)
    h.eq({ { uri } }, executed(client, "plasmon/compile"))
    h.eq({ { uri } }, executed(client, "plasmon/organizeImports"))
    h.wait_for("organize imports notification", function()
      return h.notified("imports were already organized")
    end)

    -- From a buffer Plasmon doesn't handle, commands apply to the last focused file
    vim.cmd("new")
    vim.cmd("Plasmon compile")
    h.wait_for("second compile", function()
      return #executed(client, "plasmon/compile") == 2
    end)
    h.eq({ uri }, executed(client, "plasmon/compile")[2])

    vim.cmd("Plasmon dump-server-state")
    h.wait_for("server state buffer", function()
      return vim.api.nvim_buf_get_name(0):find("plasmon://server-state.json", 1, true)
    end)
    h.eq({ '{ "state": "fake" }' }, vim.api.nvim_buf_get_lines(0, 0, -1, false))
  end)

  it("completes :Plasmon sub-commands", function()
    local commands = require("plasmon.commands")
    local res = commands.complete("load", "Plasmon load", 0)
    h.eq({ "load", "load-build-tool", "load-build-tool-or-module", "load-module" }, res)
  end)

  it("restarts the server", function()
    local _, client = open_project()
    local bufnr = vim.api.nvim_get_current_buf()
    vim.cmd("Plasmon restart")
    local new_client = h.wait_for("new client", function()
      local c = vim.lsp.get_clients({ name = "plasmon", bufnr = bufnr })[1]
      return c and c.id ~= client.id and c.initialized and c
    end)
    h.truthy(client:is_stopped(), "former client stopped")
    h.eq(client.config.root_dir, new_client.config.root_dir)
  end)

  it("warns about a concurrent server holding the lock", function()
    local root = h.tmpdir()
    h.write(root .. "/project.scala", "")
    h.write(root .. "/Main.scala", "object Main\n")
    -- A process that isn't a child of this Neovim, standing for a server started from a terminal
    local res = vim.system({ "sh", "-c", "sleep 30 > /dev/null 2>&1 & echo $!" }, { text = true }):wait()
    local pid = assert(tonumber(vim.trim(res.stdout)))
    local ok, err = pcall(function()
      h.write(root .. "/.plasmon/lock", "pid=" .. pid)
      vim.cmd.edit(vim.fn.fnameescape(root .. "/Main.scala"))
      h.wait_for_client()
      h.wait_for("warning", function()
        return h.notified("another Plasmon server (PID " .. pid .. ")")
      end)

      h.stub_select(function(items)
        return vim.fn.index(items, "Stop it") + 1
      end)
      vim.cmd("Plasmon check-concurrent-server")
      h.wait_for("the other server to stop", function()
        return not config.is_alive(pid)
      end, 5000)
    end)
    pcall(vim.uv.kill, pid, "sigkill")
    if not ok then
      error(err, 0)
    end
  end)

  it("doesn't start for buffers that aren't files", function()
    vim.cmd("enew")
    vim.bo.buftype = "nofile"
    vim.bo.filetype = "scala"
    vim.wait(300)
    h.eq(0, #vim.lsp.get_clients({ name = "plasmon" }))
  end)
end)

describe("missing server binary", function()
  after_each(h.cleanup)

  it("says which option to set", function()
    h.capture_notifications()
    plasmon.setup({ binary = "plasmon-binary-that-does-not-exist" })
    local root = h.tmpdir()
    h.write(root .. "/project.scala", "")
    h.write(root .. "/Main.scala", "object Main\n")
    pcall(vim.cmd, "silent! edit " .. vim.fn.fnameescape(root .. "/Main.scala"))
    h.wait_for("the error", function()
      return h.notified("plasmon-binary-that-does-not-exist not found or not executable")
    end)
  end)
end)

describe(":checkhealth", function()
  after_each(h.cleanup)

  it("runs", function()
    plasmon.setup({ binary = "plasmon-binary-that-does-not-exist" })
    vim.cmd("silent checkhealth plasmon")
    local text = table.concat(vim.api.nvim_buf_get_lines(0, 0, -1, false), "\n")
    h.contains(text, "plasmon-binary-that-does-not-exist not found")
  end)
end)
