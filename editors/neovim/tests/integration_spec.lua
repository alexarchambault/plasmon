-- Plugin tests against an actual Plasmon server, on a Scala CLI project.
--
-- Needs:
-- - PLASMON_NVIM_SERVER_CMD: JSON array, the command starting the server (`plasmon server …`)
-- - scala-cli on PATH
-- Optionally PLASMON_NVIM_SCALA_VERSION (Scala version of the test project) and
-- PLASMON_NVIM_TIMEOUT (seconds, base timeout for slow steps).
--
-- The tests share one server, and run in order.

local h = require("helpers")
local plasmon = require("plasmon")
local handlers = require("plasmon.handlers")

local server_cmd = vim.env.PLASMON_NVIM_SERVER_CMD and vim.json.decode(vim.env.PLASMON_NVIM_SERVER_CMD)
local scala_version = vim.env.PLASMON_NVIM_SCALA_VERSION or "3.10.0"
local timeout = (tonumber(vim.env.PLASMON_NVIM_TIMEOUT) or 180) * 1000

local main_content = ([[
//> using scala %s

trait Greeter {
  def greeting(name: String): String
}

object Main extends Greeter {
  def greeting(name: String): String = s"Hello $name"
  def main(args: Array[String]): Unit =
    println(greeting("world"))
}
]]):format(scala_version)

-- 0-based lines in main_content
local greeting_line = 7
local println_line = 9

local state = {} ---@type { root: string, main: string, bufnr: integer, client: vim.lsp.Client }

--- Position of the first occurrence of `needle` on the 0-based line `line` of the main file.
local function pos(line, needle, offset)
  local text = vim.api.nvim_buf_get_lines(state.bufnr, line, line + 1, false)[1]
  local col = assert(text:find(needle, 1, true), needle .. " not found on line " .. line) - 1
  return { line = line, character = col + (offset or 0) }
end

local function text_params(position)
  return { textDocument = { uri = vim.uri_from_bufnr(state.bufnr) }, position = position }
end

local function focus_main()
  vim.cmd("silent! only")
  vim.api.nvim_set_current_buf(state.bufnr)
end

describe("with a Plasmon server", function()
  it("starts in the workspace root and loads a Scala CLI project", function()
    assert(server_cmd, "PLASMON_NVIM_SERVER_CMD must be set")
    h.capture_notifications()

    state.root = h.tmpdir()
    state.main = state.root .. "/Main.scala"
    h.write(state.main, main_content)

    plasmon.setup({ cmd = server_cmd, on_build_change = "ignore" })
    vim.cmd.edit(vim.fn.fnameescape(state.main))
    state.bufnr = vim.api.nvim_get_current_buf()
    local ok, client = pcall(h.wait_for_client, state.bufnr, timeout)
    if not ok then
      error(("%s\nserver command: %s\nnotifications: %s"):format(
        client,
        vim.inspect(server_cmd),
        vim.inspect(h.notifications)
      ))
    end
    state.client = client
    h.eq(state.root, state.client.config.root_dir)

    -- The server works in the workspace root: lock file, log, command socket for the CLI
    h.wait_for(".plasmon/lock", function()
      local f = io.open(state.root .. "/.plasmon/lock")
      local content = f and f:read("*a")
      if f then
        f:close()
      end
      return content and content:match("^pid=%d+")
    end)

    h.stub_select(function(items)
      for i, item in ipairs(items) do
        -- "Scala CLI in this workspace", then the main module (not its test scope)
        if item.type == "BuildTool" and item.id == "scala-cli" then
          return i
        end
        if item.type == "Module" and item.alreadyLoaded == false and not item.uri:find("-test$") then
          return i
        end
      end
    end)
    vim.cmd("Plasmon load-build-tool")
    h.wait_for("module to load", function()
      return h.notified("Plasmon: loaded module") or h.notified("error")
    end, timeout)
    if h.notified("error") then
      error(h.notified("error").msg)
    end
    h.eq(2, #h.selects, "build tool picker, then module picker")
    h.restore()
  end)

  it("shows hovers", function()
    focus_main()
    local result = h.wait_for("hover", function()
      local resp = state.client:request_sync("textDocument/hover", text_params(pos(println_line, "greeting", 2)), 30000, state.bufnr)
      return resp and resp.result and resp.result.contents and resp.result
    end, timeout)
    h.contains(result.contents.value, "def greeting(name: String): String")
  end)

  it("goes to definitions, in the workspace and in dependencies", function()
    focus_main()
    local position = pos(println_line, "greeting", 2)
    vim.api.nvim_win_set_cursor(0, { position.line + 1, position.character })
    vim.lsp.buf.definition()
    h.wait_for("jump to greeting's definition", function()
      return vim.api.nvim_win_get_cursor(0)[1] == greeting_line + 1
    end, 60000)
    h.eq(state.main, vim.api.nvim_buf_get_name(0))

    position = pos(println_line, "println", 2)
    vim.api.nvim_win_set_cursor(0, { position.line + 1, position.character })
    vim.lsp.buf.definition()
    local name = h.wait_for("jump to println's definition", function()
      local name = vim.api.nvim_buf_get_name(0)
      return name ~= state.main and name
    end, 60000)
    h.contains(name, state.root .. "/.plasmon/readonly/")
    h.contains(name, "Predef.scala")
    -- Dependency sources are handled by the same server
    local dep_client = h.wait_for_client(0, 30000)
    h.eq(state.client.id, dep_client.id)
  end)

  it("completes", function()
    focus_main()
    vim.api.nvim_buf_set_lines(state.bufnr, println_line, println_line + 1, false, { '    println(greeting("world").toUpp)' })
    local result = h.wait_for("completions", function()
      local resp = state.client:request_sync(
        "textDocument/completion",
        text_params(pos(println_line, "toUpp", 5)),
        30000,
        state.bufnr
      )
      local items = resp and resp.result and (resp.result.items or resp.result)
      return items and #items > 0 and items
    end, 60000)
    local labels = vim.tbl_map(function(item)
      return item.label
    end, result)
    h.truthy(
      vim.iter(labels):any(function(label)
        return label:find("toUpperCase", 1, true) ~= nil
      end),
      "toUpperCase in " .. vim.inspect(labels)
    )
    vim.api.nvim_buf_set_lines(state.bufnr, println_line, println_line + 1, false, { '    println(greeting("world"))' })
  end)

  it("reports compilation errors", function()
    focus_main()
    vim.api.nvim_buf_set_lines(state.bufnr, println_line, println_line + 1, false, { "    println(greeting(42))" })
    vim.cmd("silent write")
    vim.cmd("Plasmon compile")
    local diagnostics = h.wait_for("an error diagnostic", function()
      local diags = vim.diagnostic.get(state.bufnr, { severity = vim.diagnostic.severity.ERROR })
      return #diags > 0 and diags
    end, timeout)
    h.eq(println_line, diagnostics[1].lnum)

    vim.api.nvim_buf_set_lines(state.bufnr, println_line, println_line + 1, false, { '    println(greeting("world"))' })
    vim.cmd("silent write")
    vim.cmd("Plasmon compile")
    h.wait_for("errors to go away", function()
      return #vim.diagnostic.get(state.bufnr, { severity = vim.diagnostic.severity.ERROR }) == 0
    end, timeout)
  end)

  it("shows code lenses, without VS Code icons", function()
    focus_main()
    local lenses = h.wait_for("a super method code lens", function()
      local resp = state.client:request_sync(
        "textDocument/codeLens",
        { textDocument = { uri = vim.uri_from_bufnr(state.bufnr) } },
        30000,
        state.bufnr
      )
      local lenses = vim.tbl_filter(function(lens)
        return lens.range.start.line == greeting_line
      end, resp and resp.result or {})
      return #lenses > 0 and lenses
    end, 60000)
    local title = lenses[1].command.title
    h.contains(title, "greeting")
    h.truthy(not title:find("$(", 1, true), "no icon marker in " .. title)
  end)

  it("receives status and logs", function()
    focus_main()
    h.wait_for("a status summary", function()
      return plasmon.statusline() ~= "" and plasmon.statusline() ~= "Plasmon"
    end, 30000)
    h.truthy(#plasmon.status() > 0)
    h.truthy(handlers.logs["lsp-requests"] and #handlers.logs["lsp-requests"].lines > 0, "lsp-requests log")
    h.truthy(vim.uv.fs_stat(state.root .. "/.plasmon/log"), "server log in the workspace")
  end)

  it("stops, releasing the workspace lock", function()
    local client = state.client
    client:stop()
    h.wait_for("the client to stop", function()
      return client:is_stopped()
    end, 30000)
    h.wait_for("the lock to be released", function()
      local f = io.open(state.root .. "/.plasmon/lock")
      local content = f and f:read("*a") or ""
      if f then
        f:close()
      end
      return content == ""
    end, 30000)
    h.cleanup()
  end)
end)
