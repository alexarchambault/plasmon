-- A fake Plasmon server, speaking LSP over stdin / stdout. Run with `nvim -l fake_server.lua`.
--
-- It records everything it receives, answers Plasmon commands with canned responses, and
-- has a few extra methods for tests:
-- - `fake/received`: messages received so far (`{ method, params }` list)
-- - `fake/info`: `{ cwd, args }` of this process
-- - `fake/notify`: `{ method, params }`, sent back to the client as a notification

local received = {}

local function read_message()
  local length
  while true do
    local line = io.stdin:read("*l")
    if not line then
      return nil
    end
    line = line:gsub("\r$", "")
    if line == "" then
      break
    end
    local value = line:match("^Content%-Length: (%d+)")
    if value then
      length = tonumber(value)
    end
  end
  if not length then
    return nil
  end
  local body = io.stdin:read(length)
  if not body then
    return nil
  end
  return vim.json.decode(body, { luanil = { object = false, array = false } })
end

local function send(msg)
  msg.jsonrpc = "2.0"
  local body = vim.json.encode(msg)
  io.stdout:write(("Content-Length: %d\r\n\r\n%s"):format(#body, body))
  io.stdout:flush()
end

local commands = {
  "plasmon/listBuildTools",
  "plasmon/listBuildToolsOrModules",
  "plasmon/listModulesOf",
  "plasmon/loadBuildTool",
  "plasmon/unloadBuildTool",
  "plasmon/loadModule",
  "plasmon/unloadModule",
  "plasmon/compile",
  "plasmon/index",
  "plasmon/organizeImports",
  "plasmon/togglePcDiagnostics",
  "plasmon/clientHeartBeat",
  "plasmon/debugServerState",
}

local root_uri

local function command_result(command, args)
  if command == "plasmon/listBuildTools" then
    return {
      {
        type = "BuildTool",
        id = "scala-cli-abc",
        discoverId = "scala-cli",
        label = "Scala CLI for Main.scala",
        detail = "Load build tool",
        alreadyAdded = false,
      },
      {
        type = "BuildTool",
        id = "scala-cli",
        discoverId = "scala-cli",
        label = "Scala CLI in this workspace",
        detail = "Load build tool",
        alreadyAdded = false,
      },
    }
  elseif command == "plasmon/listBuildToolsOrModules" or command == "plasmon/listModulesOf" then
    return {
      {
        type = "Module",
        label = "$(tag) proj_123",
        description = "Scala CLI, Scala 3.8.3, JVM",
        detail = "Load module",
        server = "scala-cli",
        uri = "file:/proj/.scala-build/?id=proj_123",
        workspace = root_uri,
        alreadyLoaded = false,
      },
    }
  elseif command == "plasmon/loadBuildTool" or command == "plasmon/unloadBuildTool" then
    return { success = true }
  elseif command == "plasmon/organizeImports" then
    return { fileChanged = false }
  elseif command == "plasmon/debugServerState" then
    return { id = "1", text = '{ "state": "fake" }' }
  end
  return vim.NIL
end

while true do
  local msg = read_message()
  if not msg then
    break
  end
  if msg.method then
    table.insert(received, { method = msg.method, params = msg.params })
  end
  local method, id, params = msg.method, msg.id, msg.params
  if method == "exit" then
    break
  elseif id ~= nil and method ~= nil then
    local result = vim.NIL
    if method == "initialize" then
      root_uri = params.rootUri
      result = {
        capabilities = {
          textDocumentSync = { openClose = true, change = 1, save = { includeText = true } },
          hoverProvider = true,
          codeLensProvider = { resolveProvider = false },
          executeCommandProvider = { commands = commands },
        },
      }
    elseif method == "workspace/executeCommand" then
      result = command_result(params.command, params.arguments)
    elseif method == "textDocument/codeLens" then
      result = {}
    elseif method == "fake/received" then
      result = received
    elseif method == "fake/info" then
      result = { cwd = vim.uv.cwd(), args = _G.arg }
    elseif method == "fake/notify" then
      send({ method = params.method, params = params.params })
    end
    send({ id = id, result = result })
  end
end
