-- :Plasmon sub-commands

local M = {}

local function plasmon()
  return require("plasmon")
end
local handlers = require("plasmon.handlers")
local strip = handlers.strip_icons

local function info(msg)
  vim.notify("Plasmon: " .. msg, vim.log.levels.INFO)
end
local function err(msg)
  vim.notify("Plasmon: " .. msg, vim.log.levels.ERROR)
end

--- Client and current file for a command.
---@param opts? { client_id?: integer, bufnr?: integer }
---@return vim.lsp.Client?, string?
local function context(opts)
  local client = plasmon().get_client(opts)
  if not client then
    return nil, nil
  end
  return client, plasmon().current_uri(client, opts and opts.bufnr)
end

local function exec(client, command, arguments, on_result)
  plasmon().execute(command, arguments, { client_id = client.id, on_result = on_result })
end

--- Executes a command that applies to the current file.
local function file_command(command, extra_args, on_result)
  return function(opts)
    local client, uri = context(opts)
    if not client then
      return
    end
    if not uri then
      err("no Scala or Java file to run " .. command .. " on")
      return
    end
    local args = { uri }
    vim.list_extend(args, extra_args or {})
    exec(client, command, args, on_result)
  end
end

--- Executes a command that takes no arguments.
local function plain_command(command, on_result)
  return function(opts)
    local client = context(opts)
    if client then
      exec(client, command, {}, on_result)
    end
  end
end

local function check_error(what)
  return function(resp)
    if type(resp) == "table" and resp.error and resp.error ~= vim.NIL then
      err(("error %s: %s"):format(what, resp.error))
    end
  end
end

--- Opens text in a scratch buffer.
---@param name string
---@param text string
---@param filetype? string
local function show_text(name, text, filetype)
  local bufname = "plasmon://" .. name
  local existing = vim.fn.bufnr(bufname)
  local bufnr = existing ~= -1 and existing or vim.api.nvim_create_buf(true, true)
  if existing == -1 then
    vim.api.nvim_buf_set_name(bufnr, bufname)
  end
  vim.bo[bufnr].modifiable = true
  vim.api.nvim_buf_set_lines(bufnr, 0, -1, false, vim.split(text or "", "\n", { plain = true }))
  vim.bo[bufnr].modifiable = false
  vim.bo[bufnr].modified = false
  if filetype then
    vim.bo[bufnr].filetype = filetype
  end
  vim.cmd("split")
  vim.api.nvim_win_set_buf(0, bufnr)
  return bufnr
end

local function open_uri(uri)
  vim.cmd.split(vim.fn.fnameescape(vim.uri_to_fname(uri)))
end

----------------------------------------------------------------------------------------------
-- Build tools and modules
----------------------------------------------------------------------------------------------

---@param entry table entry of plasmon/listBuildTools, plasmon/listBuildToolsOrModules, …
local function format_entry(entry)
  local parts = {}
  if entry.alreadyAdded == true or entry.alreadyLoaded == true then
    table.insert(parts, "[loaded]")
  end
  if entry.type == "BuildTool" then
    table.insert(parts, "Build tool:")
  elseif entry.type == "Module" then
    table.insert(parts, "Module:")
  end
  local label = type(entry.label) == "string" and entry.label or "?"
  if label:find("$(tag)", 1, true) then
    table.insert(parts, "[recommended]")
  elseif label:find("$(warning)", 1, true) then
    table.insert(parts, "[unsupported]")
  end
  table.insert(parts, strip(label))
  local description = strip(entry.description)
  if description and description ~= "" and description ~= vim.NIL then
    table.insert(parts, "(" .. description .. ")")
  end
  local detail = strip(entry.detail)
  if detail and detail ~= "" and detail ~= vim.NIL then
    table.insert(parts, "- " .. detail)
  end
  return table.concat(parts, " ")
end
M.format_entry = format_entry

local function toggle_module(client, entry)
  local command = entry.alreadyLoaded == true and "plasmon/unloadModule" or "plasmon/loadModule"
  local what = entry.alreadyLoaded == true and "unloading" or "loading"
  exec(client, command, { entry.workspace, entry.server, entry.uri }, function(resp)
    if type(resp) == "table" and resp.error and resp.error ~= vim.NIL then
      err(("error %s module %s: %s"):format(what, strip(entry.label), resp.error))
    else
      info(("%s module %s"):format(entry.alreadyLoaded == true and "unloaded" or "loaded", strip(entry.label)))
    end
  end)
end

local function toggle_build_tool(client, entry, uri, on_loaded)
  if entry.alreadyAdded == true then
    exec(client, "plasmon/unloadBuildTool", { entry.discoverId, entry.id, uri }, function(resp)
      if type(resp) == "table" and resp.success == false then
        err("error unloading build tool: " .. tostring(resp.error))
      else
        info("unloaded " .. strip(entry.label))
      end
    end)
  else
    info("loading " .. strip(entry.label) .. "…")
    exec(client, "plasmon/loadBuildTool", { entry.discoverId, entry.id, uri }, function(resp)
      if type(resp) == "table" and resp.success == false then
        err("error loading build tool: " .. tostring(resp.error))
      else
        info("loaded " .. strip(entry.label))
        if on_loaded then
          on_loaded()
        end
      end
    end)
  end
end

--- Picks a build tool or a module to load or unload.
---@param opts? { client_id?: integer, only_modules?: boolean }
function M.load_build_tool_or_module(opts)
  opts = opts or {}
  local client, uri = context(opts)
  if not client then
    return
  end
  exec(client, "plasmon/listBuildToolsOrModules", { uri }, function(entries)
    entries = vim.tbl_filter(function(e)
      return not (opts.only_modules and e.type == "BuildTool")
    end, entries or {})
    if #entries == 0 then
      info(opts.only_modules and "no module found" or "no build tool or module found")
      return
    end
    handlers.select(entries, {
      prompt = opts.only_modules and "Load or unload module" or "Load or unload build tool or module",
      format_item = format_entry,
      kind = "plasmon.build_tool_or_module",
    }, function(entry)
      if not entry then
        return
      end
      if entry.type == "BuildTool" then
        toggle_build_tool(client, entry, uri, function()
          M.load_build_tool_or_module({ client_id = client.id, only_modules = true })
        end)
      elseif entry.type == "Module" then
        toggle_module(client, entry)
      end
    end)
  end)
end

--- Picks a build tool to load or unload, then offers to load its modules.
---@param opts? { client_id?: integer }
function M.load_build_tool(opts)
  local client, uri = context(opts)
  if not client then
    return
  end
  exec(client, "plasmon/listBuildTools", { uri }, function(entries)
    entries = entries or {}
    if #entries == 0 then
      info("no build tool found")
      return
    end
    handlers.select(entries, {
      prompt = "Load or unload build tool",
      format_item = format_entry,
      kind = "plasmon.build_tool",
    }, function(entry)
      if entry then
        toggle_build_tool(client, entry, uri, function()
          M.load_build_tool_or_module({ client_id = client.id, only_modules = true })
        end)
      end
    end)
  end)
end

--- Picks a module of the current file to load or unload.
---@param opts? { client_id?: integer }
function M.load_module(opts)
  local client, uri = context(opts)
  if not client then
    return
  end
  exec(client, "plasmon/listModulesOf", { uri }, function(entries)
    entries = vim.tbl_filter(function(e)
      return e.type == "Module"
    end, entries or {})
    if #entries == 0 then
      info("no module found for this file")
      return
    end
    handlers.select(entries, {
      prompt = "Load or unload module",
      format_item = format_entry,
      kind = "plasmon.module",
    }, function(entry)
      if entry then
        toggle_module(client, entry)
      end
    end)
  end)
end

---@param opts? { client_id?: integer }
function M.build_tool_restart(opts)
  local client, uri = context(opts)
  if not client then
    return
  end
  exec(client, "plasmon/buildToolRestart", { uri }, function(resp)
    resp = resp or {}
    if resp.noModule == true then
      exec(client, "plasmon/listRunningBuildTools", {}, function(tools)
        tools = tools or {}
        if #tools == 0 then
          info("no build tools are running")
          return
        end
        handlers.select(tools, {
          prompt = "Unload build tool",
          format_item = format_entry,
          kind = "plasmon.running_build_tool",
        }, function(entry)
          if entry then
            toggle_build_tool(client, vim.tbl_extend("force", entry, { alreadyAdded = true }), uri)
          end
        end)
      end)
    elseif resp.error and resp.error ~= vim.NIL then
      err("error restarting / reconnecting to build server: " .. resp.error)
    else
      info("restarted / reconnected to build server")
    end
  end)
end

----------------------------------------------------------------------------------------------
-- Other commands
----------------------------------------------------------------------------------------------

local function pick_and_run(list_command, label_of, detail_of, command_of, prompt)
  return function(opts)
    local client, uri = context(opts)
    if not client then
      return
    end
    exec(client, list_command, { uri }, function(entries)
      entries = entries or {}
      if #entries == 0 then
        info("nothing to do")
        return
      end
      handlers.select(entries, {
        prompt = prompt,
        format_item = function(e)
          local detail = detail_of(e)
          local label = strip(label_of(e)) or "?"
          if detail and detail ~= vim.NIL and detail ~= "" then
            return label .. " - " .. strip(detail)
          end
          return label
        end,
      }, function(entry)
        if entry then
          local command, args = command_of(entry)
          exec(client, command, args)
        end
      end)
    end)
  end
end

local function show_status(opts)
  local items = plasmon().status(opts)
  if #items == 0 then
    info("no status reported yet")
    return
  end
  handlers.select(items, {
    prompt = "Plasmon status",
    format_item = function(item)
      local marks = { [1] = "[warning] ", [2] = "[error] " }
      local text = (marks[item.severity] or "") .. (strip(item.text) or "")
      if item.busy then
        text = text .. " …"
      end
      if item.detail and item.detail ~= "" then
        text = text .. " - " .. strip(item.detail)
      end
      if item.command then
        text = text .. " [" .. (strip(item.command.title) or item.command.command) .. "]"
      end
      return text
    end,
    kind = "plasmon.status",
  }, function(item)
    if item and item.command then
      local client = plasmon().get_client(opts)
      if client then
        client:exec_cmd(item.command, { bufnr = vim.api.nvim_get_current_buf() })
      end
    end
  end)
end

local function show_log(opts)
  local channel = opts.args and opts.args[1]
  if channel then
    handlers.show_log(channel)
    return
  end
  local ids = vim.tbl_keys(handlers.logs)
  table.sort(ids)
  if #ids == 0 then
    info("no logs received yet")
    return
  end
  handlers.select(ids, {
    prompt = "Plasmon log",
    format_item = function(id)
      return handlers.logs[id].label .. " (" .. id .. ")"
    end,
  }, function(id)
    if id then
      handlers.show_log(id)
    end
  end)
end

local function server_log(opts)
  local client = plasmon().get_client(opts)
  if not client then
    return
  end
  local root = client.config.cmd_cwd or client.config.root_dir
  local path = root and vim.fs.joinpath(root, ".plasmon", "log")
  if not path or not vim.uv.fs_stat(path) then
    err("no server log found" .. (path and (" at " .. path) or ""))
    return
  end
  vim.cmd.split(vim.fn.fnameescape(path))
  vim.bo.autoread = true
  vim.cmd("normal! G")
end

local function restart(opts)
  local clients = {}
  local client = plasmon().get_client(vim.tbl_extend("force", opts, { silent = true }))
  if client then
    table.insert(clients, client)
  else
    clients = vim.lsp.get_clients({ name = "plasmon" })
  end
  if #clients == 0 then
    -- Nothing running: start for the current buffer, if it qualifies
    vim.cmd("doautocmd <nomodeline> FileType")
    return
  end
  for _, c in ipairs(clients) do
    local bufs = vim.tbl_keys(c.attached_buffers)
    local cfg = c.config
    c:stop()
    local timer = assert(vim.uv.new_timer())
    local tries = 0
    timer:start(
      100,
      100,
      vim.schedule_wrap(function()
        tries = tries + 1
        if not c:is_stopped() and tries < 100 then
          if tries == 50 then
            c:stop(true)
          end
          return
        end
        timer:stop()
        timer:close()
        for _, bufnr in ipairs(bufs) do
          if vim.api.nvim_buf_is_valid(bufnr) then
            vim.lsp.start(cfg, { bufnr = bufnr })
          end
        end
      end)
    )
  end
end

local function check_concurrent_server(opts)
  local client = plasmon().get_client(vim.tbl_extend("force", opts, { silent = true }))
  local root = client and (client.config.cmd_cwd or client.config.root_dir)
  if not root then
    local name = vim.api.nvim_buf_get_name(0)
    root = name ~= "" and require("plasmon.config").find_root(name) or vim.uv.cwd()
  end
  local pid = require("plasmon.config").lock_holder(root)
  if not pid then
    info("no other Plasmon server running in " .. root)
    return
  end
  handlers.select({ "Stop it", "Leave it" }, {
    prompt = ("Another Plasmon server (PID %d) is running in %s"):format(pid, root),
  }, function(choice)
    if choice ~= "Stop it" then
      return
    end
    pcall(vim.uv.kill, pid, "sigint")
    vim.defer_fn(function()
      if require("plasmon.config").is_alive(pid) then
        pcall(vim.uv.kill, pid, "sigkill")
      end
    end, 2000)
  end)
end

local function checktime()
  vim.schedule(function()
    vim.cmd("checktime")
  end)
end

local function dump(command, extra_args, render)
  return file_command(command, extra_args, function(resp)
    if type(resp) ~= "table" then
      return
    end
    if resp.error and resp.error ~= vim.NIL then
      err(command .. ": " .. resp.error)
      return
    end
    render(resp)
  end)
end

---@type table<string, fun(opts: { args: string[], client_id?: integer })>
M.subcommands = {
  ["load-build-tool"] = M.load_build_tool,
  ["load-build-tool-or-module"] = M.load_build_tool_or_module,
  ["load-module"] = M.load_module,
  ["unload-all-modules"] = plain_command("plasmon/unloadAllModules"),
  ["unload-all-build-tools"] = plain_command("plasmon/unloadAllBuildTools"),
  ["build-tool-restart"] = M.build_tool_restart,
  ["build-tool-restart-or-reconnect"] = file_command(
    "plasmon/buildToolRestartOrReconnect",
    nil,
    function(resp)
      if type(resp) == "table" and resp.error and resp.error ~= vim.NIL then
        err("error restarting / reconnecting to build server: " .. resp.error)
      else
        info("restarted / reconnected to build server")
      end
    end
  ),
  ["re-index"] = plain_command("plasmon/index"),
  ["re-import"] = file_command("plasmon/reImport", nil, function(resp)
    if type(resp) ~= "table" then
      return
    end
    if resp.error and resp.error ~= vim.NIL then
      err("error re-importing build: " .. resp.error)
    elseif resp.message and resp.message ~= vim.NIL then
      info(resp.message .. (resp.logId and resp.logId ~= vim.NIL and (" (:Plasmon log " .. resp.logId .. ")") or ""))
    end
  end),
  ["compile"] = file_command("plasmon/compile"),
  ["compile-all"] = plain_command("plasmon/compileAll"),
  ["clean"] = file_command("plasmon/clean", nil, check_error("cleaning module")),
  ["clean-all"] = plain_command("plasmon/cleanAll"),
  ["cancel-all-compilations"] = plain_command("plasmon/cancelAllCompilations"),
  ["organize-imports"] = file_command("plasmon/organizeImports", nil, function(resp)
    if type(resp) == "table" and resp.error and resp.error ~= vim.NIL then
      err("error organizing imports: " .. resp.error)
    elseif type(resp) == "table" and resp.fileChanged == true then
      checktime()
      info("organized imports")
    else
      info("imports were already organized")
    end
  end),
  ["organize-imports-in-module"] = file_command("plasmon/organizeImportsInModule", nil, function(resp)
    if type(resp) == "table" and resp.error and resp.error ~= vim.NIL then
      err("error organizing imports for module: " .. resp.error)
    elseif type(resp) == "table" and type(resp.fileChanged) == "number" and resp.fileChanged > 0 then
      checktime()
      info(("organized imports (%d file(s) changed)"):format(resp.fileChanged))
    else
      info("imports were already organized in module")
    end
  end),
  ["enable-pc-diagnostics"] = function(opts)
    local client = context(opts)
    if client then
      exec(client, "plasmon/enablePcDiagnostics", { true })
    end
  end,
  ["disable-pc-diagnostics"] = function(opts)
    local client = context(opts)
    if client then
      exec(client, "plasmon/enablePcDiagnostics", { false })
    end
  end,
  ["toggle-pc-diagnostics"] = plain_command("plasmon/togglePcDiagnostics"),
  ["module-actions"] = pick_and_run("plasmon/listModuleActions", function(e)
    return e.label
  end, function(e)
    return e.detail
  end, function(e)
    return e.command, e.arguments
  end, "Module actions"),
  ["interactive-compiler"] = pick_and_run("plasmon/interactiveCompilerActions", function(e)
    return e.text
  end, function(e)
    return e.description
  end, function(e)
    return e.command, e.commandArgs
  end, "Interactive compiler"),
  ["status"] = show_status,
  ["log"] = show_log,
  ["server-log"] = server_log,
  ["restart"] = restart,
  ["check-concurrent-server"] = check_concurrent_server,
  ["dump-server-state"] = plain_command("plasmon/debugServerState", function(resp)
    if type(resp) ~= "table" then
      return
    end
    if resp.contentUri and resp.contentUri ~= vim.NIL then
      open_uri(resp.contentUri)
    else
      show_text("server-state.json", resp.text, "json")
    end
  end),
  ["dump-bsp-data"] = dump("plasmon/debugBspData", nil, function(resp)
    show_text("bsp-data", resp.data)
  end),
  ["dump-typed-tree"] = dump("plasmon/debugFullTree", nil, function(resp)
    show_text("typed-tree.scala", (resp.fullTree or "") .. "\n\n/*\n\n" .. (resp.diagnostics or "") .. "\n\n*/", "scala")
  end),
  ["dump-symbol-index"] = dump("plasmon/debugSymbolIndex", nil, function(resp)
    open_uri(resp.contentUri)
  end),
  ["dump-semanticdb-details"] = dump("plasmon/debugSemanticdbLookup", { true }, function(resp)
    show_text("semanticdb-details", resp.text)
  end),
  ["dump-compact-semanticdb-details"] = dump("plasmon/debugSemanticdbLookup", { false }, function(resp)
    show_text("semanticdb-compact-details", resp.text)
  end),
}

M.aliases = {
  load = "load-build-tool-or-module",
  reindex = "re-index",
  reimport = "re-import",
}

function M.names()
  local names = vim.tbl_keys(M.subcommands)
  vim.list_extend(names, vim.tbl_keys(M.aliases))
  table.sort(names)
  return names
end

--- Entry point of :Plasmon
---@param cmd_opts { fargs: string[] }
function M.run(cmd_opts)
  local name = cmd_opts.fargs[1]
  if not name then
    show_status({ args = {} })
    return
  end
  name = M.aliases[name] or name
  local fn = M.subcommands[name]
  if not fn then
    err("unknown command " .. name)
    return
  end
  fn({ args = vim.list_slice(cmd_opts.fargs, 2) })
end

function M.complete(arg_lead, cmd_line, _)
  local words = vim.split(cmd_line, "%s+", { trimempty = true })
  local n = #words
  if cmd_line:match("%s$") then
    n = n + 1
  end
  if n <= 2 then
    return vim.tbl_filter(function(name)
      return vim.startswith(name, arg_lead)
    end, M.names())
  end
  if words[2] == "log" and n == 3 then
    local ids = vim.tbl_keys(handlers.logs)
    table.sort(ids)
    return vim.tbl_filter(function(id)
      return vim.startswith(id, arg_lead)
    end, ids)
  end
  return {}
end

return M
