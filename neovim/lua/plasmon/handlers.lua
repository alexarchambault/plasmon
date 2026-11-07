-- Handlers for Plasmon's LSP extensions, and the client-side commands it refers to

local M = {}

--- vim.ui.select(), redrawing before calling `on_choice`: with the default implementation
--- (inputlist()), a message sent right after picking something would otherwise trigger a
--- "Press ENTER" prompt.
---@generic T
---@param items T[]
---@param opts table
---@param on_choice fun(item: T?, idx: integer?)
function M.select(items, opts, on_choice)
  vim.ui.select(items, opts, function(item, idx)
    vim.cmd("redraw")
    on_choice(item, idx)
  end)
end

local function config()
  return require("plasmon.config").options
end

--- `$(icon)` markers are VS Code codicons, meaningless here.
---@param s? string
---@return string?
function M.strip_icons(s)
  if type(s) ~= "string" then
    return s
  end
  local stripped = s:gsub("%$%([%w%-~]+%)%s*", ""):gsub("%s+$", "")
  return stripped
end

----------------------------------------------------------------------------------------------
-- Status
----------------------------------------------------------------------------------------------

---@class plasmon.StatusItem
---@field id string
---@field text string
---@field severity integer 0: ok, 1: warning, 2: error
---@field busy boolean
---@field detail? string
---@field command? { title: string, command: string, tooltip?: string, arguments?: any[] }

---@type table<integer, { uri: string?, items: table<string, plasmon.StatusItem>, order: string[] }>
M.status = {}

local function status_of(client_id)
  local st = M.status[client_id]
  if not st then
    st = { uri = nil, items = {}, order = {} }
    M.status[client_id] = st
  end
  return st
end

local function fire(pattern, data)
  vim.api.nvim_exec_autocmds("User", { pattern = pattern, modeline = false, data = data })
end

local function null_to_nil(v)
  if v == vim.NIL then
    return nil
  end
  return v
end

--- `plasmon/statusUpdate`: params are `[uri, updates]`
function M.status_update(_, params, ctx)
  if type(params) ~= "table" then
    return
  end
  local uri, updates = params[1], params[2]
  local st = status_of(ctx.client_id)
  st.uri = uri ~= "" and uri or nil
  for _, update in ipairs(updates or {}) do
    local id = update.id
    local text = null_to_nil(update.text)
    if text and text ~= "" then
      if not st.items[id] then
        table.insert(st.order, id)
      end
      st.items[id] = {
        id = id,
        text = text,
        severity = null_to_nil(update.severity) or 0,
        busy = null_to_nil(update.busy) == true,
        detail = null_to_nil(update.detail),
        command = null_to_nil(update.command),
      }
    elseif st.items[id] then
      st.items[id] = nil
      st.order = vim.tbl_filter(function(other)
        return other ~= id
      end, st.order)
    end
  end
  fire("PlasmonStatus", { client_id = ctx.client_id, uri = st.uri })
  vim.cmd.redrawstatus({ bang = true, mods = { silent = true } })
end

----------------------------------------------------------------------------------------------
-- Logs
----------------------------------------------------------------------------------------------

---@type table<string, { label: string, lines: string[], bufs: table<integer, boolean> }>
M.logs = {}

local function append_to_buf(bufnr, lines)
  if not vim.api.nvim_buf_is_valid(bufnr) then
    return false
  end
  vim.bo[bufnr].modifiable = true
  local count = vim.api.nvim_buf_line_count(bufnr)
  local first = vim.api.nvim_buf_get_lines(bufnr, 0, 1, false)[1]
  if count == 1 and first == "" then
    vim.api.nvim_buf_set_lines(bufnr, 0, 1, false, lines)
  else
    vim.api.nvim_buf_set_lines(bufnr, count, count, false, lines)
  end
  vim.bo[bufnr].modifiable = false
  vim.bo[bufnr].modified = false
  -- Follow the end of the log in windows whose cursor is on the last line
  for _, win in ipairs(vim.fn.win_findbuf(bufnr)) do
    if vim.api.nvim_win_get_cursor(win)[1] >= count then
      vim.api.nvim_win_set_cursor(win, { vim.api.nvim_buf_line_count(bufnr), 0 })
    end
  end
  return true
end

--- `plasmon/log`: `{ channelId, channelLabel, lines }`
function M.log(_, params, _)
  if type(params) ~= "table" or not params.channelId then
    return
  end
  local channel = M.logs[params.channelId]
  if not channel then
    channel = { label = params.channelLabel or params.channelId, lines = {}, bufs = {} }
    M.logs[params.channelId] = channel
  end
  local new_lines = {}
  for _, line in ipairs(params.lines or {}) do
    -- Lines can embed new lines, and the server's ANSI color codes
    for _, l in ipairs(vim.split((line:gsub("\27%[[%d;]*m", "")), "\n", { plain = true })) do
      table.insert(new_lines, l)
    end
  end
  vim.list_extend(channel.lines, new_lines)
  local max = config().max_log_lines or 10000
  if #channel.lines > max then
    channel.lines = vim.list_slice(channel.lines, #channel.lines - max + 1)
  end
  for bufnr in pairs(channel.bufs) do
    if not append_to_buf(bufnr, new_lines) then
      channel.bufs[bufnr] = nil
    end
  end
end

--- Opens (or focuses) a buffer following a log channel.
---@param channel_id string
function M.show_log(channel_id)
  local channel = M.logs[channel_id]
  if not channel then
    local known = vim.tbl_keys(M.logs)
    table.sort(known)
    vim.notify(
      ("Plasmon: no log named %s (known logs: %s)"):format(channel_id, table.concat(known, ", ")),
      vim.log.levels.WARN
    )
    return
  end
  local name = "plasmon://log/" .. channel_id
  for bufnr in pairs(channel.bufs) do
    if vim.api.nvim_buf_is_valid(bufnr) then
      local wins = vim.fn.win_findbuf(bufnr)
      if #wins > 0 then
        vim.api.nvim_set_current_win(wins[1])
      else
        vim.cmd("split")
        vim.api.nvim_win_set_buf(0, bufnr)
      end
      return bufnr
    end
  end
  local bufnr = vim.api.nvim_create_buf(false, true)
  vim.api.nvim_buf_set_name(bufnr, name)
  vim.bo[bufnr].bufhidden = "hide"
  vim.bo[bufnr].filetype = "plasmonlog"
  append_to_buf(bufnr, vim.deepcopy(channel.lines))
  channel.bufs[bufnr] = true
  vim.cmd("split")
  vim.api.nvim_win_set_buf(0, bufnr)
  vim.api.nvim_win_set_cursor(0, { vim.api.nvim_buf_line_count(bufnr), 0 })
  return bufnr
end

----------------------------------------------------------------------------------------------
-- Progress
----------------------------------------------------------------------------------------------

---@type table<string, boolean>
local in_progress = {}

--- `plasmon/progress`, reported as standard LSP progress (`LspProgress`, `vim.lsp.status()`)
function M.progress(_, params, ctx)
  if not config().notify_progress or type(params) ~= "table" then
    return
  end
  local token = ("plasmon/%s/%s"):format(params.buildToolId or "", params.requestId or "")
  local key = ctx.client_id .. ":" .. token
  local value
  if null_to_nil(params.done) then
    if not in_progress[key] then
      return
    end
    in_progress[key] = nil
    value = { kind = "end" }
  elseif not in_progress[key] then
    in_progress[key] = true
    value = {
      kind = "begin",
      title = ("%s: %s"):format(params.buildToolName or "Plasmon", params.request or ""),
    }
  else
    local progress = null_to_nil(params.progress)
    if not progress then
      return
    end
    value = { kind = "report", percentage = math.floor(progress * 100 + 0.5) }
  end
  local handler = vim.lsp.handlers["$/progress"]
  if handler then
    handler(nil, { token = token, value = value }, ctx)
  end
end

----------------------------------------------------------------------------------------------
-- Build changes
----------------------------------------------------------------------------------------------

M.build_change_choice = nil ---@type "reindex"|"ignore"|nil
local build_change_prompt_pending = false

function M.build_change_detected(_, _, ctx)
  local mode = M.build_change_choice or config().on_build_change
  local function reindex()
    require("plasmon").execute("plasmon/index", {}, { client_id = ctx.client_id })
  end
  if mode == "reindex" then
    reindex()
  elseif mode == "ignore" then
    return
  elseif not build_change_prompt_pending then
    build_change_prompt_pending = true
    local choices = { "Re-index", "Always re-index", "Dismiss", "Dismiss all" }
    M.select(choices, { prompt = "Plasmon: build change detected" }, function(choice)
      build_change_prompt_pending = false
      if choice == "Always re-index" then
        M.build_change_choice = "reindex"
      elseif choice == "Dismiss all" then
        M.build_change_choice = "ignore"
      end
      if choice == "Re-index" or choice == "Always re-index" then
        reindex()
      end
    end)
  end
end

----------------------------------------------------------------------------------------------
-- Commands the server asks the client to run
----------------------------------------------------------------------------------------------

--- `plasmon/executeClientCommand`: `{ command, arguments }`
function M.execute_client_command(_, params, ctx)
  if type(params) ~= "table" or not params.command then
    return
  end
  local fn = M.client_commands[params.command]
  if fn then
    fn({ command = params.command, arguments = params.arguments }, ctx)
  else
    vim.notify(("Plasmon: unsupported client command %s"):format(params.command), vim.log.levels.WARN)
  end
end

--- `plasmon/heartBeat`: only sent when the server is started with `--heartbeat`
function M.heart_beat(_, _, ctx)
  local client = vim.lsp.get_client_by_id(ctx.client_id)
  if client then
    client:request("workspace/executeCommand", { command = "plasmon/clientHeartBeat", arguments = {} }, function() end)
  end
end

--- Client-side commands, referenced by code lenses, status items and `plasmon/executeClientCommand`.
---@type table<string, fun(command: lsp.Command, ctx: table)>
M.client_commands = {
  ["plasmon-goto-location"] = function(command, ctx)
    local location = command.arguments and command.arguments[1]
    if type(location) ~= "table" or not location.uri then
      return
    end
    local client = vim.lsp.get_client_by_id(ctx.client_id)
    if location.otherWindow then
      vim.cmd("split")
    end
    vim.lsp.util.show_document(
      { uri = location.uri, range = location.range },
      client and client.offset_encoding or "utf-16",
      { focus = true }
    )
  end,
  ["plasmon.show-log"] = function(command, _)
    local channel = command.arguments and command.arguments[1]
    if channel then
      M.show_log(channel)
    end
  end,
  ["plasmon.load-build-tool-or-module"] = function(_, ctx)
    require("plasmon.commands").load_build_tool_or_module({ client_id = ctx.client_id })
  end,
  ["plasmon.build-tool-restart"] = function(_, ctx)
    require("plasmon.commands").build_tool_restart({ client_id = ctx.client_id })
  end,
  ["plasmon.toggle-pc-diagnostics"] = function(_, ctx)
    require("plasmon").execute("plasmon/togglePcDiagnostics", {}, { client_id = ctx.client_id })
  end,
}

M.handlers = {
  ["plasmon/statusUpdate"] = M.status_update,
  ["plasmon/log"] = M.log,
  ["plasmon/progress"] = M.progress,
  ["plasmon/buildChangeDetected"] = M.build_change_detected,
  ["plasmon/executeClientCommand"] = M.execute_client_command,
  ["plasmon/heartBeat"] = M.heart_beat,
}

----------------------------------------------------------------------------------------------
-- Attaching to buffers
----------------------------------------------------------------------------------------------

---@type table<integer, string>
M.last_focused = {}

--- Tells the server which file is being looked at. The server relies on it for its status, and
--- for commands not passed any file.
---@param client vim.lsp.Client
---@param bufnr integer
function M.focus(client, bufnr)
  local uri = vim.uri_from_bufnr(bufnr)
  if M.last_focused[client.id] == uri then
    return
  end
  M.last_focused[client.id] = uri
  client:notify("metals/didFocusTextDocument", { uri })
end

local group = vim.api.nvim_create_augroup("plasmon", { clear = false })
local window_state_set_up = false

local function set_up_window_state()
  if window_state_set_up then
    return
  end
  window_state_set_up = true
  vim.api.nvim_create_autocmd({ "FocusGained", "FocusLost" }, {
    group = group,
    callback = function(ev)
      for _, client in ipairs(vim.lsp.get_clients({ name = "plasmon" })) do
        client:notify("metals/windowStateDidChange", { ev.event == "FocusGained" })
      end
    end,
  })
end

local has_codelens_enable = vim.lsp.codelens.enable ~= nil

---@param client vim.lsp.Client
---@param bufnr integer
function M.on_attach(client, bufnr)
  set_up_window_state()
  local opts = config()

  if bufnr == vim.api.nvim_get_current_buf() then
    M.focus(client, bufnr)
  end
  vim.api.nvim_clear_autocmds({ group = group, buffer = bufnr })
  vim.api.nvim_create_autocmd("BufEnter", {
    group = group,
    buffer = bufnr,
    callback = function(ev)
      local c = vim.lsp.get_client_by_id(client.id)
      if c then
        M.focus(c, ev.buf)
      end
    end,
  })

  if opts.codelens then
    if has_codelens_enable then
      vim.lsp.codelens.enable(true, { client_id = client.id })
    else
      vim.lsp.codelens.refresh({ bufnr = bufnr })
      vim.api.nvim_create_autocmd({ "BufEnter", "InsertLeave", "BufWritePost" }, {
        group = group,
        buffer = bufnr,
        callback = function(ev)
          vim.lsp.codelens.refresh({ bufnr = ev.buf })
        end,
      })
    end
  end

  if opts.on_type_formatting and vim.lsp.on_type_formatting then
    vim.lsp.on_type_formatting.enable(true, { client_id = client.id })
  end
end

---@param client_id integer
function M.on_exit(client_id)
  M.status[client_id] = nil
  M.last_focused[client_id] = nil
  local prefix = client_id .. ":"
  for key in pairs(in_progress) do
    if vim.startswith(key, prefix) then
      in_progress[key] = nil
    end
  end
  vim.schedule(function()
    fire("PlasmonStatus", { client_id = client_id })
  end)
end

return M
