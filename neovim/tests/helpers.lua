-- Test helpers

local M = {}

M.tests_dir = vim.fs.dirname(vim.fs.normalize(vim.fn.fnamemodify(debug.getinfo(1, "S").source:sub(2), ":p")))
M.plugin_dir = vim.fs.dirname(M.tests_dir)

--- Waits for `cond` to return a truthy value, failing after `timeout_ms`.
---@generic T
---@param what string
---@param cond fun(): T
---@param timeout_ms? integer
---@return T
function M.wait_for(what, cond, timeout_ms)
  local value
  local ok = vim.wait(timeout_ms or 10000, function()
    value = cond()
    return value
  end, 20)
  if not ok then
    error("Timed out waiting for " .. what, 2)
  end
  return value
end

function M.eq(expected, actual, msg)
  if not vim.deep_equal(expected, actual) then
    error(
      ("%sexpected %s, got %s"):format(msg and (msg .. ": ") or "", vim.inspect(expected), vim.inspect(actual)),
      2
    )
  end
end

function M.truthy(value, msg)
  if not value then
    error(msg or "expected a truthy value", 2)
  end
  return value
end

function M.contains(haystack, needle, msg)
  if type(haystack) ~= "string" or not haystack:find(needle, 1, true) then
    error(("%sexpected %s to contain %s"):format(msg and (msg .. ": ") or "", vim.inspect(haystack), vim.inspect(needle)), 2)
  end
end

--- New empty directory, removed by cleanup().
local tmp_dirs = {}
function M.tmpdir()
  local dir = vim.fn.tempname()
  vim.fn.mkdir(dir, "p")
  dir = vim.fs.normalize(assert(vim.uv.fs_realpath(dir)))
  table.insert(tmp_dirs, dir)
  return dir
end

---@param path string
---@param content? string
function M.write(path, content)
  vim.fn.mkdir(vim.fs.dirname(path), "p")
  local f = assert(io.open(path, "w"))
  f:write(content or "")
  f:close()
end

--- Calls to vim.notify, recorded rather than printed.
M.notifications = {}
local original_notify = vim.notify
function M.capture_notifications()
  M.notifications = {}
  vim.notify = function(msg, level)
    table.insert(M.notifications, { msg = msg, level = level })
  end
end

function M.notified(pattern)
  for _, n in ipairs(M.notifications) do
    if n.msg:find(pattern, 1, true) then
      return n
    end
  end
  return nil
end

--- Makes vim.ui.select pick items with `choose(items, opts)` (index or item), recording calls.
M.selects = {}
local original_select = vim.ui.select
function M.stub_select(choose)
  M.selects = {}
  vim.ui.select = function(items, opts, on_choice)
    local formatted = {}
    for i, item in ipairs(items) do
      formatted[i] = opts.format_item and opts.format_item(item) or tostring(item)
    end
    table.insert(M.selects, { items = items, formatted = formatted, prompt = opts.prompt })
    local choice = choose(items, opts, formatted)
    if type(choice) == "number" then
      choice = items[choice]
    end
    vim.schedule(function()
      on_choice(choice, choice and vim.fn.index(items, choice) + 1 or nil)
    end)
  end
end

function M.restore()
  vim.notify = original_notify
  vim.ui.select = original_select
end

--- Stops all LSP clients, wipes buffers, removes temporary directories.
function M.cleanup()
  M.restore()
  -- Let in-flight requests (code lenses, …) complete before buffers go away
  vim.wait(50)
  local clients = vim.lsp.get_clients()
  for _, client in ipairs(clients) do
    client:stop()
  end
  vim.wait(5000, function()
    for _, client in ipairs(clients) do
      if not client:is_stopped() then
        return false
      end
    end
    return true
  end, 20)
  for _, client in ipairs(clients) do
    if not client:is_stopped() then
      client:stop(true)
    end
  end
  vim.cmd("silent! %bwipeout!")
  for _, dir in ipairs(tmp_dirs) do
    vim.fn.delete(dir, "rf")
  end
  tmp_dirs = {}
end

--- Waits for a Plasmon client attached to a buffer.
---@param bufnr? integer
---@param timeout_ms? integer
---@return vim.lsp.Client
function M.wait_for_client(bufnr, timeout_ms)
  bufnr = bufnr or vim.api.nvim_get_current_buf()
  local client = M.wait_for("a Plasmon client to attach", function()
    local c = vim.lsp.get_clients({ name = "plasmon", bufnr = bufnr })[1]
    return c and c.initialized and c
  end, timeout_ms)
  return client
end

--- Synchronous request, failing on errors and timeouts.
function M.request(client, method, params, timeout_ms, bufnr)
  local resp, err = client:request_sync(method, params or {}, timeout_ms or 10000, bufnr or 0)
  if not resp then
    error(("%s failed: %s"):format(method, tostring(err)), 2)
  end
  if resp.err then
    error(("%s failed: %s"):format(method, vim.inspect(resp.err)), 2)
  end
  return resp.result
end

return M
