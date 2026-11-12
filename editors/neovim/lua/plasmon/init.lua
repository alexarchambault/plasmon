-- Neovim support for Plasmon, the Scala language server
--
-- Quick start:
--   require("plasmon").setup()      -- or just: vim.lsp.enable("plasmon")
--
-- See :help plasmon

local M = {}

local config = require("plasmon.config")
local handlers = require("plasmon.handlers")

local initialized = false

--- Defines :Plasmon, and sets up buffers Plasmon attaches to. Done by plugin/plasmon.lua, and
--- by setup() in case the plugin was added to 'runtimepath' too late for that.
function M.init()
  if initialized then
    return
  end
  initialized = true

  vim.api.nvim_create_user_command("Plasmon", function(opts)
    require("plasmon.commands").run(opts)
  end, {
    nargs = "*",
    desc = "Plasmon commands (no argument: show status)",
    complete = function(...)
      return require("plasmon.commands").complete(...)
    end,
  })

  vim.api.nvim_create_autocmd("LspAttach", {
    group = vim.api.nvim_create_augroup("plasmon.attach", { clear = true }),
    callback = function(ev)
      local client = vim.lsp.get_client_by_id(ev.data.client_id)
      if client and client.name == "plasmon" then
        handlers.on_attach(client, ev.buf)
      end
    end,
  })
end

--- Configures and enables the `plasmon` LSP config.
---@param opts? plasmon.Options
function M.setup(opts)
  if vim.fn.has("nvim-0.11") == 0 then
    vim.notify("plasmon: Neovim >= 0.11 is required", vim.log.levels.ERROR)
    return
  end
  M.init()
  config.set(opts)
  local lsp = vim.tbl_deep_extend("force", config.default(), config.options.lsp or {})
  vim.lsp.config("plasmon", lsp)
  vim.lsp.enable("plasmon")
end

--- Plasmon client for a buffer: the one attached to it, else the one of the last focused
--- Plasmon buffer, else any.
---@param opts? { bufnr?: integer, client_id?: integer, silent?: boolean }
---@return vim.lsp.Client?
function M.get_client(opts)
  opts = opts or {}
  if opts.client_id then
    return vim.lsp.get_client_by_id(opts.client_id)
  end
  local bufnr = opts.bufnr or vim.api.nvim_get_current_buf()
  local client = vim.lsp.get_clients({ name = "plasmon", bufnr = bufnr })[1]
  if client then
    return client
  end
  local clients = vim.lsp.get_clients({ name = "plasmon" })
  for _, c in ipairs(clients) do
    if handlers.last_focused[c.id] then
      return c
    end
  end
  if clients[1] then
    return clients[1]
  end
  if not opts.silent then
    vim.notify("Plasmon: no Plasmon server running", vim.log.levels.WARN)
  end
  return nil
end

--- URI of the file commands apply to: the current buffer if Plasmon handles it, else the last
--- file focused for that client.
---@param client vim.lsp.Client
---@param bufnr? integer
---@return string?
function M.current_uri(client, bufnr)
  bufnr = bufnr or vim.api.nvim_get_current_buf()
  if vim.lsp.buf_is_attached(bufnr, client.id) then
    return vim.uri_from_bufnr(bufnr)
  end
  return handlers.last_focused[client.id]
end

--- Sends `workspace/executeCommand` to the Plasmon server.
---@param command string
---@param arguments? any[]
---@param opts? { bufnr?: integer, client_id?: integer, on_result?: fun(result: any, client: vim.lsp.Client), on_error?: fun(err: lsp.ResponseError) }
function M.execute(command, arguments, opts)
  opts = opts or {}
  local client = M.get_client(opts)
  if not client then
    return
  end
  client:request(
    "workspace/executeCommand",
    { command = command, arguments = arguments or {} },
    function(err, result)
      if err then
        if opts.on_error then
          opts.on_error(err)
        else
          vim.notify(("Plasmon: %s failed: %s"):format(command, err.message or vim.inspect(err)), vim.log.levels.ERROR)
        end
        return
      end
      if opts.on_result then
        opts.on_result(result, client)
      end
    end,
    opts.bufnr
  )
end

--- Status items last sent by the server for a client.
---@param opts? { bufnr?: integer, client_id?: integer }
---@return plasmon.StatusItem[]
function M.status(opts)
  local client = M.get_client(vim.tbl_extend("force", opts or {}, { silent = true }))
  local st = client and handlers.status[client.id]
  if not st then
    return {}
  end
  local items = {}
  for _, id in ipairs(st.order) do
    table.insert(items, st.items[id])
  end
  return items
end

--- Short status for statuslines, e.g. `require("plasmon").statusline()`.
---
--- Empty when no Plasmon server handles the current buffer.
---@param bufnr? integer
---@return string
function M.statusline(bufnr)
  bufnr = bufnr or vim.api.nvim_get_current_buf()
  local client = vim.lsp.get_clients({ name = "plasmon", bufnr = bufnr })[1]
  if not client then
    return ""
  end
  local st = handlers.status[client.id]
  local summary = st and st.items["plasmon.summary"]
  if not summary then
    return "Plasmon"
  end
  local text = handlers.strip_icons(summary.text) or ""
  if summary.busy then
    text = text .. " …"
  end
  return text
end

return M
