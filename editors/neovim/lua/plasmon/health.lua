-- :checkhealth plasmon

local M = {}

function M.check()
  local health = vim.health
  local config = require("plasmon.config")

  health.start("plasmon")

  if vim.fn.has("nvim-0.11") == 1 then
    health.ok("Neovim " .. tostring(vim.version()))
  else
    health.error("Neovim >= 0.11 is required", { "Upgrade Neovim" })
  end

  local cmd = config.server_cmd()
  local exe = cmd[1]
  if vim.fn.executable(exe) == 1 then
    health.ok(("Server command: %s (%s)"):format(table.concat(cmd, " "), vim.fn.exepath(exe)))
  else
    health.error(("%s not found"):format(exe), {
      "Install the plasmon launcher and put it on PATH",
      'Or pass its path: require("plasmon").setup({ binary = "/path/to/plasmon" })',
    })
  end

  local enabled = vim.lsp.is_enabled and vim.lsp.is_enabled("plasmon")
  if enabled then
    health.ok("plasmon LSP config enabled")
  elseif enabled == false then
    health.warn("plasmon LSP config not enabled", {
      'Call require("plasmon").setup() or vim.lsp.enable("plasmon")',
    })
  end

  local clients = vim.lsp.get_clients({ name = "plasmon" })
  if #clients == 0 then
    health.info("No Plasmon server running")
  end
  for _, client in ipairs(clients) do
    local root = client.config.cmd_cwd or client.config.root_dir or "?"
    health.info(("Server %d in %s, attached to %d buffer(s)"):format(
      client.id,
      root,
      vim.tbl_count(client.attached_buffers)
    ))
    local pid = config.lock_holder(root)
    if pid then
      health.warn(("Another Plasmon server (PID %d) holds %s/.plasmon/lock"):format(pid, root), {
        ":Plasmon check-concurrent-server",
      })
    end
  end
end

return M
