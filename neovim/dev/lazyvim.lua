-- LazyVim config to try the plugin, used by `./mill -i neovim.run <path>` (the default
-- `--config lazyvim`; `--config minimal` uses init.lua, next to this file).
--
--   NVIM_APPNAME=plasmon-dev-lazyvim nvim -u neovim/dev/lazyvim.lua [file...]
--
-- NVIM_APPNAME keeps LazyVim, its plugins and their state apart from the user's own Neovim set-up
-- (under ~/.local/share/plasmon-dev-lazyvim, ~/.local/state/plasmon-dev-lazyvim, …). Plugin
-- versions are pinned by lazy-lock.json, next to this file (`:Lazy update` updates it).
--
-- Loads the Plasmon plugin from this checkout. The server command comes from
-- PLASMON_NVIM_SERVER_CMD (JSON array, set by `neovim.run` to the server built from sources), else
-- `plasmon server` is used.

local dev_dir = vim.fs.dirname(vim.fs.normalize(vim.fn.fnamemodify(debug.getinfo(1, "S").source:sub(2), ":p")))
local plugin_dir = vim.fs.dirname(dev_dir)

-- LazyVim writes this to the config dir by default, which doesn't exist here
vim.g.lazyvim_json = vim.fn.stdpath("data") .. "/lazyvim.json"

local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.uv.fs_stat(lazypath) then
  local out = vim.fn.system({
    "git",
    "clone",
    "--filter=blob:none",
    "--branch=stable",
    "https://github.com/folke/lazy.nvim.git",
    lazypath,
  })
  if vim.v.shell_error ~= 0 then
    vim.api.nvim_echo({
      { "Failed to clone lazy.nvim:\n", "ErrorMsg" },
      { out, "WarningMsg" },
      { "\nPress any key to exit..." },
    }, true, {})
    vim.fn.getchar()
    os.exit(1)
  end
end
vim.opt.rtp:prepend(lazypath)

require("lazy").setup({
  spec = {
    {
      "LazyVim/LazyVim",
      import = "lazyvim.plugins",
      opts = { news = { lazyvim = false, neovim = false } },
    },

    -- Scala and Java highlighting
    { "nvim-treesitter/nvim-treesitter", opts = { ensure_installed = { "scala", "java" } } },

    -- The plugin from this checkout. LazyVim's LSP keymaps (gd, gr, K, <leader>ca, <leader>cr,
    -- <leader>cc for code lenses, …) apply to any server, Plasmon included.
    {
      dir = plugin_dir,
      name = "plasmon",
      lazy = false,
      opts = function()
        local opts = {}
        local server_cmd = vim.env.PLASMON_NVIM_SERVER_CMD
        if server_cmd and server_cmd ~= "" then
          opts.cmd = vim.json.decode(server_cmd)
        end
        return opts
      end,
      config = function(_, opts)
        require("plasmon").setup(opts)
        vim.api.nvim_create_autocmd("User", {
          pattern = "PlasmonStatus",
          group = vim.api.nvim_create_augroup("plasmon.dev.lualine", { clear = true }),
          callback = function()
            if package.loaded["lualine"] then
              require("lualine").refresh({ place = { "statusline" } })
            end
          end,
        })
      end,
    },

    -- Plasmon status in the statusline
    {
      "nvim-lualine/lualine.nvim",
      opts = function(_, opts)
        table.insert(opts.sections.lualine_x, 1, {
          function()
            return require("plasmon").statusline()
          end,
          cond = function()
            return package.loaded["plasmon"] ~= nil and require("plasmon").statusline() ~= ""
          end,
        })
      end,
    },
  },
  defaults = { lazy = false, version = false },
  lockfile = dev_dir .. "/lazy-lock.json",
  install = { colorscheme = { "tokyonight", "habamax" } },
  checker = { enabled = false },
  change_detection = { enabled = false },
  performance = {
    rtp = {
      disabled_plugins = { "gzip", "tarPlugin", "tohtml", "tutor", "zipPlugin" },
    },
  },
})
