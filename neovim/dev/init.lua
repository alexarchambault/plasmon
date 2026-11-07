-- Minimal Neovim config to try the plugin, used by `./mill -i neovim.run <path> --config minimal`.
--
--   nvim --clean -u neovim/dev/init.lua [file...]
--
-- Loads the plugin from this checkout, rather than any installed copy. The server command comes
-- from PLASMON_NVIM_SERVER_CMD (JSON array, set by `neovim.run` to the server built from
-- sources), else `plasmon server` is used.

local dev_dir = vim.fs.dirname(vim.fs.normalize(vim.fn.fnamemodify(debug.getinfo(1, "S").source:sub(2), ":p")))
local plugin_dir = vim.fs.dirname(dev_dir)

vim.opt.runtimepath:prepend(plugin_dir)

vim.o.number = true
vim.o.signcolumn = "yes"
vim.o.updatetime = 300
vim.o.completeopt = "menuone,noselect,popup"
vim.o.laststatus = 2
vim.o.statusline = "%f %h%m%r%=%{v:lua.require'plasmon'.statusline()}  %l:%c "
if vim.fn.exists("+winborder") == 1 then
  vim.o.winborder = "rounded" -- borders around hovers, signature help, …
end

vim.diagnostic.config({ virtual_text = true, severity_sort = true })

local server_cmd = vim.env.PLASMON_NVIM_SERVER_CMD
local opts = {}
if server_cmd and server_cmd ~= "" then
  opts.cmd = vim.json.decode(server_cmd)
end
require("plasmon").setup(opts)

-- Neovim maps K, grn, gra, grr, gri, gO, … by default (:help lsp-defaults); add a few more
vim.api.nvim_create_autocmd("LspAttach", {
  group = vim.api.nvim_create_augroup("plasmon.dev", { clear = true }),
  callback = function(ev)
    local client = vim.lsp.get_client_by_id(ev.data.client_id)
    if not client or client.name ~= "plasmon" then
      return
    end
    local map = function(lhs, rhs, desc)
      vim.keymap.set("n", lhs, rhs, { buffer = ev.buf, desc = desc })
    end
    map("gd", vim.lsp.buf.definition, "Go to definition")
    map("<leader>cl", vim.lsp.codelens.run, "Run code lens")
    map("<leader>f", function()
      vim.lsp.buf.format({ async = true })
    end, "Format")
    if client:supports_method("textDocument/completion") then
      vim.lsp.completion.enable(true, client.id, ev.buf, { autotrigger = true })
    end
  end,
})
