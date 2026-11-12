-- Plasmon: :Plasmon command and buffer set-up. Starting the server is opt-in, with
-- require("plasmon").setup() or vim.lsp.enable("plasmon").

if vim.fn.has("nvim-0.11") == 0 then
  return
end

require("plasmon").init()
