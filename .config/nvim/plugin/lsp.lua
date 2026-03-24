vim.pack.add({
  { src = "https://github.com/neovim/nvim-lspconfig" },
})

-- Global diagnostic settings
vim.diagnostic.config({
  virtual_text = true,
  signs = true,
  underline = true,
  update_in_insert = false,
  severity_sort = true,
})

-- Custom signs for diagnostics
local signs = { Error = " ", Warn = " ", Hint = " ", Info = " " }
for type, icon in pairs(signs) do
  local hl = "DiagnosticSign" .. type
  vim.fn.sign_define(hl, { text = icon, texthl = hl, numhl = "" })
end

-- LSP keymaps set on attach
vim.api.nvim_create_autocmd("LspAttach", {
  callback = function(args)
    local opts = { noremap = true, silent = true, buffer = args.buf }
    local map = vim.keymap.set
    map("n", "gd", vim.lsp.buf.definition, opts)
    map("n", "gD", vim.lsp.buf.declaration, opts)
    map("n", "gr", vim.lsp.buf.references, opts)
    map("n", "gi", vim.lsp.buf.implementation, opts)
    map("n", "<leader>rn", vim.lsp.buf.rename, opts)
    map("n", "<leader>ca", vim.lsp.buf.code_action, opts)
    map("n", "K", vim.lsp.buf.hover, opts)
  end,
})

-- Server-specific config overrides
vim.lsp.config("elixirls", {
  cmd = { "elixir-ls" },
})

vim.lsp.config("rust_analyzer", {
  settings = {
    ["rust-analyzer"] = {
      cargo = { allFeatures = true },
      procMacro = { enable = true },
    },
  },
})

vim.lsp.enable({ "elixirls", "ts_ls", "pyright", "ruby_lsp", "rust_analyzer" })
