vim.pack.add({
  { src = "https://github.com/neovim/nvim-lspconfig" },
})

local lspconfig = require("lspconfig")

-- Function to set buffer-local LSP keymaps
local function lsp_keymaps(bufnr)
  local opts = { noremap = true, silent = true, buffer = bufnr }
  local map = vim.keymap.set

  map("n", "gd", vim.lsp.buf.definition, opts)
  map("n", "gD", vim.lsp.buf.declaration, opts)
  map("n", "gr", vim.lsp.buf.references, opts)
  map("n", "gi", vim.lsp.buf.implementation, opts)
  map("n", "<leader>rn", vim.lsp.buf.rename, opts)
  map("n", "<leader>ca", vim.lsp.buf.code_action, opts)
  map("n", "K", vim.lsp.buf.hover, opts)
  map("n", "<C-k>", vim.lsp.buf.signature_help, opts)
end

-- Global diagnostic settings
vim.diagnostic.config({
  virtual_text = true,
  signs = true,
  underline = true,
  update_in_insert = false,
  severity_sort = true,
})

-- Optional: custom signs for diagnostics
local signs = { Error = " ", Warn = " ", Hint = " ", Info = " " }
for type, icon in pairs(signs) do
  local hl = "DiagnosticSign" .. type
  vim.fn.sign_define(hl, { text = icon, texthl = hl, numhl = "" })
end

-- Common on_attach function for all servers
local on_attach = function(client, bufnr)
  lsp_keymaps(bufnr)
end

-- LSP server setups
lspconfig.elixirls.setup({
  cmd = { "elixir-ls" },
  on_attach = on_attach,
})

lspconfig.ts_ls.setup({
  on_attach = on_attach,
})

lspconfig.pyright.setup({
  on_attach = on_attach,
})

lspconfig.ruby_lsp.setup({
  on_attach = on_attach,
})

lspconfig.rust_analyzer.setup({
  on_attach = on_attach,
  settings = {
    ["rust-analyzer"] = {
      cargo = { allFeatures = true },
      procMacro = { enable = true },
    },
  },
})

