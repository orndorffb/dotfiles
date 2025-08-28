-- Plugins
vim.pack.add({
  { src = "https://github.com/ibhagwan/fzf-lua" },
  { src = "https://github.com/nvim-tree/nvim-web-devicons" }, -- optional icons
})

-- Setup
local fzf = require("fzf-lua")

fzf.setup({
  -- prompt at top, no transparency, horizontal preview like your telescope layout
  winopts = {
    winblend = 0,
    preview = {
      layout = "horizontal",        -- split preview horizontally
      horizontal = "right:50%",     -- preview on the right, 50%
    },
  },
  fzf_opts = {
    ["--layout"] = "reverse",       -- prompt at top (ascending-like)
    ["--info"]   = "inline",
  },
})

-- Keymaps (mirroring your Telescope bindings)
vim.keymap.set("n", "<leader>f", function() fzf.files() end,                 { noremap = true, silent = true })
vim.keymap.set("n", "<leader>b", function() fzf.buffers() end,               { noremap = true, silent = true })
vim.keymap.set("n", "<leader>s", function() fzf.live_grep() end,             { noremap = true, silent = true })
vim.keymap.set("n", "<leader>S", function() fzf.lsp_document_symbols() end,  { noremap = true, silent = true })
