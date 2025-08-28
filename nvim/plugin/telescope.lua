vim.pack.add({
  { src = "https://github.com/nvim-telescope/telescope.nvim" },
  { src = "https://github.com/nvim-lua/plenary.nvim" },  -- dependency
})

local telescope = require("telescope")
local builtin = require("telescope.builtin")

telescope.setup({
  defaults = {
    layout_strategy = "horizontal",
    layout_config = { prompt_position = "top" },
    sorting_strategy = "ascending",
    winblend = 0,
  }
})

vim.keymap.set("n", "<leader>f", builtin.find_files, { noremap = true, silent = true })
vim.keymap.set("n", "<leader>b", builtin.buffers, { noremap = true, silent = true })
vim.keymap.set("n", "<leader>s", builtin.live_grep, { noremap = true, silent = true })
vim.keymap.set("n", "<leader>S", builtin.lsp_document_symbols, { noremap = true, silent = true })
