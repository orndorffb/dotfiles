-- Telescope fuzzy finder
vim.pack.add({
  { src = "https://github.com/nvim-telescope/telescope.nvim" },
  { src = "https://github.com/nvim-lua/plenary.nvim" }, -- required dependency
  { src = "https://github.com/nvim-tree/nvim-web-devicons" }, -- optional icons
})

local telescope = require("telescope")
local actions = require("telescope.actions")

telescope.setup({
  defaults = {
    mappings = {
      i = {
        ["<C-j>"] = actions.move_selection_next,
        ["<C-k>"] = actions.move_selection_previous,
        ["<C-q>"] = actions.send_selected_to_qflist + actions.open_qflist,
      },
    },
  },
  pickers = {
    buffers = {
      mappings = {
        i = {
          ["<C-d>"] = actions.delete_buffer,
        },
      },
    },
  },
})

-- Keymaps
local builtin = require("telescope.builtin")

vim.keymap.set("n", "<leader>f", builtin.find_files, { desc = "Find files" })
vim.keymap.set("n", "<leader>b", builtin.buffers, { desc = "Find buffers" })
vim.keymap.set("n", "<leader>s", builtin.live_grep, { desc = "Live grep" })
vim.keymap.set("n", "<leader>S", builtin.lsp_document_symbols, { desc = "LSP document symbols" })
