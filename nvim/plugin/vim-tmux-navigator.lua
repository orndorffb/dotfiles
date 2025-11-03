-- plugin/vim-tmux-navigator.lua
-- Seamless navigation between tmux panes and vim splits

vim.pack.add({
    { src = "https://github.com/christoomey/vim-tmux-navigator" },
}, { load = true })

-- Keymaps for navigating between vim splits and tmux panes
local map = vim.keymap.set

map("n", "<C-h>", "<cmd>TmuxNavigateLeft<cr>", { silent = true, desc = "Navigate left" })
map("n", "<C-j>", "<cmd>TmuxNavigateDown<cr>", { silent = true, desc = "Navigate down" })
map("n", "<C-k>", "<cmd>TmuxNavigateUp<cr>", { silent = true, desc = "Navigate up" })
map("n", "<C-l>", "<cmd>TmuxNavigateRight<cr>", { silent = true, desc = "Navigate right" })
map("n", "<C-\\>", "<cmd>TmuxNavigatePrevious<cr>", { silent = true, desc = "Navigate to previous" })
