-- plugin/01-keymaps.lua
local map = vim.keymap.set

map("n", "<leader>e", vim.cmd.Ex)
map("n", "<leader>p", "\"+p")
map("n", "<leader>P", "\"+P")
map("i", "jk", "<Esc>")

map("n", "<C-d>", "<C-d>zz")
map("n", "<C-u>", "<C-u>zz")
map("n", "n", "nzzzv")
map("n", "N", "Nzzzv")

-- Toggle background function
function ToggleBackground()
  if vim.o.background == "dark" then
    vim.o.background = "light"
    print("Switched to light background")
  else
    vim.o.background = "dark"
    print("Switched to dark background")
  end
end
map("n", "<leader>tb", ToggleBackground, { noremap = true, silent = true })
