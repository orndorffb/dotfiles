vim.pack.add({
    { src = "https://github.com/echasnovski/mini.nvim" },
}, { load = true })

-- Comment plugin
require("mini.comment").setup()

-- Statusline
-- require("mini.statusline").setup()

-- Pairs (auto-close brackets, quotes, etc.)
require("mini.pairs").setup()

-- Extra features (like auto-trim whitespace)
require("mini.extra").setup()

-- Jump2D for 2D motion
require("mini.jump2d").setup({
  view = { dim = true },
  mappings = { start_jumping = "<leader>gw" },
})

-- Indent scope
require("mini.indentscope").setup({
  draw = { delay = 50 },
})

-- Optional: surrounding plugin (uncomment to enable)
-- require("mini.surround").setup({
--   mappings = {
--     add = "sa",     -- Add surrounding with 'sa'
--     delete = "sd",  -- Delete surrounding with 'sd'
--     replace = "sr", -- Replace surrounding with 'sr'
--   },
-- })

-- Picker (fuzzy finder)
require("mini.pick").setup({
  mappings = {
    move_down = "<C-j>",
    move_up = "<C-k>",
  },
  window = {
    config = function()
      local height = math.floor(0.618 * vim.o.lines)
      local width = math.floor(0.618 * vim.o.columns)
      return {
        anchor = "NW",
        height = height,
        width = width,
        row = math.floor(0.5 * (vim.o.lines - height)),
        col = math.floor(0.5 * (vim.o.columns - width)),
      }
    end,
  },
})

-- Keymaps for mini.pick
local pick = require("mini.pick")
vim.keymap.set("n", "<leader>f", pick.builtin.files, { desc = "Find files" })
vim.keymap.set("n", "<leader>b", pick.builtin.buffers, { desc = "Find buffers" })
vim.keymap.set("n", "<leader>s", pick.builtin.grep_live, { desc = "Live grep" })
vim.keymap.set("n", "<leader>S", function()
  pick.builtin.lsp({ scope = "document_symbol" })
end, { desc = "LSP document symbols" })
