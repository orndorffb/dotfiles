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


require("mini.pick").setup({
  -- Optional: centered, rounded window (tweak to taste)
  window = {
    config = function()
      local h = math.floor(vim.o.lines * 0.8)
      local w = math.floor(vim.o.columns * 0.8)
      return {
        anchor = 'NW',
        row = math.floor(0.5 * (vim.o.lines - h)),
        col = math.floor(0.5 * (vim.o.columns - w)),
        height = h,
        width  = w,
        border = 'rounded',
      }
    end,
  },
})

-- Keymaps to mirror your fzf-lua ones
local pick  = require("mini.pick")
local extra = require("mini.extra")

vim.keymap.set("n", "<leader>f", function() pick.builtin.files() end, { desc = "Pick files" })
vim.keymap.set("n", "<leader>b", function() pick.builtin.buffers() end, { desc = "Pick buffers" })
vim.keymap.set("n", "<leader>s", function() pick.builtin.grep_live() end, { desc = "Live grep" })

-- Document symbols (current file)
vim.keymap.set("n", "<leader>S", function()
  extra.pickers.lsp({ scope = "document_symbol" })
end, { desc = "LSP document symbols" })

-- Optional: surrounding plugin (uncomment to enable)
-- require("mini.surround").setup({
--   mappings = {
--     add = "sa",     -- Add surrounding with 'sa'
--     delete = "sd",  -- Delete surrounding with 'sd'
--     replace = "sr", -- Replace surrounding with 'sr'
--   },
-- })
