vim.pack.add({
    { src = "https://github.com/echasnovski/mini.nvim" },
}, { load = true })

-- Now we can safely require modules
local mini = require("mini")

-- Comment plugin
require("mini.comment").setup()

-- Statusline
require("mini.statusline").setup()

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
