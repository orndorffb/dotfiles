-- plugin/neogit.lua
-- Install and configure Neogit

-- Install Neogit plugin via native vim.pack
vim.pack.add({
    { src = "https://github.com/TimUntersberger/neogit" },
    { src = "https://github.com/nvim-lua/plenary.nvim" }, -- dependency
}, { load = true })

-- Require neogit
local neogit = require("neogit")

-- Setup Neogit
neogit.setup({
    integrations = {
        diffview = true, -- if you also use diffview.nvim
    },
    signs = {
        section = { ">", "v" },
        item = { ">", "v" },
        hunk = { "", "" },
    },
    kind = "tab",  -- open Neogit in a new tab
})

-- Optional: keymap to toggle Neogit
vim.keymap.set("n", "<leader>gg", function()
    neogit.open({ kind = "tab" })
end, { noremap = true, silent = true })
