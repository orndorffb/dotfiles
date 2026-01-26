vim.pack.add({
    { src = "https://github.com/mcauley-penney/techbase.nvim" },
    { src = "https://github.com/zenbones-theme/zenbones.nvim"},
    { src = "https://github.com/rktjmp/lush.nvim"},
    { src = "https://github.com/rose-pine/neovim"},
    { src = "https://github.com/catppuccin/nvim"},
    { src = "https://github.com/kepano/flexoki-neovim"},
    { src = "https://github.com/f-person/auto-dark-mode.nvim"},
    { src = "https://github.com/sainnhe/gruvbox-material"},
    { src = "https://github.com/serhez/teide.nvim"}
})

require('techbase').setup({})

require('auto-dark-mode').setup({
    update_interval = 1000,
    set_dark_mode = function()
        vim.api.nvim_set_option_value("background", "dark", {})
        vim.cmd("colorscheme zenbones")
    end,
    set_light_mode = function()
        vim.api.nvim_set_option_value("background", "light", {})
        vim.cmd("colorscheme zenbones")
    end,
})
