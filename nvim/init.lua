-- Remaps
vim.g.mapleader = " "
vim.keymap.set("n", "<leader>fe", vim.cmd.Ex)
vim.keymap.set("n", "<leader>p", "\"+p")
vim.keymap.set("n", "<leader>P", "\"+P")

-- Sets
vim.opt.nu = true

vim.opt.tabstop = 2
vim.opt.softtabstop = 2
vim.opt.shiftwidth = 2
vim.opt.expandtab = true

vim.opt.smartindent = true

vim.opt.hidden = true

vim.opt.swapfile = false
vim.opt.backup = false
vim.opt.undofile = true

vim.opt.hlsearch = false
vim.opt.incsearch = true

vim.opt.termguicolors = true

vim.opt.scrolloff = 8

vim.opt.updatetime = 50

vim.keymap.set("n", "<C-d>", "<C-d>zz")
vim.keymap.set("n", "<C-u>", "<C-u>zz")
vim.keymap.set("n", "n", "nzzzv")
vim.keymap.set("n", "N", "Nzzzv")

vim.o.background = "dark"

vim.opt.list = true

-- Plugins
local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
  vim.fn.system({
    "git",
    "clone",
    "--filter=blob:none",
    "https://github.com/folke/lazy.nvim.git",
    "--branch=stable", -- latest stable release
    lazypath,
  })
end
vim.opt.rtp:prepend(lazypath)

require("lazy").setup({
  {
    "rose-pine/neovim",
    lazy = false, -- make sure we load this during startup if it is your main colorscheme
    priority = 1000, -- make sure to load this before all the other start plugins
    config = function()
      -- load the colorscheme here
      vim.cmd([[colorscheme rose-pine]])
    end,
  },
  {
    'nvim-telescope/telescope.nvim',
    tag = '0.1.6',
    dependencies = { 'nvim-lua/plenary.nvim' },
    lazy = false,
    keys = {
      {'<leader>fs', "<cmd>Telescope live_grep<cr>", desc = "Live grep"},
      {'<leader>ff', "<cmd>Telescope find_files<cr>", desc = "Find file"},
      {'<leader>fb', "<cmd>Telescope buffers<cr>", desc = "Find in buffers"},
      {'<leader>ds', "<cmd>Telescope lsp_document_symbols<cr>", desc = "lsp document symbols"},
      {'gd', "<cmd>Telescope lsp_definitions<cr>", desc = "lsp defs"},
      {'gr', "<cmd>Telescope lsp_references<cr>", desc = "lsp refs"},
    },
    config = function()
      require('telescope').setup()
    end,
  },
  {
    "nvim-treesitter/nvim-treesitter",
    build = ":TSUpdate",
    lazy = false,
    config = function ()
      local configs = require("nvim-treesitter.configs")
      ensure_installed = { "c", "lua", "vim", "vimdoc", "query", "ruby", "javascript", "html" },
      configs.setup({
          sync_install = false,
          highlight = { enable = true },
          indent = { enable = true },
        })
    end
  }
})


