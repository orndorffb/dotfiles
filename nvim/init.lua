-- Remaps
vim.g.mapleader = " "
vim.keymap.set("n", "<leader>fe", vim.cmd.Ex)
vim.keymap.set("n", "<leader>p", "\"+p")
vim.keymap.set("n", "<leader>P", "\"+P")
vim.keymap.set("i", "jk", "<Esc>")

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

-- plugin manager setup
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

-- Plugins
require("lazy").setup({
  {
    "rebelot/kanagawa.nvim",
    lazy = false, -- make sure we load this during startup if it is your main colorscheme
    priority = 1000, -- make sure to load this before all the other start plugins
    config = function()
      -- load the colorscheme here
      vim.cmd([[colorscheme kanagawa-dragon]])
    end,
  },
  {
    -- LSP Config
    "neovim/nvim-lspconfig",
    lazy = false,  -- Ensure it's loaded at startup
    priority = 1000,  -- Load this first to set up the LSP
    config = function()
      require'lspconfig'.rust_analyzer.setup({
        settings = {
          ["rust-analyzer"] = {
            cargo = { allFeatures = true },  -- Enable all Cargo features
            procMacro = { enable = true },  -- Enable procedural macros
          }
        }
      })
    end,
  },
  {
    -- nvim-cmp for autocompletion
    "hrsh7th/nvim-cmp",
    event = "InsertEnter",  -- Only load when you start typing in insert mode
    config = function()
      local cmp = require'cmp'

      -- Setup nvim-cmp.
      cmp.setup({
        -- Configure sources for completion
        sources = {
          { name = 'nvim_lsp' },  -- LSP-based completion (including rust-analyzer)
          { name = 'buffer' },    -- Autocomplete from current buffer
          { name = 'path' },      -- Autocomplete file paths
        },

        -- Mapping for completion
        mapping = {
          ['<C-n>'] = cmp.mapping.select_next_item(),
          ['<C-p>'] = cmp.mapping.select_prev_item(),
          ['<C-y>'] = cmp.mapping.confirm({ select = true }),  -- Confirm selection
          ['<C-Space>'] = cmp.mapping.complete(),  -- Trigger completion manually
        },

        -- Snippet configuration (optional but recommended)
        snippet = {
          expand = function(args)
            vim.fn["vsnip#anonymous"](args.body)  -- If using vsnip or any snippet manager
          end,
        },
      })
    end,
  },

  {
    -- Completion source for LSP
    "hrsh7th/cmp-nvim-lsp",
    event = "InsertEnter",
  },

  {
    -- Optional: Snippet engine (e.g., vsnip or luasnip)
    "hrsh7th/vim-vsnip",  -- Use vsnip for snippets (you can choose another snippet engine)
    event = "InsertEnter",
  },

  {
    'nvim-telescope/telescope.nvim',
    tag = '0.1.6',
    dependencies = { 'nvim-lua/plenary.nvim' },
    lazy = false,
    keys = {
      {'<leader>fs', "<cmd>Telescope live_grep<cr>", desc = "Live grep"},
      {'<leader>fg', "<cmd>Telescope git_files<cr>", desc = "Find files in git"},
      {'<leader>ff', "<cmd>Telescope find_files<cr>", desc = "Find file"},
      {'<leader>fb', "<cmd>Telescope buffers<cr>", desc = "Find in buffers"},
      {'<leader>ds', "<cmd>Telescope lsp_document_symbols<cr>", desc = "lsp document symbols"},
      {'<leader>ts', "<cmd>Telescope treesitter<cr>", desc = "treesitter symbols"},
      {'gd', "<cmd>Telescope lsp_definitions<cr>", desc = "lsp defs"},
      {'gr', "<cmd>Telescope lsp_references<cr>", desc = "lsp refs"},
    },
    config = function ()
      require('telescope').setup()
    end,
  },
  {
    "nvim-treesitter/nvim-treesitter",
    build = ":TSUpdate",
    config = function()
      local configs = require("nvim-treesitter.configs")

      configs.setup({
          ensure_installed = { "c", "lua", "vim", "vimdoc", "query", "elixir", "heex", "javascript", "html", "ruby" },
          sync_install = false,
          highlight = { enable = true },
          indent = { enable = true },
        })
    end
  },
  {
    'nvim-lualine/lualine.nvim',
    dependencies = { 'nvim-tree/nvim-web-devicons' },
  config = function()
      require('lualine').setup()
    end
  },
  {
    "github/copilot.vim",
    event = "InsertEnter",
    config = function()
    end
  }
})
