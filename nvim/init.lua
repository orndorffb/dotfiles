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

-- Thanks for the func chatgpt
function ToggleBackground()
  local current_bg = vim.o.background
  if current_bg == "dark" then
    vim.o.background = "light"
    print("Switched to light background")
  else
    vim.o.background = "dark"
    print("Switched to dark background")
  end
end
vim.api.nvim_set_keymap('n', '<leader>tb', ':lua ToggleBackground()<CR>', { noremap = true, silent = true })


-- vim.o.background = "dark"
vim.o.background = "light"

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
    "zenbones-theme/zenbones.nvim",
    dependencies = "rktjmp/lush.nvim",
    lazy = false,
    priority = 1000,
    config = function()
        vim.g.zenbones_darken_comments = 45
        vim.cmd.colorscheme('zenbones')
    end
  },
  {
    "echasnovski/mini.nvim",
    lazy = false,
    priority = 1000,
    config = function()
      require("mini.surround").setup({
        mappings = {
          add = "sa", -- Add surrounding with 'sa'
          delete = "sd", -- Delete surrounding with 'sd'
          replace = "sr", -- Replace surrounding with 'sr'
        },
      })

      require("mini.comment").setup()
      require("mini.statusline").setup()
      require("mini.pairs").setup()
      require("mini.pick").setup({
        mappings = {
          vim.api.nvim_set_keymap('n', '<leader><leader>', ':Pick files<CR>', { noremap = true, silent = true }),
          vim.api.nvim_set_keymap('n', '<leader>b', ':Pick buffers<CR>', { noremap = true, silent = true }),
          vim.api.nvim_set_keymap('n', '<leader>s', ':Pick grep_live<CR>', { noremap = true, silent = true }),
        }
      })

      require("mini.jump2d").setup({
        view = {
          dim = true
        },
        mappings = {
          start_jumping = "<leader>gw"
        }
      })

      require("mini.indentscope").setup({
         draw = { delay = 50, },
      })
    end,
  },
  {
    -- LSP Config
    "neovim/nvim-lspconfig",
    lazy = false,  -- Ensure it's loaded at startup
    priority = 1000,  -- Load this first to set up the LSP
    config = function()
      require'lspconfig'.ruby_lsp.setup{}
      require'lspconfig'.rust_analyzer.setup({
        settings = {
          ["rust-analyzer"] = {
            cargo = { allFeatures = true },  -- Enable all Cargo features
            procMacro = { enable = true },  -- Enable procedural macros
          }
        }
      })
      local opts = { noremap = true, silent = true }  -- Default options for the mappings
      vim.api.nvim_set_keymap('n', '<leader>ca', '<Cmd>lua vim.lsp.buf.code_action()<CR>', opts)
      vim.api.nvim_set_keymap('n', 'gd', '<Cmd>lua vim.lsp.buf.definition()<CR>', opts)
      vim.api.nvim_set_keymap('n', 'gr', '<Cmd>lua vim.lsp.buf.references()<CR>', opts)
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
    "github/copilot.vim",
    event = "InsertEnter",
    config = function()
    end
  }
})
