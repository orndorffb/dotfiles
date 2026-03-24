vim.pack.add({
  { src = "https://github.com/nvim-treesitter/nvim-treesitter", build = ":TSUpdate" },
}, { load = true })

require("nvim-treesitter.configs").setup({
  ensure_installed = {
    "c", "lua", "vim", "vimdoc", "query",
    "elixir", "heex", "javascript", "html", "ruby", "python"
  },
  sync_install = false,
  highlight = { enable = true },
  indent = { enable = true },
})
