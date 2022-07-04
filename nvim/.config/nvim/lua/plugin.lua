local use = require('packer').use
require('packer').startup(function()
  use 'wbthomason/packer.nvim'

  use 'tpope/vim-commentary'         -- "gc" to comment visual regions/lines
  use 'tpope/vim-surround'

  use 'joshdick/onedark.vim'         -- Theme inspired by Atom

  use {
    'nvim-telescope/telescope.nvim',
    requires = {'nvim-lua/plenary.nvim'},
  }

  use "folke/which-key.nvim" -- which key setup

  use 'neovim/nvim-lspconfig'
  use 'dkarter/bullets.vim'
  use { 'TimUntersberger/neogit', requires = 'nvim-lua/plenary.nvim' }

  use 'ms-jpq/coq_nvim'
  use 'psf/black'

  use 'editorconfig/editorconfig-vim'
  use {
    'nvim-lualine/lualine.nvim',
    requires = { 'kyazdani42/nvim-web-devicons', opt = true }
  }
  use 'vim-test/vim-test'
  use 'preservim/vimux'

  use 'github/copilot.vim'
end)

-- markdown folding
-- vim.g.markdown_folding = 1


-- Language specific things are deferred to a lang.lua file.
-- This main file is only used for listing plugins to install

-- todo what is cmp
-- https://old.reddit.com/r/neovim/comments/uk2uke/cool_cmp/


-- WHICHKEY
local wk = require("which-key")
wk.setup {
  ignore_missing = true, -- ignores all mappings for which we don't add a label
  presets = false -- disable mappings for default keybindings
}

-- Note: nice keybinding for telescope: C-q sends all results to the quickfix list
local tele_builtin = require('telescope.builtin')
-- default whichkey bindings
wk.register({
  f = {
    name = "find",
    f = { tele_builtin.find_files, "Find File" },
    g = { tele_builtin.live_grep, "Live Grep" },
  },
  b = { function() tele_builtin.buffers{ ignore_current_buffer = true, sort_mru = true } end, "Find Buffer" },
  p = {
    name = "formatting",
    b = {"<cmd>Black<cr>", "Run black"},
  },
  g = { "<cmd>Neogit<cr>", "Neogit" }
}, { prefix = "<leader>" })

-- LSP
-- returns an object with .lspconfig and .on_attach exposed
local lspconfig = require("lsp")

-- PYTHON
require("python")

-- STATUSLINE
require('lualine').setup {
  options = {
    icons_enabled = true,
    theme = 'auto',
    component_separators = { left = '', right = ''},
    section_separators = { left = '', right = ''},
    disabled_filetypes = {},
    always_divide_middle = true,
    globalstatus = false,
  },
  sections = {
    lualine_a = {'mode'},
    lualine_b = {'diff'},
    lualine_c = {},
    lualine_x = {'filename'},
    lualine_y = {},
    lualine_z = {'location'}
  },
  inactive_sections = {
    lualine_a = {},
    lualine_b = {},
    lualine_c = {},
    lualine_x = {},
    lualine_y = {},
    lualine_z = {}
  },
  tabline = {},
  extensions = {}
}



-- TESTING
-- vim.api.nvim_command("let test#strategy = dispatch")
--vim.api.nvim_set_var('test#strategy', 'vimux')

vim.api.nvim_set_var('test#python#pyunit#file_pattern', 'a^')



wk.register({
  t = {
    name = "test",
    n = { "<cmd>TestNearest<cr>", "Test Nearest" },
    l = { "<cmd>TestLast<cr>", "Test Last" },
    f = { "<cmd>TestFile<cr>", "Test File" },
  },
}, { prefix = "<leader>" })

