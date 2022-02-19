local use = require('packer').use
require('packer').startup(function()
  use 'wbthomason/packer.nvim'

  use 'tpope/vim-commentary'         -- "gc" to comment visual regions/lines
  use 'tpope/vim-surround'

  use 'joshdick/onedark.vim'         -- Theme inspired by Atom

  use {'nvim-telescope/telescope.nvim', requires = {{'nvim-lua/popup.nvim'}, {'nvim-lua/plenary.nvim'}} }

  use "folke/which-key.nvim" -- which key setup

  use 'neovim/nvim-lspconfig'
  use 'dkarter/bullets.vim'
  use { 'TimUntersberger/neogit', requires = 'nvim-lua/plenary.nvim' }

  use 'ms-jpq/coq_nvim'
  use 'ms-jpq/coq.artifacts'
  use 'psf/black'
end)

-- markdown folding
vim.g.markdown_folding = 1


local wk = require("which-key")
local actions = require('telescope.actions')
require('telescope').setup{
  defaults = {
    mappings = {
      n = {
        ["q"] = actions.close
      },
    },
  },
}
local tele_builtins = require('telescope.builtin')
wk.register({
  f = {
    name = "file",
    f = { "<cmd>Telescope find_files<cr>", "Find File" },
  },
  b = { function() tele_builtins.buffers{ ignore_current_buffer = true } end, "Find Buffer" },
  l = {
    name = "lsp",
    r = { "<cmd>Telescope lsp_references<cr>", "Document References" },
    s = { "<cmd>Telescope lsp_document_symbols<cr>", "Document Symbols" },
    w = { "<cmd>Telescope lsp_dynamic_workspace_symbols<cr>", "Workspace Symbols" },
    a = { "<cmd>Telescope lsp_code_actions<cr>", "Code actions" },
    n = { "<cmd>lua vim.lsp.buf.rename()<cr>", "Rename symbol" },
  },
  p = {
    name = "formatting",
    b = {"<cmd>Black<cr>", "Run black"},
  },
  g = { "<cmd>Neogit<cr>", "Neogit" }
}, { prefix = "<leader>" })


--Add leader shortcuts
-- vim.api.nvim_set_keymap('n', '<leader>f', [[<cmd>lua require('telescope.builtin').find_files()<cr>]], { noremap = true, silent = true})
-- vim.api.nvim_set_keymap('n', '<leader><space>', [[<cmd>lua require('telescope.builtin').buffers()<cr>]], { noremap = true, silent = true})
-- vim.api.nvim_set_keymap('n', '<leader>l', [[<cmd>lua require('telescope.builtin').current_buffer_fuzzy_find()<cr>]], { noremap = true, silent = true})
-- vim.api.nvim_set_keymap('n', '<leader>t', [[<cmd>lua require('telescope.builtin').tags()<cr>]], { noremap = true, silent = true})
-- vim.api.nvim_set_keymap('n', '<leader>?', [[<cmd>lua require('telescope.builtin').oldfiles()<cr>]], { noremap = true, silent = true})
-- vim.api.nvim_set_keymap('n', '<leader>s', [[<cmd>lua require('telescope.builtin').grep_string()<cr>]], { noremap = true, silent = true})
-- vim.api.nvim_set_keymap('n', '<leader>s', [[<cmd>lua require('telescope.builtin').live_grep()<cr>]], { noremap = true, silent = true})
-- vim.api.nvim_set_keymap('n', '<leader>ls', [[<cmd>lua require('telescope.builtin').lsp_workspace_symbols()<cr>]], { noremap = true, silent = true})
-- vim.api.nvim_set_keymap('n', '<leader>la', [[<cmd>lua require('telescope.builtin').lsp_code_actions()<cr>]], { noremap = true, silent = true})
-- -- vim.api.nvim_set_keymap('n', '<leader>o', [[<cmd>lua require('telescope.builtin').tags{ only_current_buffer = true }<cr>]], { noremap = true, silent = true})
-- vim.api.nvim_set_keymap('n', '<leader>gc', [[<cmd>lua require('telescope.builtin').git_commits()<cr>]], { noremap = true, silent = true})
-- vim.api.nvim_set_keymap('n', '<leader>gb', [[<cmd>lua require('telescope.builtin').git_branches()<cr>]], { noremap = true, silent = true})
-- vim.api.nvim_set_keymap('n', '<leader>gs', [[<cmd>lua require('telescope.builtin').git_status()<cr>]], { noremap = true, silent = true})
-- vim.api.nvim_set_keymap('n', '<leader>gp', [[<cmd>lua require('telescope.builtin').git_bcommits()<cr>]], { noremap = true, silent = true})


local nvim_lsp = require('lspconfig')
local on_attach = function(_client, bufnr)
  vim.api.nvim_buf_set_option(bufnr, 'omnifunc', 'v:lua.vim.lsp.omnifunc')

  local opts = { noremap=true, silent=true }
  vim.api.nvim_buf_set_keymap(bufnr, 'n', 'gD', '<Cmd>lua vim.lsp.buf.declaration()<CR>', opts)
  vim.api.nvim_buf_set_keymap(bufnr, 'n', 'gd', '<Cmd>lua vim.lsp.buf.definition()<CR>', opts)
  vim.api.nvim_buf_set_keymap(bufnr, 'n', 'K', '<Cmd>lua vim.lsp.buf.hover()<CR>', opts)
  vim.api.nvim_buf_set_keymap(bufnr, 'n', 'gi', '<cmd>lua vim.lsp.buf.implementation()<CR>', opts)
  vim.api.nvim_buf_set_keymap(bufnr, 'i', '<C-k>', '<cmd>lua vim.lsp.buf.signature_help()<CR>', opts)
  -- vim.api.nvim_buf_set_keymap(bufnr, 'n', '<leader>wa', '<cmd>lua vim.lsp.buf.add_workspace_folder()<CR>', opts)
  -- vim.api.nvim_buf_set_keymap(bufnr, 'n', '<leader>wr', '<cmd>lua vim.lsp.buf.remove_workspace_folder()<CR>', opts)
  -- vim.api.nvim_buf_set_keymap(bufnr, 'n', '<leader>wl', '<cmd>lua print(vim.inspect(vim.lsp.buf.list_workspace_folders()))<CR>', opts)
  -- vim.api.nvim_buf_set_keymap(bufnr, 'n', '<leader>D', '<cmd>lua vim.lsp.buf.type_definition()<CR>', opts)
  vim.api.nvim_buf_set_keymap(bufnr, 'n', '<leader>rn', '<cmd>lua vim.lsp.buf.rename()<CR>', opts)
  vim.api.nvim_buf_set_keymap(bufnr, 'n', 'gr', '<cmd>lua vim.lsp.buf.references()<CR>', opts)
  vim.api.nvim_buf_set_keymap(bufnr, 'n', '<leader>ca', '<cmd>lua vim.lsp.buf.code_action()<CR>', opts)
  vim.api.nvim_buf_set_keymap(bufnr, 'n', '<leader>e', '<cmd>lua vim.lsp.diagnostic.show_line_diagnostics()<CR>', opts)
  vim.api.nvim_buf_set_keymap(bufnr, 'n', '[d', '<cmd>lua vim.lsp.diagnostic.goto_prev()<CR>', opts)
  vim.api.nvim_buf_set_keymap(bufnr, 'n', ']d', '<cmd>lua vim.lsp.diagnostic.goto_next()<CR>', opts)
  vim.api.nvim_buf_set_keymap(bufnr, 'n', '<leader>q', '<cmd>lua vim.lsp.diagnostic.set_loclist()<CR>', opts)
end


-- clangd switch header
vim.api.nvim_set_keymap('n', '<leader>h', [[<cmd>:ClangdSwitchSourceHeader<cr>]], { noremap = true, silent = true})

local lspconfig = require('lspconfig')

lspconfig.pyright.setup{ on_attach=on_attach }
lspconfig.clangd.setup{ on_attach=on_attach }
lspconfig.tsserver.setup{ on_attach=on_attach }
lspconfig.tailwindcss.setup{ on_attach=on_attach }
lspconfig.hls.setup{ on_attach=on_attach }
