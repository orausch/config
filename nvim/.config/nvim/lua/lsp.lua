local lspconfig = require('lspconfig')

-- Mappings.
-- See `:help vim.diagnostic.*` for documentation on any of the below functions
local opts = { noremap=true, silent=true }
vim.keymap.set('n', '[d', vim.diagnostic.goto_prev, opts)
vim.keymap.set('n', ']d', vim.diagnostic.goto_next, opts)

-- Use an on_attach function to only map the following keys
-- after the language server attaches to the current buffer
local on_attach = function(client, bufnr)
  -- Enable completion triggered by <c-x><c-o>
  vim.api.nvim_buf_set_option(bufnr, 'omnifunc', 'v:lua.vim.lsp.omnifunc')

  -- Mappings.
  -- See `:help vim.lsp.*` for documentation on any of the below functions
  local bufopts = { noremap=true, silent=true, buffer=bufnr }
  vim.keymap.set('n', 'gD', vim.lsp.buf.declaration, bufopts)
  vim.keymap.set('n', 'gd', vim.lsp.buf.definition, bufopts)
  vim.keymap.set('n', 'K', vim.lsp.buf.hover, bufopts)
  vim.keymap.set('n', 'gi', vim.lsp.buf.implementation, bufopts)
  vim.keymap.set('i', '<C-k>', vim.lsp.buf.signature_help, bufopts)
  vim.keymap.set('n', '<C-k>', vim.lsp.buf.signature_help, bufopts)
end

-- Whichkey mappings
local tele_builtin = require('telescope.builtin')
require("which-key").register({
  l = {
    name = "lsp",
    d = { vim.diagnostic.open_float, "Show Diagnostic at point" },
    D = { vim.diagnostic.setloclist, "Put Diagnostics in Quickfix" },
    r = { tele_builtin.lsp_references, "Document References" },
    s = { tele_builtin.lsp_document_symbols, "Document Symbols" },
    w = { tele_builtin.lsp_dynamic_workspace_symbols, "Workspace Symbols" },
    a = { tele_builtin.lsp_code_actions, "Code actions" },
    n = { vim.lsp.buf.rename, "Rename symbol" },
  },
}, { prefix = "<leader>" })

-- clean up diagnostic noise
vim.lsp.handlers["textDocument/publishDiagnostics"] = vim.lsp.with(
  vim.lsp.diagnostic.on_publish_diagnostics, {
    -- Enable underline, use default values
    underline = true,
    -- Disable virtual text
    virtual_text = false,
    -- Use a function to dynamically turn signs off
    -- and on, using buffer local variables
    signs = true,
    -- Disable a feature
    update_in_insert = false,
  }
)


-- clangd switch header
vim.api.nvim_set_keymap('n', '<leader>h', [[<cmd>:ClangdSwitchSourceHeader<cr>]], { noremap = true, silent = true})


-- some default servers. More complicated servers are instantiated in their respective filetypes
lspconfig.clangd.setup{ on_attach=on_attach }
lspconfig.tsserver.setup{ on_attach=on_attach }
lspconfig.tailwindcss.setup{ on_attach=on_attach }
lspconfig.hls.setup{ on_attach=on_attach }
lspconfig.texlab.setup{ on_attach=on_attach }

return {lspconfig = lspconfig, on_attach = on_attach}
