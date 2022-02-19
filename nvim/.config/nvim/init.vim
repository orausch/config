set colorcolumn=120
set nowrap
filetype plugin indent on

set expandtab tabstop=2 shiftwidth=2 softtabstop=2

autocmd Filetype cpp setlocal expandtab tabstop=2 shiftwidth=2 softtabstop=2
autocmd Filetype lua setlocal expandtab tabstop=2 shiftwidth=2 softtabstop=2
autocmd Filetype html setlocal expandtab tabstop=4 shiftwidth=4 softtabstop=4

let g:coq_settings = { 'auto_start': 'shut-up' }

lua require("packer_bootstrap")
lua require("plugin")
set number

set hidden
color onedark
