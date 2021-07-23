set colorcolumn=120
set nowrap
filetype plugin indent on
autocmd Filetype cpp setlocal expandtab tabstop=2 shiftwidth=2 softtabstop=2


lua require("packer_bootstrap")
lua require("plugin")
