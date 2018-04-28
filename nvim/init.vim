call plug#begin('~/.vim/plugged')
Plug 'jreybert/vimagit'
Plug 'tpope/vim-fugitive'
Plug 'danilo-augusto/vim-afterglow'
Plug 'bfrg/vim-cpp-modern'
Plug 'vim-scripts/wombat256.vim'
Plug 'vim-scripts/cSyntaxAfter'
call plug#end()
autocmd! FileType c,cpp,java,php call CSyntaxAfter() 

set statusline=%<%f\ %h%m%r%{fugitive#statusline()}%=%-14.(%l,%c%V%)\ %P
