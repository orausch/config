call plug#begin('~/.vim/plugged')
Plug 'jreybert/vimagit'
Plug 'tpope/vim-fugitive'
Plug 'danilo-augusto/vim-afterglow'
Plug 'bfrg/vim-cpp-modern'
Plug 'vim-scripts/wombat256.vim'
Plug 'vim-scripts/cSyntaxAfter'
call plug#end()
autocmd! FileType c,cpp,java,php call CSyntaxAfter() 
