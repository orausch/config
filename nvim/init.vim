call plug#begin('~/.vim/plugged')
Plug 'tpope/vim-fugitive'
Plug 'danilo-augusto/vim-afterglow'
" Plug 'vim-scripts/cSyntaxAfter'
Plug 'sheerun/vim-polyglot'
Plug 'majutsushi/tagbar'
Plug 'sheerun/vim-polyglot'
Plug '~/.fzf'
Plug 'junegunn/fzf.vim'
Plug 'vhdirk/vim-cmake'
call plug#end()

"autocmd! FileType h,c,cpp,java,php call CSyntaxAfter() 

set statusline=%<%f\ %h%m%r%{fugitive#statusline()}%=%-14.(%l,%c%V%)\ %P

colorscheme afterglow
set number
if has("autocmd")
	filetype indent plugin on
endif
nmap <F8> :TagbarToggle<CR>
map <c-p> :Files<CR>
map <c-l> :Tags<CR>

command Run CMake | make build/

set tabstop=4
set shiftwidth=4

command Format %pyfile /usr/share/vim/addons/syntax/clang-format.py
noremap <Space> @q
