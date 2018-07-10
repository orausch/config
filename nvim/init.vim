call plug#begin('~/.vim/plugged')
Plug 'tpope/vim-fugitive'
Plug 'danilo-augusto/vim-afterglow'
Plug 'sheerun/vim-polyglot'
Plug 'majutsushi/tagbar'
Plug 'sheerun/vim-polyglot'
Plug '~/.fzf'
Plug 'timonv/vim-cargo'
Plug 'junegunn/fzf.vim'
Plug 'vhdirk/vim-cmake'
call plug#end()

"autocmd! FileType h,c,cpp,java,php call CSyntaxAfter() 


" Important fugitive mappings in :Gstatus
" cc commit
" D diff
" O edit in new tab
set statusline=%<%f\ %h%m%r%{fugitive#statusline()}%=%-14.(%l,%c%V%)\ %P

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


" COLORS {{{
if (has("termguicolors"))
	set termguicolors
endif

syntax enable
set number

colorscheme afterglow
hi LineNr guibg=black
"}}}


" copy to system clipboard by default
set clipboard=unnamedplus
