call plug#begin('~/.vim/plugged')
Plug 'tpope/vim-fugitive'
Plug 'danilo-augusto/vim-afterglow'
Plug 'sheerun/vim-polyglot'
Plug 'majutsushi/tagbar'
Plug 'sheerun/vim-polyglot'
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'
Plug 'timonv/vim-cargo'
Plug 'junegunn/fzf.vim'
Plug 'vhdirk/vim-cmake'
Plug 'lervag/vimtex'
Plug 'vimwiki/vimwiki'
call plug#end()
"autocmd InsertLeave,CompleteDone * if pumvisible() == 0 | pclose | endif
"set statusline=%<%f\ %h%m%r%{fugitive#statusline()}%=%-14.(%l,%c%V%)\ %P
"let g:LanguageClient_serverCommands = {
"    \ 'python': ['pyls'],
"    \ }
"nnoremap <F5> :call LanguageClient_contextMenu()<CR>
"" Or map each action separately
"nnoremap <silent> K :call LanguageClient#textDocument_hover()<CR>
"nnoremap <silent> gd :call LanguageClient#textDocument_definition()<CR>
"nnoremap <silent> <F2> :call LanguageClient#textDocument_rename()<CR>


" Important fugitive mappings in :Gstatus
" cc commit
" D diff
" O edit in new tab
"
nnoremap <F9> :Gstatus<CR>
nnoremap <F10> :Gpush<CR>

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


let g:polyglot_disabled = ['latex']

let g:vimtex_view_general_viewer = 'zathura'
set cursorline

" help lag
set noshowcmd noruler
set mouse=a
set mousemodel=popup
