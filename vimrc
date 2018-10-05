call plug#begin('~/.vim/plugged')
Plug 'tpope/vim-fugitive'
Plug 'danilo-augusto/vim-afterglow'
Plug 'sheerun/vim-polyglot'
Plug 'majutsushi/tagbar'
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'
Plug 'vhdirk/vim-cmake'
Plug 'lervag/vimtex'
Plug 'vimwiki/vimwiki'
Plug 'jremmen/vim-ripgrep'
Plug 'nvie/vim-flake8'
Plug 'Valloric/YouCompleteMe'
Plug 'NLKNguyen/papercolor-theme'
Plug 'tpope/vim-surround'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'sjl/badwolf'
Plug 'mhinz/vim-signify'
call plug#end()
" Important fugitive mappings in :Gstatus
" cc commit
" D diff
" O edit in new tab

"idks
if has("autocmd")
	filetype indent plugin on
endif
set noshowcmd noruler
set nocompatible

" MAPS
nnoremap <F9> :Gstatus<CR>
nnoremap <F8> :TagbarToggle<CR>
nnoremap <c-p> :Files<CR>
nnoremap <c-l> :Tags<CR>
nnoremap <Space><Space> @q

nnoremap <C-c> :ccl<CR>:pc<CR>
nnoremap <f3> :YcmCompleter GetDoc<CR>
nnoremap <f4> :YcmCompleter GoTo<CR>
let g:ycm_goto_buffer_command = 'same_buffer'
let g:ycm_autoclose_preview_window_after_completion = 1

set background=dark
set tabstop=4
set shiftwidth=4
set splitbelow
if (has("termguicolors"))
	set termguicolors
endif

syntax enable

colorscheme badwolf
let g:airline_theme='badwolf'
hi LineNr guibg=black


" copy to system clipboard by default
set clipboard=unnamedplus


let g:polyglot_disabled = ['latex', 'python']

let g:vimtex_view_general_viewer = 'zathura'

" GUI
set mouse=a
set mousemodel=popup_setpos
let g:vimwiki_use_mouse = 1
set guioptions -=m
set guioptions -=T
set guioptions-=L
set guioptions-=r
set guifont=Roboto\ Mono\ 11

" WRAPPING
set wrap
set textwidth=79
set colorcolumn=79
au BufRead,BufNewFile *.tex setlocal textwidth=120
au BufRead,BufNewFile *.tex setlocal colorcolumn=120

" SEARCH
set hlsearch
set ignorecase
set smartcase
set incsearch

set scrolloff=5
" Automatically open, but do not go to (if there are errors) the quickfix /
" location list window, or close it when is has become empty.
"
" Note: Must allow nesting of autocmds to enable any customizations for quickfix
" buffers.
" Note: Normally, :cwindow jumps to the quickfix window if the command opens it
" (but not if it's already open). However, as part of the autocmd, this doesn't
" seem to happen.
autocmd QuickFixCmdPost [^l]* nested cwindow
autocmd QuickFixCmdPost    l* nested lwindow

set belloff=all

let g:fzf_colors =
\ { 'fg':      ['fg', 'Normal'],
  \ 'bg':      ['bg', 'Normal'],
  \ 'hl':      ['fg', 'Comment'],
  \ 'fg+':     ['fg', 'CursorLine', 'CursorColumn', 'Normal'],
  \ 'bg+':     ['bg', 'CursorLine', 'CursorColumn'],
  \ 'hl+':     ['fg', 'Statement'],
  \ 'info':    ['fg', 'PreProc'],
  \ 'border':  ['fg', 'Ignore'],
  \ 'prompt':  ['fg', 'Conditional'],
  \ 'pointer': ['fg', 'Exception'],
  \ 'marker':  ['fg', 'Keyword'],
  \ 'spinner': ['fg', 'Label'],
  \ 'header':  ['fg', 'Comment'] }

let g:airline#parts#ffenc#skip_expected_string='utf-8[unix]'
let g:airline#extensions#tagbar#enabled = 0

highlight SignifySignAdd    	gui=bold guifg=green cterm=bold ctermfg=green
highlight SignifySignDelete    	gui=bold guifg=red cterm=bold ctermfg=red
highlight SignifySignChange    	gui=bold guifg=orange cterm=bold ctermfg=yellow
let g:airline_section_x=''
