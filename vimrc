call plug#begin('~/.vim/plugged')
Plug 'tpope/vim-fugitive'
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
Plug 'tpope/vim-surround'
Plug 'agude/vim-eldar'
Plug 'mhinz/vim-signify'
Plug 'jsit/disco.vim'
Plug 'rakr/vim-one'
Plug 'itchyny/lightline.vim'
Plug 'vim-scripts/EditPlus'
Plug 'endel/vim-github-colorscheme'
Plug 'nelstrom/vim-mac-classic-theme'
Plug 'skywind3000/quickmenu.vim'
call plug#end()
set laststatus=2

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

set tabstop=4
set shiftwidth=4
set splitbelow
if (has("termguicolors"))
	set termguicolors
endif

syntax enable

colorscheme mac_classic
set background=light


" copy to system clipboard by default
set clipboard=unnamedplus


let g:polyglot_disabled = ['latex', 'python']

let g:vimtex_view_general_viewer = 'zathura'

" GUI
set mouse=a
let g:vimwiki_use_mouse = 1
set guioptions -=m
set guioptions -=T
set guioptions-=L
set guioptions-=r
set mousemodel=popup_setpos
set guifont=Roboto\ Mono\ 11
set guifont=DejaVu\ Sans\ Mono\ Book\ 13

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



highlight SignifySignAdd    	gui=bold guifg=green guibg=NONE cterm=bold ctermfg=green
highlight SignifySignDelete    	gui=bold guifg=red guibg=NONE cterm=bold ctermfg=red
highlight SignifySignChange    	gui=bold guifg=orange guibg=NONE cterm=bold ctermfg=yellow


hi SignColumn guibg=white
hi IncSearch guifg=Darkblue guibg=white
hi Search guifg=black guibg=white
hi LineNr guibg=black

" Mac Classic colorscheme customization
highlight CursorLineNR guibg=#F0F6FF
highlight FoldColumn guibg=white
highlight LineNr guibg=white
highlight SignColumn guibg=white
highlight VertSplit guibg=white guifg=#CFCFCF
highlight TabLineFill guifg=LightGrey

" Signify plugin sign colors
highlight SignifySignAdd    gui=bold guibg=#F5F5F5 guifg=#00BC41
highlight SignifySignChange gui=bold guibg=#F5F5F5 guifg=darkorange
highlight SignifySignDelete gui=bold guibg=#F5F5F5 guifg=red
highlight SignColumn guibg=#F5F5F5 gui=NONE

" Fix Error visibility when in cursor line
highlight Error guibg=#FFDDDD guifg=red gui=bold

" Highlight tabs and trailing spaces with red:		     
highlight SpecialKey guifg=red

" Diff
highlight DiffAdd guibg=#DDFFDD
highlight DiffChange guibg=#FFFFDD
highlight DiffDelete guibg=#FFDDDD
highlight DiffText guibg=#FFFFAA

" Hide tilde from empty lines after file contents
highlight EndOfBuffer guifg=bg

" ColorColumn
highlight ColorColumn guibg=#F5F5F5

call g:quickmenu#reset()
noremap <silent><F12> :call quickmenu#toggle(0)<cr>
call g:quickmenu#append('# Misc', '')
call g:quickmenu#append('vimrc', 'tabe ~/.config/vimrc')
call g:quickmenu#append('# Projects', '')
call g:quickmenu#append('scikit-learn', 'source ~/.vim/sessions/scikit')
call g:quickmenu#append('systems-ex', 'source ~/.vim/sessions/systems')

call g:quickmenu#append('# Zusammenfassungen', '')
call g:quickmenu#append('analysis', 'source ~/.vim/sessions/analysis | VimtexCompile')
call g:quickmenu#append('systems', 'source ~/.vim/sessions/systems-zf | VimtexCompile')


