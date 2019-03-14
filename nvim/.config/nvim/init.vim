set t_Co=256
call plug#begin('~/.vim/plugged')
Plug 'tpope/vim-fugitive'
Plug 'sheerun/vim-polyglot'
Plug 'majutsushi/tagbar'
Plug 'junegunn/fzf.vim'
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'lervag/vimtex'

Plug 'vimwiki/vimwiki'
Plug 'jremmen/vim-ripgrep'
Plug 'nvie/vim-flake8'
"Plug 'Valloric/YouCompleteMe'
Plug 'tpope/vim-surround'
Plug 'mhinz/vim-signify'
Plug 'skywind3000/quickmenu.vim'
Plug 'NLKNguyen/papercolor-theme'
Plug 'tpope/vim-commentary'
Plug 'alfredodeza/pytest.vim'

Plug 'ncm2/ncm2'
Plug 'roxma/nvim-yarp'
Plug 'ncm2/ncm2-jedi'
Plug 'ncm2/ncm2-bufword'
Plug 'ncm2/ncm2-path'
Plug 'numirias/semshi'

"colors
Plug 'nelstrom/vim-mac-classic-theme'
Plug 'danilo-augusto/vim-afterglow'
call plug#end()

autocmd BufEnter * call ncm2#enable_for_buffer()
set completeopt=noinsert,menuone,noselect

set laststatus=2
colorscheme PaperColor
set background=dark

nmap <silent> <leader>r :Semshi rename<CR>
nmap <silent> <Tab> :Semshi goto name next<CR>
nmap <silent> <S-Tab> :Semshi goto name prev<CR>

nmap <silent> <leader>c :Semshi goto class next<CR>
nmap <silent> <leader>C :Semshi goto class prev<CR>

nmap <silent> <leader>f :Semshi goto function next<CR>
nmap <silent> <leader>F :Semshi goto function prev<CR>

nmap <silent> <leader>ee :Semshi error<CR>
nmap <silent> <leader>ge :Semshi goto error<CR>

if has("autocmd")
	filetype indent plugin on
endif
set noshowcmd noruler
set nocompatible
" MAPS
nnoremap <silent><Leader>tf <Esc>:Pytest file<CR>
nnoremap <silent><Leader>tc <Esc>:Pytest class<CR>
nnoremap <silent><Leader>tm <Esc>:Pytest function<CR>
nnoremap <silent><Leader>tb <Esc>:TagbarToggle<CR>

nnoremap <silent><Leader>g :Gstatus<CR>
nnoremap <F10> :VimtexTocToggle<CR>
nnoremap <c-p> :Files<CR>
nnoremap <c-l> :Tags<CR>
nnoremap <c-k> :BTags<CR>
nnoremap <Space><Space> @q

nnoremap <C-c> :ccl<CR>:pc<CR>
nnoremap K :YcmCompleter GetDoc<CR>
nnoremap <f4> :YcmCompleter GoTo<CR>

set tabstop=4
set shiftwidth=4
set splitbelow
if (has("termguicolors"))
endif

syntax enable



" copy to system clipboard by default
set clipboard=unnamedplus


"vimtex
let g:polyglot_disabled = ['latex', 'python']

let g:vimtex_view_general_viewer = 'zathura'

let g:vimtex_quickfix_latexlog = {
			\ 'overfull' : 0,
			\ 'underfull' : 0,
			\ 'packages' : {
			\   'default' : 0,
			\ },
			\}

" GUI
let g:vimwiki_use_mouse = 1
set mouse=a

" WRAPPING
"set wrap
set textwidth=79
set colorcolumn=79
au BufRead,BufNewFile *.tex setlocal textwidth=100
au BufRead,BufNewFile *.tex setlocal colorcolumn=100


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
"autocmd QuickFixCmdPost [^l]* nested cwindow
"autocmd QuickFixCmdPost    l* nested lwindow

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



"highlight SignifySignAdd    	gui=bold guifg=green guibg=NONE cterm=bold ctermfg=green
"highlight SignifySignDelete    	gui=bold guifg=red guibg=NONE cterm=bold ctermfg=red
"highlight SignifySignChange    	gui=bold guifg=orange guibg=NONE cterm=bold ctermfg=yellow
"

hi SignColumn ctermbg=black
""hi IncSearch guifg=Darkblue guibg=white
"hi LineNr guibg=black

"colorscheme mac_classic
"" Mac Classic colorscheme customization
"highlight CursorLineNR guibg=#F0F6FF
"highlight FoldColumn guibg=white
"highlight LineNr guibg=white
"highlight SignColumn guibg=white
"highlight VertSplit guibg=white guifg=#CFCFCF
"highlight TabLineFill guifg=LightGrey
"
"" Signify plugin sign colors
"highlight SignifySignAdd    gui=bold guibg=#F5F5F5 guifg=#00BC41
"highlight SignifySignChange gui=bold guibg=#F5F5F5 guifg=darkorange
"highlight SignifySignDelete gui=bold guibg=#F5F5F5 guifg=red
"highlight SignColumn guibg=#F5F5F5 gui=NONE
"
"" Fix Error visibility when in cursor line
"highlight Error guibg=#FFDDDD guifg=red gui=bold
"
"" Highlight tabs and trailing spaces with red:		     
"highlight SpecialKey guifg=red
"
"" Diff
"highlight DiffAdd guibg=#DDFFDD
"highlight DiffChange guibg=#FFFFDD
"highlight DiffDelete guibg=#FFDDDD
"highlight DiffText guibg=#FFFFAA
"
"" Hide tilde from empty lines after file contents
"highlight EndOfBuffer guifg=bg
"
"" ColorColumn
"highlight ColorColumn guibg=#F5F5F5

call g:quickmenu#reset()
noremap <silent><F12> :call quickmenu#toggle(0)<cr>
call g:quickmenu#append('# Misc', '')
call g:quickmenu#append('vimrc', 'tabe ~/.config/nvim/init.vim')
call g:quickmenu#append('# Projects', '')
call g:quickmenu#append('araneum HN', 'cd ~/araneum/hidden-networks/')
call g:quickmenu#append('scikit-learn', 'source ~/.vim/sessions/scikit')
call g:quickmenu#append('systems-ex', 'source ~/.vim/sessions/systems')

call g:quickmenu#append('# Zusammenfassungen', '')
call g:quickmenu#append('analysis', 'source ~/.vim/sessions/analysis | VimtexCompile')
call g:quickmenu#append('economics', 'source ~/.vim/sessions/econ | VimtexCompile')
call g:quickmenu#append('systems', 'source ~/.vim/sessions/systems-zf | VimtexCompile')
call g:quickmenu#append('ti', 'source ~/.vim/sessions/ti')


if has("gui_running")
	set go=c "use TUI in gvim
	set guioptions -=m
	set guioptions -=T
	set guioptions-=L
	set guioptions-=r
	set mousemodel=popup_setpos
	set guifont=Roboto\ Mono\ 11
	set guifont=DejaVu\ Sans\ Mono\ Book\ 13
endif

let g:vimwiki_list = [{'path': '~/Documents/vimwiki/'}]

set number
set smartcase
set statusline+=%F
