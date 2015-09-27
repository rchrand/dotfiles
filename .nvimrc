"****** General settings ********"
let mapleader = "," " Leader
" set backspace=2   " Backspace deletes like most programs in insert mode

" Let Git do all the backups
set nobackup
set nowritebackup
set noswapfile
set history=50

" Softtabs, 2 spaces
set tabstop=2
set shiftwidth=2
set shiftround
set expandtab

set clipboard=unnamed " Share clipboard with System

" Finally
set incsearch

" Numbers
set numberwidth=2

"****** Plugins ********"
filetype plugin indent on
" set shell=/bin/bash " Vundle needs this

if (&t_Co > 2 || has("gui_running")) && !exists("syntax_on")
  syntax on
endif

" Bundles
filetype off
set rtp+=~/.vim/bundle/Vundle.vim/
call vundle#begin()

Plugin 'gmarik/Vundle.vim'

" Tpope magic
Plugin 'tpope/vim-endwise'
Plugin 'tpope/vim-fugitive'
Plugin 'tpope/vim-sensible'
Plugin 'tpope/vim-surround'
Plugin 'tpope/vim-obsession'

" Theme
Plugin 'tomasr/molokai'

" General development
Plugin 'Shougo/neocomplete.vim'
Plugin 'scrooloose/syntastic'
Plugin 'vim-scripts/ctags.vim'
Plugin 'vim-scripts/matchit.zip'
Plugin 'vim-scripts/tComment'

" Tmux!
Plugin 'christoomey/vim-tmux-navigator'

" Web development
Plugin 'tpope/vim-rails'
Plugin 'vim-ruby/vim-ruby'
Plugin 'jelera/vim-javascript-syntax'
Plugin 'slim-template/vim-slim'
Plugin 'kchmck/vim-coffee-script'
Plugin 'mattn/emmet-vim'

" Searching
Plugin 'ctrlpvim/ctrlp.vim'
Plugin 'rking/ag.vim'
Plugin 'takac/vim-hardtime'

" Haskell
Plugin 'Shougo/vimproc.vim'
"Plugin 'eagletmt/ghcmod-vim'
Plugin 'chrisdone/hindent'
Plugin 'dag/vim2hs'

" Pandoc"
Plugin 'vim-pandoc/vim-pandoc'
Plugin 'vim-pandoc/vim-pandoc-syntax'

" Prolog
Plugin 'mikeyhc/prolog.vim'

call vundle#end()
filetype on

" Use The Silver Searcher https://github.com/ggreer/the_silver_searcher
if executable('ag')
  " Use Ag over Grep
  set grepprg=ag\ --nogroup\ --nocolor

  " Use ag in CtrlP for listing files. Lightning fast and respects .gitignore
  let g:ctrlp_user_command = 'ag %s -l --nocolor -g ""'

  " ag is fast enough that CtrlP doesn't need to cache
  let g:ctrlp_use_caching = 0
endif

" Tmux cursor
if exists('$TMUX')
  let &t_SI = "\<Esc>Ptmux;\<Esc>\<Esc>]50;CursorShape=1\x7\<Esc>\\"
  let &t_EI = "\<Esc>Ptmux;\<Esc>\<Esc>]50;CursorShape=0\x7\<Esc>\\"
else
  let &t_SI = "\<Esc>]50;CursorShape=1\x7"
  let &t_EI = "\<Esc>]50;CursorShape=0\x7"
endif

" Font
set guifont=Monoid\ Retina:h12

" Colortheme
set t_Co=256
colorscheme molokai

" Ctags
"let g:Tlist_Ctags_Cmd="ctags --exclude='*.js'"

" Index ctags from any project, including those outside Rails
"map <Leader>ct :!ctags -R .<CR>

" Open new split panes to right and bottom, which feels more natural
set splitbelow
set splitright

" Quicker window movement
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-h> <C-w>h
nnoremap <C-l> <C-w>l
"
" map <C-h> <C-w>h
" map <C-j> <C-w>j
" map <C-k> <C-w>k
" map <C-l> <C-w>

" Numbers
nnoremap <leader>n :exec &nu==&rnu? "se nu!" : "se rnu!"<CR>

" Window control
nnoremap <leader>v <C-w>v<C-w>l
nmap <leader>h :sp<CR>

" Syntastic
let g:syntastic_check_on_open=1
let g:syntastic_error_symbol = '✗'
let g:syntastic_style_error_symbol = '✠'
let g:syntastic_warning_symbol = '∆'
let g:syntastic_style_warning_symbol = '≈'

" Always use vertical diffs
set diffopt+=vertical

"Switch between the last two files
nmap <C-e> :e#<CR>

"CtrlP buffer
nnoremap <leader>b :CtrlPBuffer<CR>

" Explore
nmap <leader>e :Explore<cr>

" Macros
nnoremap <Space> @q

" Commands to get out of insert mode
imap jj <Esc>

" Because of Dvorak
nnoremap Q :

" Searching related
nmap <C-b> :noh<CR>
nnoremap <leader>a :Ag

"" Some leader command
nmap <leader>w :w!<cr>
nmap <leader>x :wq<cr>

" Hardtime
let g:hardtime_default_on = 0
nnoremap <leader>q :HardTimeToggle<CR>

" Prolog file type
autocmd BufRead,BufNewFile *.pl set filetype=prolog

" Neocomplete
let g:neocomplete#enable_at_startup = 1
