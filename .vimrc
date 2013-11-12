filetype off
set rtp+=~/.vim/bundle/vundle/
call vundle#rc()

Bundle 'gmarik/vundle'
Bundle 'Lokaltog/vim-powerline', {'rpt': 'powerline/bindings/vim/'}
Bundle 'scrooloose/nerdtree'
Bundle 'plasticboy/vim-markdown'
Bundle 'tpope/vim-fugitive'
Bundle 'vim-ruby/vim-ruby'
Bundle 'kien/ctrlp.vim'
Bundle 'myusuf3/numbers.vim'
Bundle 'Valloric/YouCompleteMe'
Bundle 'lunaru/vim-less'
Bundle 'takac/vim-hardtime'
Bundle 'cakebaker/scss-syntax.vim'
Bundle 'rking/ag.vim'
Bundle 'tpope/vim-rails'
Bundle 'tpope/vim-fireplace'
Bundle 'guns/vim-clojure-static'

set modelines=0
filetype plugin indent on
set t_Co=256            " set 256 color
set nocompatible        " use Vim defaults
set mouse=            " make sure mouse is used in all cases.
syntax enable               " enable syntax highlighting
"set background=dark
colorscheme zenburn     " define syntax color scheme
set shortmess+=I        " disable the welcome screen
"set complete+=k         " enable dictionary completion
"set completeopt+=longest
set backspace=2         " full backspacing capabilities
set history=300         " 300 lines of command line history
set ruler               " ruler display in status line
set ww=<,>,[,]          " whichwrap -- left/right keys can traverse up/down
set cmdheight=2         " set the command height
set showmatch           " show matching brackets (),{},[]
set mat=5               " show matching brackets for 0.3 seconds

set expandtab           " insert spaces instead of tab chars
set tabstop=2           " a n-space tab width
set shiftwidth=2        " allows the use of < and > for VISUAL indenting
set softtabstop=2       " counts n spaces when DELETE or BCKSPCE is used
set autoindent          " auto indents next new line

" Powerline Setup
set guifont=Inconsolata\ 10
"set guifont=Bitstream\ Vera\ Sans\ Mono\ 11
set laststatus=2

let g:ruby_path = system('echo $HOME/.rbenv/shims')

" " searching
set hlsearch            " highlight all search results
set incsearch           " increment search
set ignorecase          " case-insensitive search
set smartcase           " upper-case sensitive search

" " syntax highlighting
set encoding=utf-8
set scrolloff=3
set showmode
set showcmd
set hidden
set wildmenu
set wildmode=list:longest
set cursorline
set ttyfast
set backspace=indent,eol,start
set number
set nohidden
set shell=zsh
set nobackup
set nowritebackup
set noswapfile

set noerrorbells
set novisualbell
set novb
set t_vb=
set t_md=

" Gvim options so it behaves like the terminal vim
set guioptions+=c
set guioptions+=R
set guioptions-=m
set guioptions-=r
set guioptions-=b
set guioptions-=T
set guioptions-=R
set guioptions-=L
set guioptions-=e

let mapleader = ","

nnoremap / /\v
vnoremap / /\v
set gdefault
nnoremap <leader><space> :noh<cr>
nnoremap <tab> %
vnoremap <tab> %

set wrap
set textwidth=79
set formatoptions=qrn1

nnoremap <up> <nop>
nnoremap <down> <nop>
nnoremap <left> <nop>
nnoremap <right> <nop>
inoremap <up> <nop>
inoremap <down> <nop>
inoremap <left> <nop>
inoremap <right> <nop>
nnoremap j gj
nnoremap k gk

inoremap <F1> <ESC>
nnoremap <F1> <ESC>
vnoremap <F1> <ESC>

"au FocusLost * :wa

inoremap jj <ESC>

nnoremap <leader>v <C-w>v<C-w>l
nmap <leader>h :sp<CR>

map <F2> :NERDTreeToggle<CR>

nnoremap <C-h> <C-w>h
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-l> <C-w>l

nmap <C-e> :e#<CR>
nmap <C-n> :bnext<CR> 
nmap <C-b> :noh<CR>

nmap <leader>b :CtrlPBuffer<CR> 
let g:ctrlp_match_window_bottom = 0
let g:ctrlp_match_window_reversed = 0
let g:ctrlp_working_path_mode = 0
let g:ctrlp_dotfiles = 0
let g:ctrlp_switch_buffer = 0

nnoremap <leader>a :Ag

nnoremap <F4> <Esc>:call HardTimeToggle()<CR>
nnoremap <F3> :NumbersToggle<CR>

let g:ycm_min_num_of_chars_for_completion = 4
let g:ycm_complete_in_comments = 0
let g:ycm_autoclose_preview_window_after_insertion = 1

