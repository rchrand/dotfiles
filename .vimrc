filetype off
set rtp+=~/.vim/bundle/vundle/
call vundle#rc()

Bundle 'gmarik/vundle'
Bundle 'Lokaltog/vim-powerline', {'rpt': 'powerline/bindings/vim/'}
Bundle 'scrooloose/nerdtree'
Bundle 'swaroopch/vim-markdown-preview'
Bundle 'tpope/vim-fugitive'
Bundle 'mileszs/ack.vim'
"Bundle 'vim-ruby/vim-ruby'
Bundle 'kien/ctrlp.vim'
Bundle 'wikitopian/hardmode'
Bundle 'myusuf3/numbers.vim'
Bundle 'Valloric/YouCompleteMe'
Bundle 'lunaru/vim-less'

set modelines=0
filetype plugin indent on
set t_Co=256            " set 256 color
set nocompatible        " use Vim defaults
set mouse=a             " make sure mouse is used in all cases.
syntax enable               " enable syntax highlighting
set background=dark
colorscheme solarized     " define syntax color scheme
let g:solarized_termcolors=256
let g:solarized_bold=0
set shortmess+=I        " disable the welcome screen
set complete+=k         " enable dictionary completion
set completeopt+=longest
set backspace=2         " full backspacing capabilities
set history=300         " 300 lines of command line history
set ruler               " ruler display in status line
set ww=<,>,[,]          " whichwrap -- left/right keys can traverse up/down
set cmdheight=2         " set the command height
set showmatch           " show matching brackets (),{},[]
set mat=3               " show matching brackets for 0.3 seconds

set expandtab           " insert spaces instead of tab chars
set tabstop=4           " a n-space tab width
set shiftwidth=4        " allows the use of < and > for VISUAL indenting
set softtabstop=4       " counts n spaces when DELETE or BCKSPCE is used
set autoindent          " auto indents next new line

" Powerline Setup
set guifont=DejaVu\ Sans\ Mono\ for\ Powerline\ 9
set laststatus=2

" " searching
 set hlsearch            " highlight all search results
 set incsearch           " increment search
 set ignorecase          " case-insensitive search
 set smartcase           " upper-case sensitive search
"
" " syntax highlighting
"
set encoding=utf-8
set scrolloff=3
set showmode
set showcmd
set hidden
set wildmenu
set wildmode=list:longest
""set visualbell
set cursorline
set ttyfast
set backspace=indent,eol,start
""set undofile
""set relativenumber
set number

set nobackup
set nowritebackup
set noswapfile

set noerrorbells
set novisualbell
set t_vb=
set t_md=

let mapleader = ","

let g:EasyMotion_leader_key = '<Leader>'

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

au FocusLost * :wa

inoremap jj <ESC>

nnoremap <leader>w <C-w>v<C-w>l

map <F2> :NERDTreeToggle<CR>

nnoremap <C-h> <C-w>h
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-l> <C-w>l

nmap <C-e> :e#<CR>
nmap <C-n> :bnext<CR> 
nmap <C-b> :noh<CR>

nmap <leader>; :CtrlPBuffer<CR>
let g:ctrlp_match_window_bottom = 0
let g:ctrlp_match_window_reversed = 0
let g:ctrlp_working_path_mode = 0
let g:ctrlp_dotfiles = 0
let g:ctrlp_switch_buffer = 0

nnoremap <leader>a :Ack
nnoremap <leader>c :SyntasticCheck<CR>
nnoremap <leader>h <Esc>:call ToggleHardMode()<CR>

nnoremap <F3> :NumbersToggle<CR>

