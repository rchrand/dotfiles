""" Plugins  (Vundle)
filetype off
set rtp+=~/.vim/bundle/vundle/
call vundle#rc()

Bundle 'gmarik/vundle'
Bundle 'plasticboy/vim-markdown'
Bundle 'vim-ruby/vim-ruby'
Bundle 'kien/ctrlp.vim'
Bundle 'myusuf3/numbers.vim'
Bundle 'takac/vim-hardtime'
Bundle 'ervandew/supertab'
Bundle 'cakebaker/scss-syntax.vim'
Bundle 'rking/ag.vim'
Bundle 'kien/rainbow_parentheses.vim'
Bundle 'bling/vim-airline'
Bundle 'scrooloose/nerdtree'
Bundle 'scrooloose/syntastic'
Bundle 'scrooloose/nerdcommenter'
Bundle 'tpope/vim-rails'
Bundle 'tpope/vim-fugitive'
Bundle 'tpope/vim-endwise'
Bundle 'tpope/vim-haml'
Bundle "daylerees/colour-schemes", { "rtp": "vim-themes/" }

let g:ruby_path = system('echo $HOME/.rbenv/shims') " Is needed for some plugins
let g:hardtime_default_on = 1
let g:hardtime_showmsg = 0

""" General configs
set modelines=0
filetype plugin on      "Enable filetype-specfic plugings
filetype indent on      "Enable filetype-specfic identing
set nocompatible        " use Vim defaults
set mouse=               " make sure mouse is NOT used in all cases.
set encoding=utf-8

" MacVim specfics commands
autocmd VimLeave * macaction terminate:

""" Visual configs 
set t_Co=256            " set 256 color
syntax enable               " enable syntax highlighting
"set background=dark " needs to be on to control some themes (solarized)
"let g:hybrid_use_Xresources = 1
colorscheme peacocks-in-space

""
set shortmess+=I        " disable the welcome screen
set ruler               " ruler display in status line
set showmatch           " show matching brackets (),{},[]
set mat=5               " show matching brackets for 0.3 seconds

""" Editor configs
set backspace=2         " full backspacing capabilities
set history=300         " 300 lines of command line history
set ww=<,>,[,]          " whichwrap -- left/right keys can traverse up/down
set cmdheight=2         " set the command height
set autoindent          " auto indents next new line

set wrap
set textwidth=79
set formatoptions=qrn1
autocmd FileType * setlocal formatoptions-=c formatoptions-=r formatoptions-=o

""" Identation configs
set expandtab           " insert spaces instead of tab chars
set tabstop=2           " a n-space tab width
set shiftwidth=2        " allows the use of < and > for VISUAL indenting
set softtabstop=2       " counts n spaces when DELETE or BCKSPCE is used
"""
" Powerline Setup
set guifont=Source\ Code\ Pro:h14
set laststatus=2 " Always shows the statusline

""" Searching configs
set hlsearch            " highlight all search results
set incsearch           " increment search
set ignorecase          " case-insensitive search
set smartcase           " upper-case sensitive search
"set scrolloff=5 " Scroll offset used in searching

nnoremap / /\v
vnoremap / /\v

""" Syntax highlighting configs
set showmode " If in Insert, Replace or Visual mode put a message on the last line.
"set showcmd " Shows the last command in the status-line
set hidden " hides the buffer if it's not active
set wildmenu " Tab completion when typing in ex-mode (:)
set wildmode=list:longest
set cursorline " Highlights the cursorline
set ttyfast " Indicates a fast terminal connection
set backspace=indent,eol,start " Makes backspace (Big delete button) work sane
set number " Set line-numbers i think
set shell=zsh

""" Backup configs
set nobackup
set nowritebackup
set noswapfile

""" Sound configs
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

source $VIMRUNTIME/mswin.vim
behave mswin

""" Keybindings config
let mapleader = ","

set gdefault
nnoremap <leader><space> :noh<cr>
nnoremap <tab> %
vnoremap <tab> %

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

nnoremap <leader>v <C-w>v<C-w>l
nmap <leader>h :sp<CR>

nnoremap <C-h> <C-w>h
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-l> <C-w>l

nmap <C-e> :e#<CR>
nmap <C-n> :bnext<CR> 
nmap <C-b> :noh<CR>

""" Plugin keybindings
nmap <leader>b :CtrlPBuffer<CR> 
let g:ctrlp_match_window_bottom = 1
let g:ctrlp_match_window_reversed = 0
let g:ctrlp_working_path_mode = 0
let g:ctrlp_dotfiles = 0
let g:ctrlp_switch_buffer = 0

nnoremap <leader>a :Ag
nnoremap <F4> <Esc>:call HardTimeToggle()<CR>
nnoremap <F3> :NumbersToggle<CR>
nnoremap <Space> @q

let g:vim_markdown_initial_foldlevel=4

let g:rbpt_colorpairs = [
  \ [ '13', '#6c71c4'],
  \ [ '5',  '#d33682'],
  \ [ '1',  '#dc322f'],
  \ [ '9',  '#cb4b16'],
  \ [ '3',  '#b58900'],
  \ [ '2',  '#859900'],
  \ [ '6',  '#2aa198'],
  \ [ '4',  '#268bd2'],
  \ ]

au VimEnter * RainbowParenthesesToggle
au Syntax * RainbowParenthesesLoadRound
au Syntax * RainbowParenthesesLoadSquare
au Syntax * RainbowParenthesesLoadBraces

nnoremap <leader>n :NERDTreeToggle<CR>

" Airline
let g:airline_right_sep = ' « '
let g:airline_left_sep = ' » '
let g:airline_theme="base16"

