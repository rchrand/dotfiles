set nobackup
set nowb
set noswapfile
set noerrorbells

" set so=999
set clipboard=unnamedplus
set wildmenu

if has('mac')
    set clipboard=unnamed
endif

" Appearance "

syntax enable

set number
set relativenumber
set linespace=12

set title
set titlestring=%F\ -\ vim
set showmode
set modeline
set laststatus=2

set background=dark

" File behaviour "

set expandtab
set smarttab
" set linebreak
set breakindent
set nostartofline
set nowrap

set shiftwidth=2
set tabstop=2

" Search "

set smartcase
set hlsearch
set incsearch

" Panes "

set splitbelow
set splitright

" Bindings "

let mapleader="\<SPACE>"
nnoremap <silent> <leader>f :Files<CR>
nnoremap <silent> <leader>pf :GFiles<CR>
nnoremap <silent> <leader>b :Buffers<CR>
nnoremap <silent> <leader>a :Ag<CR>
nnoremap <silent> <leader>r :Tags<CR>

" window splitting
nnoremap <leader>wv <C-W>v
nnoremap <leader>ws <C-W>s
nnoremap <silent> <leader>q :q<CR>
nnoremap <silent> <leader>wq :wq<CR>
nnoremap <silent> <leader>ww :w<CR>

" Window movement
nnoremap <C-J> <C-W><C-J>
nnoremap <C-K> <C-W><C-K>
nnoremap <C-L> <C-W><C-L>
nnoremap <C-H> <C-W><C-H>

" Toggle between last 2 buffers
nnoremap <leader><tab> <c-^>

" Explore
nnoremap <silent> <leader>e :Explore<CR>

" Netrw
let g:netrw_liststyle = 3
let g:netrw_banner = 0

"" Plugins

let g:gruvbox_vert_split = 'bg1'
let g:gruvbox_sign_column = 'bg0'

so ~/.config/nvim/plugins.vim

colorscheme gruvbox
hi CursorLineNr ctermfg=white

let g:lightline = {
  \     'active': {
  \         'left': [['mode', 'paste' ], ['readonly', 'filename', 'modified']],
  \         'right': [['lineinfo'], ['percent'], ['gitbranch', 'fileformat', 'fileencoding']]
  \     },
  \     'component_function': {
  \         'gitbranch': 'gitbranch#name'
  \     }
  \ }

let NERDTreeShowHidden=1


let g:ale_fixers = {
\   '*': ['remove_trailing_lines', 'trim_whitespace'],
\   'javascript': ['prettier'],
\   'graphql': ['prettier'],
\   'python': ['black'],
\   'css': ['prettier'],
\   'json': ['prettier'],
\}

let g:ale_fix_on_save = 1
let g:ale_completion_enabled = 1
" let g:ale_ruby_rufo_executable = 'bundle exec rufo'

"" Testing
let test#strategy = {
      \ 'nearest': 'vtr',
      \ 'file':    'vtr',
      \ 'suite':   'vtr',
      \}

nmap <silent> <leader>ts :TestNearest<CR>
nmap <silent> <leader>tf :TestFile<CR>
nmap <silent> <leader>ta :TestSuite<CR>
nmap <silent> <leader>tr :TestLast<CR>
nmap <silent> <leader>tv :TestVisit<CR>
