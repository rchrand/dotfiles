call plug#begin('~/.vim/plugged')
Plug 'benmills/vimux'
Plug 'tpope/vim-sensible'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-rails'
Plug 'tpope/vim-endwise'
Plug 'tpope/vim-vinegar'
Plug 'tpope/vim-surround'
Plug 'spf13/vim-autoclose'
Plug 'altercation/vim-colors-solarized'
Plug 'vim-pandoc/vim-pandoc-syntax'
Plug 'christoomey/vim-tmux-navigator'
Plug 'ctrlpvim/ctrlp.vim'
Plug 'vim-scripts/tComment'
Plug 'rking/ag.vim'
Plug 'neomake/neomake'
Plug 'jnurmine/Zenburn'
Plug 'jszakmeister/vim-togglecursor'
" Plug 'scrooloose/syntastic'

Plug 'justinmk/vim-syntax-extra'
Plug 'sheerun/vim-polyglot'

" Javascript
Plug 'moll/vim-node'
Plug 'jiangmiao/simple-javascript-indenter'

Plug 'skalnik/vim-vroom'

Plug 'junegunn/rainbow_parentheses.vim' " Awesome for everything with parentheses!

Plug 'ervandew/supertab'
call plug#end()

" Sane defaults for vim
set autoindent
set magic
set nobackup
set noswapfile          
set nowritebackup
set showcmd
set showmatch
set smartcase
set noerrorbells
set novisualbell
set relativenumber 
set number 

" Leader
let mapleader = "," 
nnoremap <leader>w :w<CR>
nnoremap <leader>x :wq<CR>

" Ag
let g:ag_prg="ag --vimgrep --smart-case"

nnoremap <leader>a :Ag
" Tabs and spaces
set tabstop=2
set shiftwidth=2
set expandtab

"Searching 
set hlsearch
set incsearch 
set lazyredraw 

" Color theme
syntax enable
" set background=light
colorscheme zenburn

" Because of Dvorak
nnoremap Q : 
inoremap jk <ESC>
"Switch between the last two files
nmap <C-e> :e#<CR>

" Javascript
nmap <Leader>cl yiwoconsole.log('<c-r>"', <c-r>")<Esc>^

" Share that damn clipboard
set clipboard=unnamed " Share clipboard with System

" Word wrapping
set textwidth=95
set tw=95
set formatoptions+=t
set nowrap

" CtrlP usage of ag
" let g:ctrlp_working_path_mode = 'wa'
if executable('ag')
  " Use Ag over Grep
  set grepprg=ag\ --nogroup\ --nocolor

  " Use ag in CtrlP for listing files. Lightning fast and respects .gitignore
  let g:ctrlp_user_command = 'ag %s -l --nocolor -g ""'

  " ag is fast enough that CtrlP doesn't need to cache
  let g:ctrlp_use_caching = 0
endif

let g:ctrlp_custom_ignore = '\v[\/](node_modules|target|dist)|(\.(swp|ico|git|svn))$'

" Toggle numbers
nnoremap <leader>n :exec &nu==&rnu? "se nu!" : "se rnu!"<CR>

" Window control
nnoremap <leader>v <C-w>v<C-w>l
nmap <leader>h :sp<CR>

" Ctrl P
nnoremap <leader>b :CtrlPBuffer<CR>

" Movement
map <C-j> <C-W>j<C-W>_
map <C-k> <C-W>k<C-W>_
map <C-h> <C-W>h<C-W>_
map <C-l> <C-W>l<C-W>_

" Clear search
nnoremap <silent><CR> :noh<CR><CR>

" Netrw
map <leader>e :Explore<CR>

let g:netrw_banner       = 0
let g:netrw_keepdir      = 0
let g:netrw_liststyle    = 1 " or 3
let g:netrw_sort_options = 'i'

autocmd VimEnter * if !argc() | Explore | endif

" Neomake
let g:neomake_verbose = 0
let g:neomake_ruby_enabled_makers = ['rubocop']
let g:neomake_javascript_enabled_makers = ['standard']
let g:neomake_jsx_enabled_makers = ['standard']

autocmd! BufWritePost * Neomake

" Standard
let g:jsx_ext_required = 0

" Rename current file, thanks Gary Bernhardt via Ben Orenstein
function! RenameFile()
  let old_name = expand('%')
  let new_name = input('New file name: ', expand('%'), 'file')
  if new_name != '' && new_name != old_name
    exec ':saveas ' . new_name
    exec ':silent !rm ' . old_name
    redraw!
  endif
endfunction
map <leader>mv :call RenameFile()<cr>

let g:vroom_use_spring = 0
let g:vroom_use_dispatch = 0
let g:vroom_clear_screen = 1
let g:vroom_use_colors = 1
let g:vroom_use_vimux = 1
let g:vroom_use_binstubs = 1
let g:vroom_write_all  = 0
let g:vroom_test_unit_command = 'm'
nmap <silent> <leader>R :VroomRunNearestTest<CR>
nmap <silent> <leader>r :VroomRunTestFile<CR>
nmap <silent> <leader>l :VroomRunLastTest<CR>
nmap <silent> <leader>a :TestSuite<CR>

" Journal
:nnoremap <F5> "=strftime("%a %d/%m - %H:%M:%S")<CR>P
:inoremap <F5> <C-R>=strftime("%a %d/%m - %H:%M:%S")<CR>

" Python
set statusline=%F%m%r%h%w\ [TYPE=%Y\ %{&ff}]\ [%l/%L]\ (%p%%)
filetype plugin indent on
au FileType py set autoindent

fun! <SID>StripTrailingWhitespaces()
    let l = line(".")
    let c = col(".")
    %s/\s\+$//e
    call cursor(l, c)
endfun

autocmd FileType c,cpp,java,php,ruby,python autocmd BufWritePre <buffer> :call <SID>StripTrailingWhitespaces()
