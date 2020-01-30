"
" > Map the leader key to SPACE
let mapleader="\<SPACE>"

" > Use ; to go into command mode instead of :
" > Saves one keystroke (no need to press shift)
noremap : ;
noremap ; :

" > Use `qp` to go into escape/normal mode
inoremap qp <Esc>
noremap qp <Esc>

set ignorecase
set smartcase

set ruler
set relativenumber

filetype off
filetype plugin indent on

set pastetoggle=<F3>

" Allows git gutter to update faster
set updatetime=100

" Avoid forced end of lines on saving
set nofixendofline

set expandtab
set smarttab
set tabstop=4
set softtabstop=0
set shiftwidth=4

" Specify a directory for plugins
" - For Neovim: ~/.local/share/nvim/plugged
" - Avoid using standard Vim directory names like 'plugin'
call plug#begin('~/.local/share/nvim/plugged')

Plug 'tpope/vim-sensible'
" Plugin outside ~/.vim/plugged with post-update hook
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'

Plug 'flazz/vim-colorschemes'

" jump between hunks with `[c` and `]c`
" Leader hp : Hunk Preview
" Leader hs : Hunk Stage
" Leader hu : Hunk Undo
Plug 'airblade/vim-gitgutter'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-surround'

" Uses C-n, C-x, C-p
Plug 'terryma/vim-multiple-cursors'

" gcc
Plug 'tpope/vim-commentary'

Plug 'itchyny/lightline.vim'

Plug 'preservim/nerdtree'
nnoremap <leader>n :NERDTreeToggle<cr>

Plug 'davidhalter/jedi-vim'

Plug 'numirias/semshi', {'do': ':UpdateRemotePlugins'}

" Better Visual Guide
Plug 'Yggdroot/indentLine'
" syntax check
Plug 'w0rp/ale'
let g:ale_linters = {
      \   'python': ['flake8', 'pylint'],
      \   'javascript': ['eslint'],
      \}

let g:ale_fixers = {
      \   '*': ['remove_trailing_lines', 'trim_whitespace'],
      \   'python': ['black', ],
      \   'javascript': [''],
      \}

Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
let g:deoplete#enable_at_startup = 1

Plug 'deoplete-plugins/deoplete-jedi'
Plug 'majutsushi/tagbar'
Plug 'tpope/vim-sleuth'
Plug 'tmhedberg/SimpylFold'
let g:SimpylFold_docstring_preview=1

" Plug 'Vimjas/vim-python-pep8-indent'

Plug 'francoiscabrol/ranger.vim'
let g:ranger_map_keys = 0
nnoremap - :Ranger<CR>

call plug#end()

au BufNewFile,BufRead *.py
    \ set tabstop=4
    \ softtabstop=4
    \ shiftwidth=4
    \ textwidth=79
    \ expandtab
    \ autoindent
    \ fileformat=unix
    \ foldmethod=indent


set encoding=utf-8

" Flagging Unnecessary Whitespace
au BufRead, BufNewFile *.py,*.pyw,*.c,*.h match BadWhitespace /\s\+$/

au BufNewFile, BufRead *.js, *.html, *.css
    \ set tabstop=2
    \ set softtabstop=2
    \ set shiftwidth=2

nnoremap <C-p> :Files<Cr>
nnoremap <leader>o :Files<cr>
nnoremap <leader>f :Rg<cr>
nnoremap <leader>b :Buffers<cr>
nnoremap <tab> :Buffers<cr>

"split navigations
nnoremap <C-J> <C-W><C-J>
nnoremap <C-K> <C-W><C-K>
nnoremap <C-L> <C-W><C-L>
nnoremap <C-H> <C-W><C-H>

" Enable folding
set foldmethod=indent
set foldlevel=99

colorscheme jellybeans

nnoremap <leader>t :TagbarOpenAutoClose<cr>
let g:python3_host_prog = '/Users/ox/.virtualenvs/neovim/bin/python3'

" Switch to last edited buffer by pressing backspace in normal mode
nnoremap <silent> <BS> :b#<CR>

set completeopt=menuone,noselect,noinsert
set shortmess+=c
inoremap <c-c> <ESC>

" Disable Jedi-vim autocompletion and enable call-signatures options
let g:jedi#auto_initialization = 1
let g:jedi#completions_enabled = 0
let g:jedi#auto_vim_configuration = 0
let g:jedi#smart_auto_mappings = 0
let g:jedi#popup_on_dot = 0
let g:jedi#completions_command = ""
let g:jedi#show_call_signatures = "1"

let g:deoplete#auto_complete_delay = 100

