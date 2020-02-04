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

" Enable persistent undo so that undo history persists across vim sessions
set undofile
set undodir=~/.vim/undo

" Specify a directory for plugins
" - For Neovim: ~/.local/share/nvim/plugged
" - Avoid using standard Vim directory names like 'plugin'
call plug#begin('~/.local/share/nvim/plugged')

Plug 'arcticicestudio/nord-vim'
Plug 'psliwka/vim-smoothie'
Plug 'tpope/vim-sensible'
Plug 'tpope/vim-unimpaired'
Plug 'tpope/vim-sleuth'
Plug 'tmhedberg/SimpylFold'
let g:SimpylFold_docstring_preview=1
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-surround'
" Uses C-n, C-x, C-p
Plug 'terryma/vim-multiple-cursors'
" gcc to comment
Plug 'tpope/vim-commentary'
Plug 'itchyny/lightline.vim'
Plug 'Yggdroot/indentLine'
let g:lightline = {
      \ 'colorscheme': 'nord',
      \ }
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'
Plug 'rbgrouleff/bclose.vim'
Plug 'francoiscabrol/ranger.vim'
let g:ranger_map_keys = 0
nnoremap - :Ranger<CR>

Plug 'simnalamburt/vim-mundo'
nnoremap <F5> :MundoToggle<CR>

Plug 'SirVer/ultisnips'
Plug 'honza/vim-snippets'

" Trigger configuration. Do not use <tab> if you use https://github.com/Valloric/YouCompleteMe.
let g:UltiSnipsExpandTrigger="<tab>"
let g:UltiSnipsJumpForwardTrigger="<c-b>"
let g:UltiSnipsJumpBackwardTrigger="<c-z>"

" If you want :UltiSnipsEdit to split your window.
let g:UltiSnipsEditSplit="vertical"

" jump between hunks with `[c` and `]c`
" Leader hp : Hunk Preview
" Leader hs : Hunk Stage
" Leader hu : Hunk Undo
Plug 'airblade/vim-gitgutter'
Plug 'tpope/vim-fugitive'
autocmd User fugitive
  \ if fugitive#buffer().type() =~# '^\%(tree\|blob\)$' |
  \   nnoremap <buffer> .. :edit %:h<CR> |
  \ endif


Plug 'preservim/nerdtree', { 'on':  'NERDTreeToggle' }
Plug 'Xuyuanp/nerdtree-git-plugin'
nnoremap <leader><tab> :NERDTreeToggle<cr>
" autocmd StdinReadPre * let s:std_in=1
" open a NERDTree automatically when vim starts up if no files were specified
" autocmd VimEnter * if argc() == 0 && !exists("s:std_in") | NERDTree | endif

" IDE Plugins
Plug 'davidhalter/jedi-vim'
Plug 'numirias/semshi', {'do': ':UpdateRemotePlugins'}
Plug 'w0rp/ale'
let g:ale_fix_on_save = 1
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
" Plug 'Vimjas/vim-python-pep8-indent'

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

command! -bang -nargs=* Rg
  \ call fzf#vim#grep(
  \   'rg --column --line-number --no-heading --color=always --smart-case '.shellescape(<q-args>), 1,
  \   fzf#vim#with_preview(), <bang>0)

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

nnoremap <leader>t :TagbarOpenAutoClose<cr>
let s:uname = system("uname")
if s:uname =~ "Linux"
  let g:python3_host_prog = expand('~/.virtualenvs/neovim/bin/python3')
elseif s:uname =~ "Darwin"
  let g:python3_host_prog = '/Users/ox/.virtualenvs/neovim/bin/python3'
endif

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

set grepprg=rg\ --vimgrep\ --smart-case

if &term =~# '256color' && ( &term =~# '^screen'  || &term =~# '^tmux' )
    let &t_8f = "\<Esc>[38;2;%lu;%lu;%lum"
    let &t_8b = "\<Esc>[48;2;%lu;%lu;%lum"
    set termguicolors
endif
set background=dark
set t_Co=256
let g:nord_uniform_diff_background=1
colorscheme nord
" colorscheme slate
hi clear Visual
hi Visual guibg=#345456
set fillchars=diff:\ ,fold:\ ,
