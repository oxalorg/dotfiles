" (neo)vimrc of @oxalorg

" map the leader key to SPACE
let mapleader="\<SPACE>"

set shell=/bin/bash

" install vim-plug if not already there
if empty(glob('~/.local/share/nvim/site/autoload/plug.vim'))
  silent !curl -fLo ~/.local/share/nvim/site/autoload/plug.vim --create-dirs
    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  augroup install_vim_plug
      autocmd!
      autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
  augroup END
endif

" shortcut to editing vimrc quickly
nnoremap <silent> <leader>nve :e $MYVIMRC<CR>

" shortcut to reload vimrc without restarting vim
nnoremap <silent> <leader>nvr :so $MYVIMRC<CR>

" Use ; to go into command mode instead of :
" Saves one keystroke (no need to press shift)
noremap : ;
noremap ; :

" Use `qp` to go into escape/normal mode
inoremap qp <Esc>
noremap qp <Esc>

" Basic Settings
set ignorecase
set smartcase
set ruler
set number
set relativenumber
filetype off
filetype plugin indent on

" use F3 to enter paste mode -> paste -> F3 to exit paste mode
" this allows to paste without vim messing up the indents
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

" theme
Plug 'tomasiser/vim-code-dark'


" makes Ctrl-D and Ctrl-U Smooooooth
Plug 'psliwka/vim-smoothie'

" Some sensible defaults
Plug 'tpope/vim-sensible'

" automatic pairing of parens, braces, quotes etc.
Plug 'Raimondi/delimitMate'

" auto detect indent/tab/space settings based on existing file
" Plug 'tpope/vim-sleuth'

Plug 'tmhedberg/SimpylFold'
let g:SimpylFold_docstring_preview=1

"Plug 'dkarter/bullets.vim'
Plug 'justinmk/vim-sneak'
Plug 'tpope/vim-unimpaired'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-surround'
Plug 'markonm/traces.vim'

" Uses C-n, C-x, C-p
Plug 'terryma/vim-multiple-cursors'

" gcc to comment
Plug 'tpope/vim-commentary'

Plug 'itchyny/lightline.vim'
Plug 'Yggdroot/indentLine'
let g:lightline = {
      \ 'colorscheme': 'nord',
      \ 'component_function': {
      \   'filename': 'LightlineFilename',
      \ },
  \ }

function! LightlineFilename()
    return expand('%:.')
  endfunction

" fuzzy finder
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'
Plug 'rbgrouleff/bclose.vim'

" use ranger integration
Plug 'francoiscabrol/ranger.vim'
let g:ranger_map_keys = 0
nnoremap - :Ranger<CR>

" Plug 'ConradIrwin/vim-bracketed-paste'
" Plugin 'chamindra/marvim'
Plug 'simnalamburt/vim-mundo'
"nnoremap <F5> :MundoToggle<CR>

Plug 'SirVer/ultisnips'
Plug 'honza/vim-snippets'

" Trigger configuration. Do not use <tab> if you use https://github.com/Valloric/YouCompleteMe.
let g:UltiSnipsExpandTrigger="<c-s>"
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

" Plug 'vimwiki/vimwiki'
" let g:vimwiki_list = [{'path': '~/vimwiki/',
"                       \ 'syntax': 'markdown', 'ext': '.md'}]
" let g:vimwiki_folding = ''
"":nmap <Leader>wb <Plug>VimwikiGoBackLink

" IDE Plugins
Plug 'mattn/emmet-vim'
Plug 'prettier/vim-prettier'
Plug 'fatih/vim-go', { 'do': ':GoUpdateBinaries' }

" language pack
Plug 'sheerun/vim-polyglot'
" disable vim-go :GoDef short cut (gd)
" this is handled by LanguageClient [LC]
let g:go_def_mapping_enabled = 0
Plug 'dense-analysis/ale'
let g:ale_set_highlights = 0
let g:ale_echo_msg_format = '%linter%: %s'
let g:ale_linters = {
      \   'python': ['flake8',],
      \   'javascript': ['eslint'],
      \   'html': ['eslint'],
      \}

let g:ale_python_black_executable = '~/.virtualenvs/neovim/bin/black'
" let g:ale_fixers = {
"       \   '*': ['remove_trailing_lines', 'trim_whitespace'],
"       \   'python': ['black',],
"       \   'javascript': ['eslint',],
"       \}
let g:ale_javascript_prettier_use_global = 1
let g:ale_fixers = {
      \   'python': ['isort', 'black'],
      \   'javascript': ['prettier',],
      \   'javascriptreact': ['prettier',],
      \}
      " \   'htmldjango': ['html-beautify'],
let g:ale_fix_on_save = 1

Plug 'davidhalter/jedi-vim'
Plug 'numirias/semshi', {'do': ':UpdateRemotePlugins' }
" Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins', 'on': [] }
" Plug 'deoplete-plugins/deoplete-jedi', { 'on': [] }

" augroup deoplete_py
"   autocmd!
"   autocmd FileType python call plug#load('deoplete.nvim', 'deoplete-jedi')
"                          \| call deoplete#enable()
"                          \| autocmd! deoplete_py
" augroup END

" Plug 'neoclide/coc.nvim', {'branch': 'release', 'on': []}
Plug 'neoclide/coc.nvim', {'branch': 'release' }
" autocmd FileType go call plug#load('coc.nvim')
" autocmd FileType javascript call plug#load('coc.nvim')
" autocmd FileType html,htmldjango call plug#load('coc.nvim')


Plug 'ludovicchabant/vim-gutentags'
Plug 'majutsushi/tagbar'
" Plug 'Vimjas/vim-python-pep8-indent'
"
Plug 'tpope/vim-endwise'

call plug#end()

au BufNewFile,BufRead *.py
    \ set tabstop=4
    \ softtabstop=4
    \ shiftwidth=4
    \ textwidth=150
    \ expandtab
    \ autoindent
    \ fileformat=unix
    \ foldmethod=indent


set encoding=utf-8

" Flagging Unnecessary Whitespace
au BufRead, BufNewFile *.py,*.pyw,*.c,*.h match BadWhitespace /\s\+$/

autocmd FileType htmldjango setlocal shiftwidth=2 softtabstop=2 tabstop=2 expandtab
autocmd FileType html setlocal shiftwidth=2 softtabstop=2 tabstop=2 expandtab
autocmd FileType css setlocal shiftwidth=2 softtabstop=2 tabstop=2 expandtab
autocmd FileType javascript setlocal shiftwidth=2 softtabstop=2 tabstop=2 expandtab
autocmd FileType javascriptreact setlocal shiftwidth=2 softtabstop=2 tabstop=2 expandtab

command! -bang -nargs=* Rg
  \ call fzf#vim#grep(
  \   'rg --column --line-number --no-heading --color=always --smart-case '.shellescape(<q-args>), 1,
  \   fzf#vim#with_preview(), <bang>0)

" Insert mode completion
imap <c-x><c-k> <plug>(fzf-complete-word)
imap <c-x><c-f> <plug>(fzf-complete-path)
imap <c-x><c-j> <plug>(fzf-complete-file-ag)
imap <c-x><c-l> <plug>(fzf-complete-line)

inoremap <expr> <c-x><c-l> fzf#vim#complete(fzf#wrap({
  \ 'prefix': '^.*$',
  \ 'source': 'rg -n ^ --color always',
  \ 'options': '--ansi --delimiter : --nth 3..',
  \ 'reducer': { lines -> join(split(lines[0], ':\zs')[2:], '') }}))

" command! -bang -nargs=* Ag
"   \ call fzf#vim#grep(
"   \   'ag --column --numbers --noheading --smart-case . '.shellescape(<q-args>), 1,
"   \   fzf#vim#with_preview(), <bang>0)

nnoremap <C-p> :Files<Cr>
nnoremap <leader>o :Files<cr>
nnoremap <leader>f :Rg<cr>
nnoremap <leader>bd :bwipeout<cr>
nnoremap <tab> :Buffers<cr>
nnoremap <leader>tt :Tags<cr>
nnoremap <leader>tb :TagbarOpenAutoClose<cr>

"split navigations
nnoremap <C-J> <C-W><C-J>
nnoremap <C-K> <C-W><C-K>
nnoremap <C-L> <C-W><C-L>
nnoremap <C-H> <C-W><C-H>

" Enable folding
set foldmethod=indent
set foldlevel=99

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
colorscheme codedark
" hi clear Visual
" hi Visual guibg=#345456
set fillchars=diff:\ ,fold:\ ,

set diffopt+=vertical,iwhite
if &diff
  " diff mode
  set diffopt+=iwhite
endif

let g:go_fmt_command = "goimports"
let g:go_fmt_autosave=1

set guicursor=n-v-c:block,i-ci-ve:ver25,r-cr:hor20,o:hor50
      \,a:blinkwait700-blinkoff400-blinkon250-Cursor/lCursor
      \,sm:block-blinkwait175-blinkoff150-blinkon175

highl CursorTransparent ctermfg=16 ctermbg=253 guifg=#000000 guibg=#00FF00 gui=strikethrough blend=100

" ========================= COC.NVIM ===============================
nnoremap <silent> <space>y  :<C-u>CocList -A --normal yank<cr>
" if hidden is not set, TextEdit might fail.
set hidden
" Better display for messages
set cmdheight=2
" Smaller updatetime for CursorHold & CursorHoldI
set updatetime=300
" don't give |ins-completion-menu| messages.
set shortmess+=c
" always show signcolumns
set signcolumn=yes

" Use tab for trigger completion with characters ahead and navigate.
" NOTE: Use command ':verbose imap <tab>' to make sure tab is not mapped by
" other plugin before putting this into your config.
inoremap <silent><expr> <TAB>
      \ pumvisible() ? "\<C-n>" :
      \ <SID>check_back_space() ? "\<TAB>" :
      \ coc#refresh()
inoremap <expr><S-TAB> pumvisible() ? "\<C-p>" : "\<C-h>"

inoremap <silent><expr> <C-Enter>
      \ pumvisible() ? "\<C-y" :
      \ coc#expandableOrJumpable() ? "\<C-r>=coc#rpc#request('doKeymap', ['snippets-expand-jump',''])\<CR>" :
      \ <SID>check_back_space() ? "\<TAB>" :
      \ coc#refresh()

function! s:check_back_space() abort
  let col = col('.') - 1
  return !col || getline('.')[col - 1]  =~# '\s'
endfunction

" highlight DiffChange ctermbg=1
" highlight DiffText ctermbg=22

highlight GitGutterAdd    guifg=#009900 guibg=#1e1e1e ctermfg=2 ctermbg=234
highlight GitGutterChange guifg=#bbbb00 guibg=#1e1e1e ctermfg=3 ctermbg=234
highlight GitGutterDelete guifg=#ff2222 guibg=#1e1e1e ctermfg=1 ctermbg=234


let g:mundo_width = 88
let g:mundo_preview_height = 20
let g:mundo_right = 0

" Bind F5 to save file if modified and execute python script in a buffer.
nnoremap <silent> <F5> :call SaveAndExecutePython()<CR>
vnoremap <silent> <F5> :<C-u>call SaveAndExecutePython()<CR>

function! SaveAndExecutePython()
    " SOURCE [reusable window]: https://github.com/fatih/vim-go/blob/master/autoload/go/ui.vim

    " save and reload current file
    silent execute "update | edit"

    " get file path of current file
    let s:current_buffer_file_path = expand("%")

    let s:output_buffer_name = "Python"
    let s:output_buffer_filetype = "output"

    " save source buffer
    let s:source_buf_nr = bufnr('%')

    " reuse existing buffer window if it exists otherwise create a new one
    if !exists("s:buf_nr") || !bufexists(s:buf_nr)
        silent execute 'botright vertical new ' . s:output_buffer_name
        let s:buf_nr = bufnr('%')
    elseif bufwinnr(s:buf_nr) == -1
        silent execute 'botright new'
        silent execute s:buf_nr . 'buffer'
    elseif bufwinnr(s:buf_nr) != bufwinnr('%')
        silent execute bufwinnr(s:buf_nr) . 'wincmd w'
    endif

    silent execute "setlocal filetype=" . s:output_buffer_filetype
    setlocal bufhidden=delete
    setlocal buftype=nofile
    setlocal noswapfile
    setlocal nobuflisted
    setlocal winfixheight
    setlocal cursorline " make it easy to distinguish
    setlocal nonumber
    setlocal norelativenumber
    setlocal showbreak=""

    " clear the buffer
    setlocal noreadonly
    setlocal modifiable
    %delete _

    " add the console output
    silent execute ".!python " . shellescape(s:current_buffer_file_path, 1)

    " resize window to content length Note: This is annoying because if you
    " print a lot of lines then your code buffer is forced to a height of one
    " line every time you run this function.  However without this line the
    " buffer starts off as a default size and if you resize the buffer then it
    " keeps that custom size after repeated runs of this function.  But if you
    " close the output buffer then it returns to using the default size when
    " its recreated
    "execute 'resize' . line('$')

    " make the buffer non modifiable
    setlocal readonly
    setlocal nomodifiable
    silent execute bufwinnr(s:source_buf_nr) . 'wincmd w'
endfunction

cabbrev help tab help
cabbrev h tab help
