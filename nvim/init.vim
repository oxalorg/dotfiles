" (neo)vimrc of @oxalorg
" test

" map the leader key to SPACE
let mapleader="\<SPACE>"

set clipboard=unnamedplus
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

" Specify a directory for plugins
" - For Neovim: ~/.local/share/nvim/plugged
" - Avoid using standard Vim directory names like 'plugin'
call plug#begin('~/.local/share/nvim/plugged')
Plug '/projects/vim-genox'

Plug 'tomasiser/vim-code-dark'
Plug 'andreasvc/vim-256noir'
Plug 'psliwka/vim-smoothie' " smoother CTRL+D/CTRL+U
Plug 'tpope/vim-sensible'
Plug 'Raimondi/delimitMate' " automatic pairing of parens, braces, quotes etc.
Plug 'tmhedberg/SimpylFold' | let g:SimpylFold_docstring_preview=1
Plug 'tpope/vim-unimpaired'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-surround'
Plug 'markonm/traces.vim'
Plug 'terryma/vim-multiple-cursors' " Uses C-n, C-x, C-p
Plug 'tpope/vim-commentary' " gcc to comment

Plug 'itchyny/lightline.vim'
let g:lightline = {
      \ 'colorscheme': 'nord',
      \ 'component_function': {
      \   'filename': 'LightlineFilename',
      \ },
  \ }
function! LightlineFilename()
    return expand('%:.')
endfunction

Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'
Plug 'rbgrouleff/bclose.vim'
Plug 'francoiscabrol/ranger.vim' | let g:ranger_map_keys = 0 | nnoremap - :Ranger<CR>
Plug 'simnalamburt/vim-mundo' "nnoremap <F5> :MundoToggle<CR>

Plug 'SirVer/ultisnips'
let g:UltiSnipsExpandTrigger="<C-s>"
let g:UltiSnipsJumpForwardTrigger="<c-b>"
let g:UltiSnipsJumpBackwardTrigger="<c-z>"
let g:UltiSnipsEditSplit="vertical"
let g:UltiSnipsSnippetDirectories = ['~/.config/nvim/UltiSnips', 'UltiSnips']

Plug 'honza/vim-snippets'
Plug 'airblade/vim-gitgutter'

Plug 'tpope/vim-fugitive'
autocmd User fugitive
  \ if fugitive#buffer().type() =~# '^\%(tree\|blob\)$' |
  \   nnoremap <buffer> .. :edit %:h<CR> |
  \ endif
nnoremap <leader>gg :Gstatus<CR>

Plug 'rbong/vim-flog'
Plug 'preservim/nerdtree', { 'on':  'NERDTreeToggle' }
Plug 'Xuyuanp/nerdtree-git-plugin'
Plug 'prettier/vim-prettier'
Plug 'arp242/gopher.vim'
autocmd BufWritePre *.go :call CocAction('runCommand', 'editor.action.organizeImport')

Plug 'tpope/vim-dadbod'
Plug 'kristijanhusak/vim-dadbod-ui'
Plug 'sheerun/vim-polyglot'

Plug 'dense-analysis/ale'
let g:ale_set_highlights = 0
let g:ale_echo_msg_format = '%linter%: %s'
let g:ale_linters = {
      \   'python': ['flake8',],
      \   'javascript': ['eslint'],
      \   'html': ['eslint'],
      \}
let g:ale_python_black_executable = '~/.virtualenvs/neovim/bin/black'
let g:ale_javascript_prettier_use_global = 1
let g:ale_fixers = {
      \   '*': ['remove_trailing_lines', 'trim_whitespace'],
      \   'python': ['isort', 'black'],
      \   'javascript': ['prettier',],
      \   'javascriptreact': ['prettier',],
      \   'htmldjango': ['html-beautify'],
      \}
let g:ale_fix_on_save = 1

Plug 'numirias/semshi', {'do': ':UpdateRemotePlugins' }
Plug 'neoclide/coc.nvim', {'branch': 'release' }
let g:coc_global_extensions = ['coc-css', 'coc-html', 'coc-json', 'coc-python', 'coc-tsserver', 'coc-yank', 'coc-emmet', 'coc-ultisnips']
Plug 'ludovicchabant/vim-gutentags'
Plug 'majutsushi/tagbar'
Plug 'Valloric/MatchTagAlways'

Plug 'Yggdroot/indentLine'
let g:indentLine_fileTypeExclude = ['markdown']

Plug 'easymotion/vim-easymotion'
let g:EasyMotion_smartcase = 1
let g:EasyMotion_do_mapping = 0 " Disable default mappings
nmap s <Plug>(easymotion-overwin-f)
vmap s <Plug>(easymotion-overwin-f)
nmap s <Plug>(easymotion-overwin-f2)

Plug 'unblevable/quick-scope'
Plug 'godlygeek/tabular' " Call :TableFormat
Plug 'elzr/vim-json'

Plug 'plasticboy/vim-markdown' " Call :Toc
let g:tex_conceal = ""
let g:vim_markdown_folding_disabled = 1
let g:vim_markdown_conceal = 0
let g:vim_markdown_math = 1
let g:vim_markdown_frontmatter = 1  " for YAML format
let g:vim_markdown_toml_frontmatter = 1  " for TOML format
let g:vim_markdown_json_frontmatter = 1  " for JSON format

Plug 'junegunn/goyo.vim'
autocmd! User GoyoEnter Limelight
autocmd! User GoyoLeave Limelight!

Plug 'junegunn/limelight.vim'

Plug 'iamcco/markdown-preview.nvim', { 'do': 'cd app & yarn install'  }
" let g:mkdp_markdown_css = expand('~/code/sakura/css/sakura.css')

Plug 'mattn/emmet-vim'
Plug 'terryma/vim-expand-region'
" Plug 'mhinz/vim-startify'
" Plug 'tpope/vim-endwise'
" Plug 'ryanpcmcquen/fix-vim-pasting'
" Plugin 'chamindra/marvim'
" Plug 'davidhalter/jedi-vim'
" Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins', 'on': [] }
" Plug 'deoplete-plugins/deoplete-jedi', { 'on': [] }
call plug#end()


let g:go_def_mapping_enabled = 0
nnoremap <leader><tab> :NERDTreeToggle<cr>


" augroup deoplete_py
"   autocmd!
"   autocmd FileType python call plug#load('deoplete.nvim', 'deoplete-jedi')
"                          \| call deoplete#enable()
"                          \| autocmd! deoplete_py
" augroup END

" Vim Default Settings
" --------------------

" shortcut to editing vimrc quickly
nnoremap <silent> <leader>nve :tabedit $MYVIMRC<CR>

" shortcut to reload vimrc without restarting vim
nnoremap <silent> <leader>nvr :so $MYVIMRC<CR>

" Use ; to go into command mode instead of :
" Saves one keystroke (no need to press shift)
noremap : ;
noremap ; :

" Use `qp` to go into escape/normal mode
inoremap qp <Esc>
noremap qp <Esc>

set encoding=utf-8
set ignorecase
set smartcase
set ruler
set number
set relativenumber
filetype on
filetype plugin indent on
set pastetoggle=<F3>
" Allows git gutter to update faster
set updatetime=100
set nofixendofline
set expandtab
set smarttab
set tabstop=4
set softtabstop=0
set shiftwidth=4
" Enable persistent undo so that undo history persists across vim sessions
set undofile
set undodir=~/.vim/undo
"split navigations
nnoremap <C-J> <C-W><C-J>
nnoremap <C-K> <C-W><C-K>
nnoremap <C-L> <C-W><C-L>
nnoremap <C-H> <C-W><C-H>

" Enable folding
set foldmethod=indent
set foldlevel=99
" Switch to last edited buffer by pressing backspace in normal mode
nnoremap <silent> <BS> :b#<CR>

set completeopt=menuone,noselect,noinsert
set shortmess+=c
inoremap <c-c> <ESC>

if &term =~# '256color' && ( &term =~# '^screen'  || &term =~# '^tmux' )
    let &t_8f = "\<Esc>[38;2;%lu;%lu;%lum"
    let &t_8b = "\<Esc>[48;2;%lu;%lu;%lum"
    set termguicolors
else
    set termguicolors
endif
set background=dark
set t_Co=256
augroup qs_colors
    autocmd!
    autocmd ColorScheme * highlight QuickScopePrimary guifg='#afff5f' gui=underline ctermfg=155 cterm=underline
    autocmd ColorScheme * highlight QuickScopeSecondary guifg='#5fffff' gui=underline ctermfg=81 cterm=underline
augroup END
colorscheme codedark
" hi clear Visual
" hi Visual guibg=#345456
set fillchars=diff:\ ,fold:\ ,

set diffopt+=vertical,iwhite
if &diff
  " diff mode
  set diffopt+=iwhite
endif

set guicursor=n-v-c:block,i-ci-ve:ver25,r-cr:hor20,o:hor50
      \,a:blinkwait700-blinkoff400-blinkon250-Cursor/lCursor
      \,sm:block-blinkwait175-blinkoff150-blinkon175

highl CursorTransparent ctermfg=16 ctermbg=253 guifg=#000000 guibg=#00FF00 gui=strikethrough blend=100

au BufNewFile,BufRead *.py
    \ set tabstop=4
    \ softtabstop=4
    \ shiftwidth=4
    \ textwidth=150
    \ expandtab
    \ autoindent
    \ fileformat=unix
    \ foldmethod=indent

" Flagging Unnecessary Whitespace
au BufRead, BufNewFile *.py,*.pyw,*.c,*.h match BadWhitespace /\s\+$/

autocmd FileType html,css,htmldjango,javascript,javascriptreact,json setlocal shiftwidth=2 softtabstop=2 tabstop=2 expandtab

let g:fzf_layout = { 'window': 'call FloatingFZF()' }

function! FloatingFZF()
  let buf = nvim_create_buf(v:false, v:true)
  call setbufvar(buf, '&signcolumn', 'no')

  let height = float2nr(20)
  let width = float2nr(120)
  let horizontal = float2nr((&columns - width) / 2)
  let vertical = 1

  let opts = {
        \ 'relative': 'editor',
        \ 'row': vertical,
        \ 'col': horizontal,
        \ 'width': width,
        \ 'height': height,
        \ 'style': 'minimal'
        \ }

  call nvim_open_win(buf, v:true, opts)
endfunction

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

command! -bang -nargs=* Ag
  \ call fzf#vim#grep(
  \   'ag --column --numbers --noheading --smart-case . '.shellescape(<q-args>), 1,
  \   fzf#vim#with_preview(), <bang>0)

nnoremap <C-p> :Files<Cr>
nnoremap <leader>o :Files<cr>
nnoremap <leader>f :Rg<cr>
nnoremap <leader>a :Ag<cr>
nnoremap <leader>bd :bwipeout<cr>
nnoremap <tab> :Buffers<cr>
nnoremap <leader>tt :Tags<cr>
nnoremap <leader>tb :TagbarOpenAutoClose<cr>
nnoremap <leader>p p<cr>


let s:uname = system("uname")
if s:uname =~ "Linux"
  let g:python3_host_prog = expand('~/.virtualenvs/neovim/bin/python3')
elseif s:uname =~ "Darwin"
  let g:python3_host_prog = '/Users/ox/.virtualenvs/neovim/bin/python3'
endif


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
let g:go_fmt_command = "goimports"
let g:go_fmt_autosave=1


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

" Use <c-space> to trigger completion.
inoremap <silent><expr> <c-space> coc#refresh()

" Use <cr> to confirm completion, `<C-g>u` means break undo chain at current
" position. Coc only does snippet and additional edit on confirm.
if exists('*complete_info')
  inoremap <expr> <cr> complete_info()["selected"] != "-1" ? "\<C-y>" : "\<C-g>u\<CR>"
else
  imap <expr> <cr> pumvisible() ? "\<C-y>" : "\<C-g>u\<CR>"
endif

" Use `[g` and `]g` to navigate diagnostics
nmap <silent> [g <Plug>(coc-diagnostic-prev)
nmap <silent> ]g <Plug>(coc-diagnostic-next)

" GoTo code navigation.
nmap <silent> gd <Plug>(coc-definition)
nmap <silent> gy <Plug>(coc-type-definition)
nmap <silent> gi <Plug>(coc-implementation)
nmap <silent> gr <Plug>(coc-references)

highlight DiffAdd        ctermbg=237 guibg=#373D29
highlight DiffChange     ctermbg=52 guibg=#345456
highlight DiffDelete     ctermfg=12 ctermbg=52 guifg=Blue guibg=#4f0a0a
highlight DiffText       ctermbg=52 guibg=#005500

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

xnoremap <leader>c <esc>:'<,'>:w !cat\|pbcopy<CR>
xnoremap <leader>gs <esc>:'<,'>:w !cat\|oxsnip<CR>
vnoremap J :m '>+1<CR>gv=gv
vnoremap K :m '<-2<CR>gv=gv
function! TodoComplete()
    let l:line = fnameescape(getline('.'))
    let l:now = fnameescape(strftime("%F%T"))
    let l:append_cmd = printf("echo %s %s >> /projects/todo/completed.txt", l:now, l:line)
    let l:output = system(append_cmd)
    execute ".d_"
    echom l:output
endfunction

function! TodoMove(to)
    execute 'normal dd'
    execute 'normal /# ' . a:to . "\<CR>"
    execute 'normal }p'
endfunction

function! TodoMoveNext()
    execute 'normal dd'
    execute 'normal /^# ' . "\<CR>"
    execute 'normal }p'
endfunction

function! TodoMovePrev()
    execute 'normal dd'
    execute 'normal ?^# ' . "\<CR>n"
    execute 'normal }p'
endfunction

nnoremap <leader>cc :call TodoComplete()<CR>
nnoremap <leader>cw :call TodoMove('Week')<CR>
nnoremap <leader>cm :call TodoMove('Month')<CR>
nnoremap <leader>cy :call TodoMove('Year')<CR>
nnoremap <leader>cb :call TodoMove('Backlog')<CR>
nnoremap <leader>ca :call TodoMove('Archived')<CR>
nnoremap <leader>c} :call TodoMoveNext()<CR>
nnoremap <leader>c{ :call TodoMovePrev()<CR>

function! s:editHtmlClass()
    try
        exe "normal! vato\<esc>vi>\<esc>/\\%Vclass=\<cr>2f\"i \<esc>l"
        startinsert
    catch
        exe "normal! a class=\"\"\<esc>"
        startinsert
    endtry
endfunction

command! EditHtmlClass call s:editHtmlClass()
nnoremap <leader>cc :EditHtmlClass<CR>
