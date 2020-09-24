" (neo)vimrc of @oxalorg
" map the leader key to SPACE
let mapleader="\<SPACE>"
set clipboard=unnamedplus
set shell=/bin/bash

let s:uname = system("uname")
if s:uname =~ "Linux"
  let g:python3_host_prog = expand('~/.virtualenvs/neovim/bin/python3')
elseif s:uname =~ "Darwin"
  let g:python3_host_prog = '/Users/ox/.virtualenvs/neovim/bin/python3'
endif

nnoremap <silent> <BS> :<C-u>exe v:count ? v:count . 'b' : 'b' . (bufloaded(0) ? '#' : 'n')<CR>
nnoremap <silent> <leader>nve :tabedit $MYVIMRC<CR>
nnoremap <silent> <leader>nvr :so $MYVIMRC<CR>
noremap : ;
noremap ; :
inoremap qp <Esc>
noremap qp <Esc>

cabbrev help tab help
cabbrev h tab help
cnoreabbrev w!! w !sudo tee > /dev/null %|  " write file with sudo

xnoremap <leader>c <esc>:'<,'>:w !cat\|pbcopy<CR>
xnoremap <leader>gs <esc>:'<,'>:w !cat\|oxsnip<CR>
command! -nargs=0 -range=% OxSnip <line1>,<line2>:w !oxsnip
vnoremap J :m '>+1<CR>gv=gv
vnoremap K :m '<-2<CR>gv=gv
" vnoremap <C-h> "hy:%s/<C-r>h//gc<left><left><left>"
nnoremap <leader>h :%s/<C-r>///gc<left><left><left>
vnoremap <c-s> y<ESC>/<c-r>"<CR>N
vnoremap R y<ESC>:%s/<c-r>"//gc<left><left><left>
nnoremap <c-s> <ESC>/<c-r>"<CR>
vnoremap <C-h> <esc>:'<,'>s/<C-r>"//g<left><left>
" now it is possible to paste many times over selected text
xnoremap <expr> p 'pgv"'.v:register.'y`>'
xnoremap <expr> P 'Pgv"'.v:register.'y`>'

" add and subtract should be easy now
nnoremap <c-x>a <c-a>
vnoremap <c-x>a g<c-a>
nnoremap <c-x>s <c-x>
vnoremap <c-x>s g<c-x>

" nnoremap yA "Ayy
" vnoremap yA '<,'>"Ayy
nnoremap Y y$

nnoremap <C-J> <C-W><C-J>
nnoremap <C-K> <C-W><C-K>
nnoremap <C-L> <C-W><C-L>
nnoremap <C-H> <C-W><C-H>

set hidden
set encoding=utf-8 ignorecase smartcase ruler number relativenumber
filetype plugin indent on
set pastetoggle=<F3>
set updatetime=200
set nofixendofline
set expandtab smarttab tabstop=4 softtabstop=4 shiftwidth=4
set undofile undodir=~/.vim/undo
set conceallevel=0

set foldmethod=indent foldlevel=99

set completeopt=menuone,noselect,noinsert
set shortmess+=c
inoremap <c-shift-c> <ESC>

function! GitBranch()
  return system("git rev-parse --abbrev-ref HEAD 2>/dev/null | tr -d '\n'")
endfunction

function! StatuslineGit()
  let l:branchname = GitBranch()
  return strlen(l:branchname) > 0?'  '.l:branchname.' ':''
endfunction

set textwidth=120
au BufNewFile,BufRead *.py
    \ set tabstop=4
    \ softtabstop=4
    \ shiftwidth=4
    \ textwidth=150
    \ expandtab
    \ autoindent
    \ fileformat=unix
    \ foldmethod=indent

au BufRead, BufNewFile *.py,*.pyw,*.c,*.h match BadWhitespace /\s\+$/

autocmd FileType html,css,htmldjango,javascript,scss,javascriptreact,json setlocal shiftwidth=2 softtabstop=2 tabstop=2 expandtab

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

function! RipgrepFzf(query, fullscreen)
  let command_fmt = 'rg --column --line-number --no-heading --color=always --smart-case -- %s || true'
  let initial_command = printf(command_fmt, shellescape(a:query))
  let reload_command = printf(command_fmt, '{q}')
  let spec = {'options': ['--phony', '--query', a:query, '--bind', 'change:reload:'.reload_command]}
  call fzf#vim#grep(initial_command, 1, fzf#vim#with_preview(spec), a:fullscreen)
endfunction

command! -nargs=* -bang RG call RipgrepFzf(<q-args>, <bang>0)

" Rg current word
nnoremap <silent> <Leader>rg :Rg <C-R><C-W><CR>
nnoremap <leader>o :Files!<cr>
nnoremap <leader>go :GF!?<cr>
nnoremap <leader>f :Rg!<cr>
nnoremap <leader>a :Ag<cr>
nnoremap <leader>bd :bwipeout<cr>
nnoremap <tab> :Buffers!<cr>
nnoremap <leader>tt :Tags!<cr>
nnoremap <leader>tb :TagbarOpenAutoClose<cr>
nnoremap <leader>p p<cr>

set grepprg=rg\ --vimgrep\ --smart-case

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

augroup Todo
    autocmd! Todo
    autocmd FileType text nnoremap <leader>cc :call TodoComplete()<CR>
    autocmd FileType text nnoremap <leader>cw :call TodoMove('Week')<CR>
    autocmd FileType text nnoremap <leader>cm :call TodoMove('Month')<CR>
    autocmd FileType text nnoremap <leader>cy :call TodoMove('Year')<CR>
    autocmd FileType text nnoremap <leader>cb :call TodoMove('Backlog')<CR>
    autocmd FileType text nnoremap <leader>ca :call TodoMove('Archived')<CR>
    autocmd FileType text nnoremap <leader>c} :call TodoMoveNext()<CR>
    autocmd FileType text nnoremap <leader>c{ :call TodoMovePrev()<CR>
augroup END

" Save current view settings on a per-window, per-buffer basis.
function! AutoSaveWinView()
    if !exists("w:SavedBufView")
        let w:SavedBufView = {}
    endif
    let w:SavedBufView[bufnr("%")] = winsaveview()
endfunction

" Restore current view settings.
function! AutoRestoreWinView()
    let buf = bufnr("%")
    if exists("w:SavedBufView") && has_key(w:SavedBufView, buf)
        let v = winsaveview()
        let atStartOfFile = v.lnum == 1 && v.col == 0
        if atStartOfFile && !&diff
            call winrestview(w:SavedBufView[buf])
        endif
        unlet w:SavedBufView[buf]
    endif
endfunction

" When switching buffers, preserve window view.
if v:version >= 700
    autocmd BufLeave * call AutoSaveWinView()
    autocmd BufEnter * call AutoRestoreWinView()
endif

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

nnoremap <leader>ii :PyImportAll<CR>
nnoremap <leader>il :PyImportLocal<CR>
nnoremap <leader>iv :PyImportVenv<CR>

nnoremap <leader>tv :PyTagsVenv<CR>
nnoremap <leader>tg :PyTagsGenerate<CR>

command! RupeeSymbol execute "normal! i" . "₹" . "\<esc>"

command! -range MacroDjChoices let @z = '^i(qpf=akbkb,qpf"lgUlA),qpj' | execute "'<,'>normal @z"
command! -range MacroDjChoices2 let @z = '^gUwyiwea = ""qphphguiwj' | execute "'<,'>normal @z"

function! RecMacroExe(cmds)
    let a = @a
    let @a = a:cmds . "@a"
    try
        normal @a
    finally
        let @a = a
    endtry
endfunction

nmap <f6> :call RecMacroExe("macrohere")<cr>

"nnoremap <leader>/ :BLines<cr>

" install vim-plug if not already there
if empty(glob('~/.local/share/nvim/site/autoload/plug.vim'))
  silent !curl -fLo ~/.local/share/nvim/site/autoload/plug.vim --create-dirs
    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  augroup install_vim_plug
      autocmd!
      autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
  augroup END
endif

call plug#begin('~/.local/share/nvim/plugged')
Plug '~/projects/vim-python-magic'
let g:genox_root_dir = '~/projects/oxal.org'
let g:genox_blog_dir = '~/projects/oxal.org/src/blog'
Plug '/projects/vim-genox'
Plug 'sheerun/vim-polyglot'
Plug 'psliwka/vim-smoothie' " smoother CTRL+D/CTRL+U
Plug 'kana/vim-textobj-user'
Plug 'beloglazov/vim-textobj-quotes'
Plug 'axelf4/vim-strip-trailing-whitespace'
Plug 'guns/vim-clojure-highlight'
Plug 'guns/vim-clojure-static'
Plug 'luochen1990/rainbow'
Plug 'tpope/vim-eunuch'
Plug 'tpope/vim-unimpaired'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-commentary' " gcc to comment
Plug 'tpope/vim-fugitive'
nnoremap <leader>gg :Gstatus<CR>
Plug 'tpope/vim-fireplace'
Plug 'airblade/vim-gitgutter'
Plug 'ludovicchabant/vim-gutentags'
Plug 'majutsushi/tagbar'
Plug 'justinmk/vim-gtfo'
" Plug 'danilo-augusto/vim-afterglow'
Plug 'tomasiser/vim-code-dark'
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'
Plug 'rbgrouleff/bclose.vim'
Plug 'francoiscabrol/ranger.vim' | let g:ranger_map_keys = 0 | nnoremap - :Ranger<CR>
Plug 'Yggdroot/indentLine'
let g:indentLine_fileTypeExclude = ['markdown']
Plug 'mattn/emmet-vim'
let g:user_emmet_settings = {
    \ 'javascriptreact': {'extends': 'html', 'filters': 'html'}
\ }

Plug 'SirVer/ultisnips'
let g:UltiSnipsExpandTrigger="<C-s-0>"
let g:UltiSnipsJumpForwardTrigger="<c-b>"
let g:UltiSnipsJumpBackwardTrigger="<c-z>"
let g:UltiSnipsEditSplit="vertical"
let g:UltiSnipsSnippetDirectories = ['~/.config/nvim/UltiSnips', 'UltiSnips']

Plug 'honza/vim-snippets'

Plug 'easymotion/vim-easymotion'
let g:EasyMotion_smartcase = 1
let g:EasyMotion_do_mapping = 0 " Disable default mappings
" vmap s <Plug>(easymotion-overwin-f)
" nmap s <Plug>(easymotion-overwin-f2)
nmap s <Plug>(easymotion-bd-w)
nmap S <Plug>(easymotion-bd-e)
nmap <leader>s <Plug>(easymotion-jumptoanywhere)
nmap <leader>w <Plug>(easymotion-bd-w)
vmap <leader>w <Plug>(easymotion-bd-w)
nmap <leader>j <Plug>(easymotion-j)
nmap <leader>k <Plug>(easymotion-k)

Plug 'dense-analysis/ale'
let g:ale_set_highlights = 0
let g:ale_echo_msg_format = '%linter%: %s'
let g:ale_linters = {
      \   'python': ['flake8',],
      \   'javascript': ['eslint'],
      \   'html': ['eslint'],
      \   'go': ['go build', 'gometalinter'],
      \   'clojure': ['clj-kondo'],
      \}
let g:ale_python_black_executable = '~/.virtualenvs/neovim/bin/black'
let g:ale_javascript_prettier_use_global = 1
let g:ale_fixers = {
      \   '*': ['remove_trailing_lines', 'trim_whitespace'],
      \   'python': ['isort', 'black'],
      \   'javascriptreact': ['prettier',],
      \   'htmldjango': ['html-beautify'],
      \   'go': ['goimports'],
      \   'javascript': ['prettier',],
      \   'html': ['html-beautify'],
      \}
let g:ale_fix_on_save = 1

Plug 'bhurlow/vim-parinfer'
"Plug 'guns/vim-sexp'
"Plug 'tpope/vim-sexp-mappings-for-regular-people'
Plug 'neoclide/coc.nvim', {'branch': 'release' }
let g:coc_global_extensions = ['coc-css', 'coc-json', 'coc-python', 'coc-tsserver', 'coc-yank', 'coc-emmet', 'coc-ultisnips', 'coc-tailwindcss', 'coc-conjure']
call plug#end()

colorscheme codedark
set termguicolors t_Co=256
hi Visual guibg=#345456
hi Normal guibg=#000000
hi SignColumn guibg=#000000
hi LineNr guibg=#000000
hi EndOfBuffer guibg=#000000
hi IncSearch guibg=yellow ctermbg=green term=underline
hi DiffAdd        ctermbg=237 guibg=#373D29
hi DiffChange     ctermbg=52 guibg=#345456
hi DiffDelete     ctermfg=12 ctermbg=52 guifg=Blue guibg=#4f0a0a
hi DiffText       ctermbg=52 guibg=#005500
hi GitGutterAdd    guifg=#009900 guibg=#000000 ctermfg=2 ctermbg=234
hi GitGutterChange guifg=#bbbb00 guibg=#000000 ctermfg=3 ctermbg=234
hi GitGutterDelete guifg=#ff2222 guibg=#000000 ctermfg=1 ctermbg=234

set fillchars=diff:\ ,fold:\ ,
set diffopt+=vertical,iwhite
if &diff
  set diffopt+=iwhite
endif


" COC
if has("patch-8.1.1564")
  " Recently vim can merge signcolumn and number column into one
  set signcolumn=number
else
  set signcolumn=yes
endif

" Use tab for trigger completion with characters ahead and navigate.
" NOTE: Use command ':verbose imap <tab>' to make sure tab is not mapped by
" other plugin before putting this into your config.
inoremap <silent><expr> <TAB>
      \ pumvisible() ? "\<C-n>" :
      \ <SID>check_back_space() ? "\<TAB>" :
      \ coc#refresh()
inoremap <expr><S-TAB> pumvisible() ? "\<C-p>" : "\<C-h>"

function! s:check_back_space() abort
  let col = col('.') - 1
  return !col || getline('.')[col - 1]  =~# '\s'
endfunction

" Use <c-space> to trigger completion.
if has('nvim')
  inoremap <silent><expr> <c-space> coc#refresh()
else
  inoremap <silent><expr> <c-@> coc#refresh()
endif

" Use <cr> to confirm completion, `<C-g>u` means break undo chain at current
" position. Coc only does snippet and additional edit on confirm.
" <cr> could be remapped by other vim plugin, try `:verbose imap <CR>`.
if exists('*complete_info')
  inoremap <expr> <cr> complete_info()["selected"] != "-1" ? "\<C-y>" : "\<C-g>u\<CR>"
else
  inoremap <expr> <cr> pumvisible() ? "\<C-y>" : "\<C-g>u\<CR>"
endif

" Use `[g` and `]g` to navigate diagnostics
" Use `:CocDiagnostics` to get all diagnostics of current buffer in location list.
nmap <silent> [g <Plug>(coc-diagnostic-prev)
nmap <silent> ]g <Plug>(coc-diagnostic-next)

" GoTo code navigation.
nmap <silent> gd <Plug>(coc-definition)
nmap <silent> gy <Plug>(coc-type-definition)
nmap <silent> gi <Plug>(coc-implementation)
nmap <silent> gr <Plug>(coc-references)

" Use K to show documentation in preview window.
nnoremap <silent> K :call <SID>show_documentation()<CR>

function! s:show_documentation()
  if (index(['vim','help'], &filetype) >= 0)
    execute 'h '.expand('<cword>')
  else
    call CocAction('doHover')
  endif
endfunction

" Highlight the symbol and its references when holding the cursor.
autocmd CursorHold * silent call CocActionAsync('highlight')

" Symbol renaming.
nmap <leader>rn <Plug>(coc-rename)

" Formatting selected code.
xmap <leader>gf  <Plug>(coc-format-selected)
nmap <leader>gf  <Plug>(coc-format-selected)

augroup mygroup
  autocmd!
  " Setup formatexpr specified filetype(s).
  autocmd FileType typescript,json setl formatexpr=CocAction('formatSelected')
  " Update signature help on jump placeholder.
  autocmd User CocJumpPlaceholder call CocActionAsync('showSignatureHelp')
augroup end

" Applying codeAction to the selected region.
" Example: `<leader>aap` for current paragraph
xmap <leader>a  <Plug>(coc-codeaction-selected)
nmap <leader>a  <Plug>(coc-codeaction-selected)

" Remap keys for applying codeAction to the current buffer.
nmap <leader>ac  <Plug>(coc-codeaction)
" Apply AutoFix to problem on the current line.
nmap <leader>qf  <Plug>(coc-fix-current)

" Map function and class text objects
" NOTE: Requires 'textDocument.documentSymbol' support from the language server.
xmap if <Plug>(coc-funcobj-i)
omap if <Plug>(coc-funcobj-i)
xmap af <Plug>(coc-funcobj-a)
omap af <Plug>(coc-funcobj-a)
xmap ic <Plug>(coc-classobj-i)
omap ic <Plug>(coc-classobj-i)
xmap ac <Plug>(coc-classobj-a)
omap ac <Plug>(coc-classobj-a)

" Use CTRL-S for selections ranges.
" Requires 'textDocument/selectionRange' support of LS, ex: coc-tsserver
nmap <silent> <C-s> <Plug>(coc-range-select)
xmap <silent> <C-s> <Plug>(coc-range-select)

" Add `:Format` command to format current buffer.
command! -nargs=0 Format :call CocAction('format')

" Add `:Fold` command to fold current buffer.
command! -nargs=? Fold :call     CocAction('fold', <f-args>)

" Add `:OR` command for organize imports of the current buffer.
command! -nargs=0 OR   :call     CocAction('runCommand', 'editor.action.organizeImport')

" Add (Neo)Vim's native statusline support.
" NOTE: Please see `:h coc-status` for integrations with external plugins that
" provide custom statusline: lightline.vim, vim-airline.
set statusline^=%{coc#status()}%{get(b:,'coc_current_function','')}

set conceallevel=0
