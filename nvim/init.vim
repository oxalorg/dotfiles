"dein Scripts-----------------------------
if &compatible
  set nocompatible               " Be iMproved
endif

let s:uname = system("uname")
if s:uname == "Darwin\n"
    set runtimepath+=/Users/ox/.random/repos/github.com/Shougo/dein.vim
    call dein#begin('/Users/ox/.random')
    let g:python3_host_prog = '/usr/local/bin/python3'
else
    set runtimepath+=/home/ox/.random/repos/github.com/Shougo/dein.vim
    call dein#begin('/home/ox/.random')
    let g:python3_host_prog = '/usr/bin/python3'
endif

"Macro Marvim configs should be set before sourcing the plugin
let marvim_store = '/dotfiles/nvim/macros'
" let marvim_find_key = '<Space>' 
" let marvim_store_key = 'ms' 
" let marvim_register = 'c' 

" Let dein manage dein
" Required:
call dein#add('Shougo/dein.vim')

"------------------------------------------
" Add or remove your plugins here:
"------------------------------------------
call dein#add('Shougo/neosnippet.vim')
call dein#add('Shougo/neosnippet-snippets')
call dein#add('Shougo/deoplete.nvim')
" call dein#add('zchee/deoplete-jedi')
call dein#add('junegunn/fzf', { 'build': './install', 'merged': 0 })
call dein#add('junegunn/fzf.vim')

" Tpope in my vimrc
call dein#add('tpope/vim-commentary')
call dein#add('tpope/vim-surround')
" Pretty theme
call dein#add('junegunn/seoul256.vim')
" Writing
" call dein#add('reedes/vim-pencil')
call dein#add('junegunn/limelight.vim')
call dein#add('junegunn/goyo.vim')
" Pasting
call dein#add('ConradIrwin/vim-bracketed-paste')
" Git
call dein#add('airblade/vim-gitgutter')
" Macros
call dein#add('vim-scripts/marvim')

"------------------------------------------
" You can specify revision/branch/tag.
call dein#add('Shougo/vimshell', { 'rev': '3787e5' })

" Required:
call dein#end()

" Required:
filetype plugin indent on
syntax enable

" If you want to install not installed plugins on startup.
if dein#check_install()
  call dein#install()
endif

"End dein Scripts-------------------------

" Enable plugins
let g:deoplete#enable_at_startup=1
autocmd FileType markdown let g:deoplete#enable_at_startup=0
set updatetime=250 "for vim gitgutter

" NeoSnippets
" Plugin key-mappings.
imap <C-k>     <Plug>(neosnippet_expand_or_jump)
smap <C-k>     <Plug>(neosnippet_expand_or_jump)
xmap <C-k>     <Plug>(neosnippet_expand_target)

" SuperTab like snippets behavior.
imap <expr><TAB>
 \ pumvisible() ? "\<C-n>" :
 \ neosnippet#expandable_or_jumpable() ?
 \    "\<Plug>(neosnippet_expand_or_jump)" : "\<TAB>"
smap <expr><TAB> neosnippet#expandable_or_jumpable() ?
 \ "\<Plug>(neosnippet_expand_or_jump)" : "\<TAB>"

imap <expr><silent><CR> pumvisible() ? deoplete#mappings#close_popup() .
      \ "\<Plug>(neosnippet_jump_or_expand)" : "\<CR>"
smap <silent><CR> <Plug>(neosnippet_jump_or_expand)

" Limelight on/off based on Goyo
autocmd! User GoyoEnter Limelight
autocmd! User GoyoLeave Limelight!

" TODO: Fix this
source ~/.vim/vimrc
