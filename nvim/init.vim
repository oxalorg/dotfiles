"dein Scripts-----------------------------
if &compatible
  set nocompatible               " Be iMproved
endif

" Required:
set runtimepath^=/home/ox/.random/repos/github.com/Shougo/dein.vim

" Required:
call dein#begin(expand('/home/ox/.random'))

" Let dein manage dein
" Required:
call dein#add('Shougo/dein.vim')

" Add or remove your plugins here:
"call dein#add('Shougo/neosnippet.vim')
"call dein#add('Shougo/neosnippet-snippets')
call dein#add('Shougo/deoplete.nvim')
call dein#add('zchee/deoplete-jedi')
call dein#add('mtth/scratch.vim')
call dein#add('scrooloose/nerdtree')
call dein#add('jmcantrell/vim-virtualenv')

" Tpope in my vimrc
call dein#add('tpope/vim-commentary')
call dein#add('tpope/vim-surround')

" Pretty
call dein#add('junegunn/seoul256.vim')

" You can specify revision/branch/tag.
"call dein#add('Shougo/vimshell', { 'rev': '3787e5' })

" Required:
call dein#end()

" Required:
filetype plugin indent on

" If you want to install not installed plugins on startup.
if dein#check_install()
  call dein#install()
endif

"End dein Scripts-------------------------

" Enable plugins
call deoplete#enable()
colo seoul256

" Plugin customization
map <C-\> :NERDTreeToggle<CR>


" Python
"let g:python_host_prog = '/usr/bin/python'
let g:python3_host_prog = '/usr/bin/python3'
autocmd FileType python nnoremap <leader>y :0,$!yapf<Cr>

" Other config
set relativenumber number
autocmd CompleteDone * pclose

set tabstop=4		" number of visual spaces interpreted for each tab
set softtabstop=4	" number of spaces inserted when using tab
set expandtab		" expand tabs to spaces
set shiftwidth=4        " When indenting with > / <
set smartindent
