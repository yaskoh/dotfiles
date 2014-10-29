"--------------------------------------
" Settings
"--------------------------------------

syntax on

set nocompatible
set cursorline
set backspace=start,eol,indent
set mouse=
set incsearch
set number
set whichwrap=b,s,[,],<,>,~
set nohlsearch
set laststatus=2
set statusline=%F%r%h%=
set ignorecase
set wildmenu wildmode=list:full

highlight Normal ctermfg=black
highlight StatusLine ctermfg=grey ctermbg=black
highlight CursorLine ctermfg=darkgray ctermbg=none cterm =none
highlight matchParen ctermfg=none ctermbg=darkgray


"--------------------------------------
" NeoBundle
"--------------------------------------

if has('vim_starting')
    set runtimepath+=~/.vim/bundle/neobundle.vim/
endif

call neobundle#begin(expand('~/.vim/bundle/'))

" NeoBundle
NeoBundleFetch 'Shougo/neobundle.vim'

" plug-in
NeoBundle 'scrooloose/nerdtree'
NeoBundle 'grep.vim'
NeoBundle 'scrooloose/syntastic'
NeoBundle 'Shougo/unite.vim'
NeoBundle 'ujihisa/unite-colorscheme'

" color
NeoBundle 'altercation/vim-colors-solarized'
NeoBundle 'croaker/mustang-vim'
NeoBundle 'tomasr/molokai'

call neobundle#end()

filetype plugin indent on

NeoBundleCheck
