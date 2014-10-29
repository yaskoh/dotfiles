"--------------------------------------
" Settings
"--------------------------------------

syntax on
set cursorline

highlight Normal ctermbg=black ctermfg=grey
highlight StatusLine term=none cterm=none ctermfg=black ctermbg=grey
highlight CursorLine term=none cterm=none ctermfg=none ctermbg=darkgray

set backspace=start,eol,indent

set mouse=a
set incsearch

"--------------------------------------
" NeoBundle
"--------------------------------------

if has('vim_starting')
    set nocompatible
"    filetype off
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
