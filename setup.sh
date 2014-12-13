#!/bin/bash

DOT_FILES=( .zshrc .emacs.d .vimrc .tmux.conf)
cwd=$(cd $(dirname $0); pwd)

for file in ${DOT_FILES[@]}
do
  if [ -a $HOME/$file ]; then
    echo "exist : $file"
  else
    ln -s $cwd/$file $HOME/$file
    echo "create symbolic link: $file"
  fi
done

# neobundle
mkdir -p ~/.vim/bundle
git clone https://github.com/Shougo/neobundle.vim ~/.vim/bundle/neobundle.vim
