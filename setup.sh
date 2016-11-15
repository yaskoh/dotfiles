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

# pyenv
git clone https://github.com/yyuu/pyenv.git ~/.pyenv

# rbenv
git clone https://github.com/sstephenson/rbenv.git ~/.rbenv
git clone https://github.com/sstephenson/ruby-build.git ~/.rbenv/plugins/ruby-build
