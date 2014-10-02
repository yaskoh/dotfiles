#!/bin/bash

DOT_FILES=( .emacs.d .zshrc )
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
