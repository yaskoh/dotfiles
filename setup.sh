#! /bin/bash

DOT_FILES=( .emacs.d .zshrc )

for file in ${DOT_FILES[@]}
do
  if [ -a $HOME/$file ]; then
    echo "exist : $file"
  else
    ln -s $HOME/dotfiles/$file $HOME/$file
    echo "create symbolic link: $file"
  fi
done
