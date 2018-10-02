#!/bin/bash

# variables
DOT_FILES=( .zshrc .emacs.d .vimrc .tmux.conf .tmux-darwin.conf .bashrc)

# make symbolic link
cwd=$(cd $(dirname $0); pwd)
for file in ${DOT_FILES[@]}
do
  if [ -h ${HOME}/${file} ]; then
    rm -f ${HOME}/${file}
    ln -s ${cwd}/${file} ${HOME}/${file}
    echo "replace symbolic link: ${file}"
  elif [ -e ${HOME}/${file} ]; then
    echo "exist : ${file}"
  else
    ln -s ${cwd}/${file} ${HOME}/${file}
    echo "create symbolic link: ${file}"
  fi
done

# git setting
echo " "
echo "present git config :"
git config --global -l
echo " "
echo "type git user name : "
read n
if [ "${n}" != "" ]; then git config --global user.name "${n}"; fi
echo "type git user email : "
read e
if [ "${e}" != "" ]; then git config --global user.email "${e}"; fi
echo " "
git config core.filemode false  # ignore mode difference
echo "present git config :"
git config --global -l
echo " "

# pyenv
git clone https://github.com/yyuu/pyenv.git ~/.pyenv

# rbenv
git clone https://github.com/sstephenson/rbenv.git ~/.rbenv
git clone https://github.com/sstephenson/ruby-build.git ~/.rbenv/plugins/ruby-build
