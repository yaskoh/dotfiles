#!/bin/bash

# variables
DOT_FILES=( .zshrc .emacs.d .vimrc .tmux.conf .tmux-darwin.conf .bashrc)

# set os settings
if [ "$(uname -s)" == 'Darwin' ]; then
  os_name='Mac'
elif [ "$(expr substr $(uname -s) 1 5)" == 'Linux' ]; then
  os_name='Linux'
elif [ "$(expr substr $(uname -s) 1 5)" == 'MINGW' ]; then
  os_name='Windows'
elif [ "$(expr substr $(uname -s) 1 4)" == 'MSYS' ]; then
  os_name='Windows'
else
  os_name='Unknown'
  exit 1
fi

# set directory path of setup shell
cwd=$(cd $(dirname $0); pwd)

# set today's date
today=$(date '+%Y%m%d')

# make symbolic link
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

# make links for vscode
if [ "${os_name}" == 'Mac' ]; then
  vscode_setpath="${HOME}/Library/Application Support/Code/User";
elif [ "${os_name}" == 'Linux' ]; then
  vscode_setpath="${HOME}/.config/Code/User";
else
  echo "No target"
  exit 1
fi

for file in $(ls vscode)
do
  echo file: ${file}
  if [ -h "${vscode_setpath}/${file}" ]; then
    rm -f "${vscode_setpath}/${file}"
    ln -s ${cwd}/vscode/${file} "${vscode_setpath}/${file}"
    echo "replace symbolic link: ${file}"
  elif [ -e "${vscode_setpath}/${file}" ]; then
    mv "${vscode_setpath}/${file}" "${vscode_setpath}/${file}_${today}"
    ln -s "${cwd}/vscode/${file}" "${vscode_setpath}/${file}"
    echo "create symbolic link: ${file}"
  else
    echo "VSCode is not found"
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
