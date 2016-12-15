@echo off
cd %~dp0

rem ********************
rem emacs
rem ********************
mklink /d %HOME%\.emacs.d %~dp0\.emacs.d

rem ********************
rem zsh
rem ********************
mklink %HOME%\.zshrc %~dp0\.zshrc

rem ********************
rem bash
rem ********************
mklink %HOME%\.bashrc %~dp0\.bashrc

rem ********************
rem tmux
rem ********************
mklink %HOME%\.tmux.conf %~dp0\.tmux.conf

rem ********************
rem vim
rem ********************
mklink %HOME%\.vimrc %~dp0\.vimrc

rem ********************
rem xyzzy
rem ********************
rem if exist winlink rmdir /s /q winlink
rem if not exist winlink mkdir winlink
rem mklink winlink\siteinit.l %~dp0\site-lisp\siteinit.l
