@echo off
cd %~dp0

if exist winlink rmdir /s /q winlink
if not exist winlink mkdir winlink

rem ********************
rem emacs
rem ********************
mklink /d %HOME%\.emacs.d %~dp0\.emacs.d

rem ********************
rem zsh
rem ********************
mklink %HOME%\.zshrc %~dp0\.zshrc

rem ********************
rem xyzzy
rem ********************
mklink winlink\siteinit.l %~dp0\site-lisp\siteinit.l

