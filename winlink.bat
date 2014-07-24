@echo off
cd %~dp0

if exist winlink rmdir /s /q winlink
if not exist winlink mkdir winlink

rem ********************
rem emacs
rem ********************
mklink /d winlink\.emacs.d %~dp0\.emacs.d

rem ********************
rem zsh
rem ********************
mklink winlink\.zshrc %~dp0\.zshrc

rem ********************
rem xyzzy
rem ********************
mklink winlink\siteinit.el %~dp0\sitelisp\.siteinit.el

