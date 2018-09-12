@echo off
rem -- Run Vim --

setlocal
set VIM_EXE_DIR=C:\Neovim\bin


if exist "%VIM_EXE_DIR%\nvim-qt.exe" goto havevim
echo "%VIM_EXE_DIR%\nvim-qt.exe" not found
goto eof

:havevim
rem collect the arguments in VIMARGS for Win95
set VIMARGS= -- --
:loopstart
if .%1==. goto loopend
set VIMARGS=%VIMARGS% %1
shift
goto loopstart
:loopend

echo "%VIM_EXE_DIR%\nvim-qt.exe"  %VIMARGS%
"%VIM_EXE_DIR%\nvim-qt.exe"  %VIMARGS%
goto eof


:eof
set VIMARGS=
