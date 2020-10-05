@echo off

pushd "%~dp0"
"%ProgramFiles%\swipl\bin\swipl-win.exe" -s joust.pl
popd
