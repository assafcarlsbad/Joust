@echo off

pushd "%~dp0"
"%ProgramFiles%\swipl\bin\swipl-win.exe" -s src\joust.pl
popd
