@echo off
for %%f in (*.asm) do call :cmpfile %%f
exit
:cmpfile
jwasm -q -bin %1
jfc %~n1.BIN %~n1.EXP
