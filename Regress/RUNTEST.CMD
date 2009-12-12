@echo off
rem tools: jwasm, wlink, jfc
for %%f in (*.asm) do call :cmpbin %%f
for %%f in (*.asn) do call :cmpmz  %%f
for %%f in (*.aso) do call :cmperr %%f
for %%f in (*.asz) do call :cmpexe %%f
exit
:cmpbin
jwasm -q -bin %1
jfc %~n1.BIN %~n1.EXP
goto end
:cmpmz
jwasm -q -mz %1
jfc %~n1.EXE %~n1.EXP
goto end
:cmperr
jwasm -q -omf %1 >NUL
jfc %~n1.ERR %~n1.EXP
goto end
:cmpexe
jwasm -q -omf %1
if errorlevel 1 goto end
wlink op q format dos file %~n1.OBJ
jfc %~n1.EXE %~n1.EXP
:end
