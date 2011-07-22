@echo off
rem tools: jwasm, wlink, jfc, ms link (COFF)
for %%f in (*.asm) do call :cmpbin %%f
for %%f in (*.asn) do call :cmpmz  %%f
for %%f in (*.aso) do call :cmperr %%f
for %%f in (*.asz) do call :lnkomf %%f
call :safeseh
call :coff
call :coffdbg
call :ifdef
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

:lnkomf
jwasm -q -omf %1
if errorlevel 1 goto end
wlink op q,nofar format dos file %~n1.OBJ
jfc %~n1.EXE %~n1.EXP
goto end

:safeseh
jwasm -q -coff -safeseh SAFESEH.ASC
if errorlevel 1 goto end
link /nologo /SAFESEH SAFESEH.OBJ SAFESEH.LIB
jfc -pe SAFESEH.EXE SAFESEH.EXP
goto end

:coff
rem weak externals
jwasm -q -coff EXTERN4.ASC
if errorlevel 1 goto end
link /nologo /subsystem:console EXTERN4.OBJ
jfc -pe EXTERN4.EXE EXTERN4.EXP

jwasm -q -coff SEH64.ASC
if errorlevel 1 goto end
jfc -co SEH64.OBJ SEH64.EXP

jwasm -q -win64 MOV643.ASC
if errorlevel 1 goto end
jfc -co MOV643.OBJ MOV643.EXP

jwasm -q -win64 PROC641.ASC
if errorlevel 1 goto end
jfc -co PROC641.OBJ PROC641.EXP
goto end

:coffdbg
jwasm -q -Zi -coff DBGCV.ASC
if errorlevel 1 goto end
jfc -co DBGCV.OBJ DBGCV.EXP
goto end

:ifdef
..\owdos16r\jwasmr -q -zlc -zld ISDEF.ASC
if errorlevel 1 goto end
jfc ISDEF.OBJ ISDEF.EXP
goto end

:end
