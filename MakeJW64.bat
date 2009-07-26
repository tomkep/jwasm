@echo off
rem create the experimental JWasm v2.0 (64bit-aware)

rem use Open Watcom + Makefile
wmake.exe -h amd64=1 outd=JW64R
wmake.exe -h amd64=1 outd=JW64D debug=1
wmake.exe -h -f OWLinux.mak amd64=1 outd=JW64L

rem use MS VC + msvc.mak
rem nmake.exe -f msvc.mak amd64=1 outd=JW64R
rem nmake.exe -f msvc.mak amd64=1 outd=JW64D debug=1

rem use MS VC without Makefile.
rem Make sure sub-directory JW64R and/or JW64D exists!
rem uses VC++ 2003 Toolkit, VC++ 2005 EE not recommended!
rem set MSCDIR=\msvc8
rem %MSCDIR%\bin\cl.exe -DAMD64_SUPPORT=1 -D__NT__ -IH -I %MSCDIR%\include -FoJW64R\ -FeJW64R\JWasm.exe -Oty2 -Gs *.c -link -libpath:%MSCDIR%\lib -map:JW64R\JWasm.map
rem %MSCDIR%\bin\cl.exe -DAMD64_SUPPORT=1 -D__NT__ -DDEBUG_OUT -IH -I %MSCDIR%\include -FoJW64D\ -FeJW64D\JWasm.exe -Od -Zi *.c -link -libpath:%MSCDIR%\lib -map:JW64D\JWasm.map
