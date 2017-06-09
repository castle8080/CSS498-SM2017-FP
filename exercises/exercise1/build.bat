@echo off

if "%1" == "" goto compile
if "%1" == "compile" goto compile
if "%1" == "clean" goto clean
if "%1" == "run" goto run
goto end

:clean
del *.hi *.o *.exe
goto end

:compile
ghc exercise1.hs -o exercise1.exe
goto end

:run
call .\build.bat compile
.\exercise1.exe
goto end

:end
