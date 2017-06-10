@echo off
if "%1" == "" goto compile
if "%1" == "compile" goto compile
if "%1" == "clean" goto clean
if "%1" == "run" goto run
goto end

:clean
if exist bin rd /S /Q bin
del src\*.hi src\*.o src\*.exe
goto end

:compile
if not exist bin md bin
ghc src\chapter1.hs -o bin\chapter1.exe
ghc src\chapter2.hs -o bin\chapter2.exe
goto end

:run
call .\build.bat compile
.\bin\chapter1.exe
.\bin\chapter2.exe
goto end

:end
