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
if %errorlevel% neq 0 goto :error
ghc src\chapter2.hs -o bin\chapter2.exe
if %errorlevel% neq 0 goto :error
ghc src\chapter3.hs -o bin\chapter3.exe
if %errorlevel% neq 0 goto :error
ghc src\chapter4.hs -o bin\chapter4.exe
if %errorlevel% neq 0 goto :error
ghc src\chapter5.hs -o bin\chapter5.exe
if %errorlevel% neq 0 goto :error
goto end

:run
call .\build.bat compile
if %errorlevel% neq 0 goto :error
.\bin\chapter1.exe
.\bin\chapter2.exe
.\bin\chapter3.exe
.\bin\chapter4.exe
.\bin\chapter5.exe
goto end

:error
goto end

:end
