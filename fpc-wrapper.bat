@echo off
REM A wrapper for fpc to automatically add enough flags

REM Check if fpc is available
where fpc >nul 2>nul
if errorlevel 1 (
	echo Could not find fpc on your system.
	exit /b 1
)

REM Get the directory of this batch file
SET "HERE=%~dp0"

REM Remove trailing backslash
SET "HERE=%HERE:~0,-1%"

REM Set base flags and include flags
SET BASE_FLAGS=-MObjFPC -Scghi -l -vew

REM Set platform default
SET PLATFORM=x86_32-windows

REM Check the architecture
if "%PROCESSOR_ARCHITECTURE%" == "AMD64" (
	SET PLATFORM=x86_64-windows
    SET BASE_FLAGS=%BASE_FLAGS% -Twin64 -Px86_64
)

SET INCLUDE_FLAGS=-Fl"%HERE%\libs\%PLATFORM%" -Fu"%HERE%\source" -Fu"%HERE%\headers"

REM Run the fpc command
fpc %BASE_FLAGS% %INCLUDE_FLAGS% %*

REM Copy raylib.dll to source directory
copy "%HERE%\libs\%PLATFORM%\raylib.dll%" .

REM Clean up object files
del *.o

echo Compilation successful
