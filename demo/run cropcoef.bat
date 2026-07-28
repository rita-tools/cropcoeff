@echo off

if exist "%~dp0cropcoef.exe" (
    "%~dp0cropcoef.exe"
) else if exist "%~dp0..\release\cropcoef.exe" (
    "%~dp0..\release\cropcoef.exe"
) else if exist "%~dp0..\cropcoef.exe" (
    "%~dp0..\cropcoef.exe"
) else (
    echo cropcoef.exe was not found.
    echo Searched:
    echo   %~dp0cropcoef.exe
    echo   %~dp0..\release\cropcoef.exe
    echo   %~dp0..\cropcoef.exe
)

pause