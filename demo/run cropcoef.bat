@echo off
setlocal

rem Try current directory
for %%F in ("%~dp0CropCoef*.exe") do (
    if exist "%%~fF" (
        "%%~fF"
        goto :done
    )
)

rem Try parent\release
for %%F in ("%~dp0..\release\CropCoef*.exe") do (
    if exist "%%~fF" (
        "%%~fF"
        goto :done
    )
)

rem Try parent directory
for %%F in ("%~dp0..\CropCoef*.exe") do (
    if exist "%%~fF" (
        "%%~fF"
        goto :done
    )
)

echo No CropCoef executable was found.
echo Searched:
echo   %~dp0CropCoef*.exe
echo   %~dp0..\release\CropCoef*.exe
echo   %~dp0..\CropCoef*.exe

:done
pause