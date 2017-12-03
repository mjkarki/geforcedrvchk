@echo off

set RACO="C:\Program Files\Racket\raco.exe"

if x%1==x goto HELP
if x%1==xall goto ALL
if x%1==xclean goto CLEAN
if x%1==xhelp goto HELP

:ALL
%RACO% exe --gui GeForceDrvChk.rkt
%RACO% distribute GeForceDrvChk GeForceDrvChk.exe

copy LICENSE GeForceDrvChk
copy README.md GeForceDrvChk

%RACO% pkg create GeForceDrvChk

del GeForceDrvChk.zip.CHECKSUM
rmdir /s /q GeForceDrvChk

echo.

goto END

:HELP
echo.
echo Usage:
echo.
echo build.bat all    - Builds binaries and packages them into a zip file.
echo build.bat clean  - Cleans all built binaries.
echo build.bat help   - This help.
echo.
echo Currently this script is configured to use raco from the following path:
echo %RACO%
echo.
goto END

:CLEAN
echo Cleaning...
del /q GeForceDrvChk.zip
del /q GeForceDrvChk.exe

:END
echo Done.
echo.
