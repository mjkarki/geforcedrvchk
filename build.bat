@echo off

set RACO="C:\Program Files\Racket\raco.exe"

if x%1==xclean goto CLEAN

%RACO% exe --gui GeForceDrvChk.rkt
%RACO% distribute GeForceDrvChk GeForceDrvChk.exe

copy LICENSE GeForceDrvChk
copy README.md GeForceDrvChk

%RACO% pkg create GeForceDrvChk

del GeForceDrvChk.zip.CHECKSUM
rmdir /s /q GeForceDrvChk

goto END

:CLEAN

del /q GeForceDrvChk.zip
del /q GeForceDrvChk.exe

:END
