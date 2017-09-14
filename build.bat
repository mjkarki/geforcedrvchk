@echo off

set RACO="C:\Program Files\Racket\raco.exe"

%RACO% exe --gui GeForceDrvChk.rkt
%RACO% distribute GeForceDrvChk GeForceDrvChk.exe

copy LICENSE GeForceDrvChk
copy README.md GeForceDrvChk

%RACO% pkg create GeForceDrvChk

del GeForceDrvChk.zip.CHECKSUM
rmdir /s /q GeForceDrvChk
