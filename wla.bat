@echo off
cd /d %~dp0
goto continue

Notes:
to compile EXAMPLE.ASM ---> EXAMPLE.SMC
from the command prompt type:  wla EXAMPLE
OR
drag and drop ASM file onto this BAT file

:continue
echo [objects] > temp.prj
echo %~n1.obj >> temp.prj

wla-65816 -o %~n1.asm %~n1.obj
wlalink -vr temp.prj %~n1.sfc

del %~n1.obj
del temp.prj

pause
