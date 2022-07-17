@echo off
set CurrentDir=%cd%
set ScriptDir=%~dp0
set Args=%*
cd /d %ScriptDir%

@echo on
swig -lua -c++ -Wall -no-old-metatable-bindings -nomoduleglobal -o src/cyclone_wrapper.cxx -outdir ./ include/cyclone/cyclone.i
swig -lua -c++ -E include/cyclone/cyclone.i >output.c

cd %CurrentDir%
pause
