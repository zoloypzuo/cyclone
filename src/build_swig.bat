@echo off
set CurrentDir=%cd%
set ScriptDir=%~dp0
set Args=%*
cd /d %ScriptDir%

@echo on
swig -python -c++ -Wall -o src/cyclone_wrapper.cxx -outdir ./ include/cyclone/cyclone.i
swig -python -c++ -E include/cyclone/cyclone.i >output.c

mkdir build_vs2017_win32
cd build_vs2017_win32

cmake -G  "Visual Studio 15" ..
cmake --build . --config release --target cyclone_py

cd %ScriptDir%

copy build_vs2017_win32\src\Release\cyclone_py.dll ..\..\..\ZeloEngineScript\_cyclone.pyd
copy cyclone.py ..\..\..\ZeloEngineScript\cyclone.py

cd %CurrentDir%
pause
