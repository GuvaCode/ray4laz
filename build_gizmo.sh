#!/bin/bash

#https://github.com/cloudofoz/raylib-gizmo
# sudo apt install gcc-multilib
# sudo apt install g++-multilib

rm -f libs/x86_64-linux/libraygizmo.a
rm -f libs/x86_32-linux/libraygizmo.a
rm -f libs/x86_64-win64/libraygizmo.dll
rm -f libs/i386-win32/libraygizmo.dll

git clone https://github.com/GuvaCode/raylib-gizmo

cd raylib-gizmo
git submodule update --init --recursive
mkdir build
cd build
rm -rf *

echo "Build x86_64_LINUX statics" 
cmake -DRGIZMO_BUILD_EXAMPLES=OFF .. 
cmake --build .
cp libraygizmo.a ../../libs/x86_64-linux/libraygizmo.a
rm -rf *

echo "Build x86_32_Linux statics" 
cmake -DRGIZMO_BUILD_EXAMPLES=OFF -D CMAKE_CXX_FLAGS=-m32 .. 
cmake --build .
cp libraygizmo.a ../../libs/x86_32-linux/libraygizmo.a
rm -rf *

echo " build x64 windows"
cmake -DCMAKE_TOOLCHAIN_FILE=mingw-w64-x86_64.cmake -DRGIZMO_BUILD_SHARED=ON -DRGIZMO_BUILD_EXAMPLES=OFF ..
cmake --build .
cp libraygizmo.dll ../../libs/x86_64-win64/libraygizmo.dll

echo " build x32 windows"
cmake -DCMAKE_TOOLCHAIN_FILE=mingw-w32-x86_64.cmake -DRGIZMO_BUILD_SHARED=ON -DRGIZMO_BUILD_EXAMPLES=OFF ..
cmake --build .
cp libraygizmo.dll ../../libs/i386-win32/libraygizmo.dll
