#!/bin/bash

#https://github.com/cloudofoz/raylib-gizmo
# sudo apt install gcc-multilib
# sudo apt install g++-multilib

rm -f libs/x86_64-linux/libraygizmo*
rm -f libs/x86_64-win64/libraygizmo*

git clone -b cmake-support https://github.com/cloudofoz/raylib-gizmo


cp mingw-w32-x86_64.cmake r3d/mingw-w32-x86_64.cmake
cp mingw-w64-x86_64.cmake r3d/mingw-w64-x86_64.cmake
cp -r raylib raylib-gizmo/external

cd raylib-gizmo
#git submodule update --init --recursive
mkdir build
cd build
rm -rf *

echo "Build x86_64_LINUX statics" 
cmake -DRGIZMO_BUILD_EXAMPLES=OFF .. 
cmake --build .
cp libraygizmo.a ../../../libs/x86_64-linux/libraygizmo.a
rm -rf *

echo " build x64 windows"
cmake -DCMAKE_TOOLCHAIN_FILE=mingw-w64-x86_64.cmake -DRGIZMO_BUILD_EXAMPLES=OFF ..
cmake --build .
cp libraygizmo.a ../../../libs/x86_64-win64/libraygizmo.a
#cp libraygizmo.dll.a ../../libs/x86_64-win64/libraygizmo.dll.a
cd ../../



