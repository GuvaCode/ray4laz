#!/bin/bash

rm -f ../libs/x86_64-linux/librmedia*
rm -f ../libs/x86_32-linux/librmedia*
rm -f ../libs/x86_64-win64/librmedia*
rm -f ../libs/i386-win32/librmedia*

git clone https://github.com/GuvaCode/raylib-media
#git clone https://github.com/cloudofoz/raylib-media

cp mingw-w32-x86_64.cmake r3d/mingw-w32-x86_64.cmake
cp mingw-w64-x86_64.cmake r3d/mingw-w64-x86_64.cmake

cp -r raylib raylib-media/external

cd raylib-media
#rm -f CMakeLists.txt
rm -f CMakeModules/FindRAYLIB.cmake

cp ../Cmake_Media.txt CMakeLists.txt
mkdir build
cd build

rm -rf *
echo "Build x86_64_LINUX statics" 
cmake -DRMEDIA_BUILD_EXAMPLES=OFF .. 
cmake --build .
cp libraymedia.a ../../../libs/x86_64-linux/libraymedia.a

