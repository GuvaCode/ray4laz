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
#cp ../Cmake_Media.txt CMakeLists.txt
mkdir build
cd build

rm -rf *
echo "Build x86_64_LINUX statics" 
cmake -DRMEDIA_BUILD_EXAMPLES=OFF .. 
cmake --build .
cp libraymedia.a ../../../libs/x86_64-linux/libraymedia.a

rm -rf *
echo "Build x86_32_Linux statics" 
cmake -DRMEDIA_BUILD_EXAMPLES=OFF -DCMAKE_CXX_FLAGS=-m32 .. 
cmake --build .
cp libraymedia.a ../../../libs/x86_32-linux/libraymedia.a


rm -rf *
echo " build x64 windows"
cmake -DCMAKE_TOOLCHAIN_FILE=mingw-w64-x86_64.cmake -DRMEDIA_BUILD_SHARED=ON -DRMEDIA_BUILD_EXAMPLES=OFF ..
cmake --build .
cp libraymedia.dll ../../../libs/x86_64-win64/libraymedia.dll
#cp libr3d.dll.a ../../../libs/x86_64-win64/libr3d.dll.a

rm -rf *
echo " build x32 windows"
cmake -DCMAKE_TOOLCHAIN_FILE=mingw-w32-x86_64.cmake -DRMEDIA_BUILD_SHARED=ON -DRMEDIA_BUILD_EXAMPLES=OFF ..
cmake --build .
cp libraymedia.dll ../../../libs/i386-win32/libraymedia.dll
#cp libr3d.dll.a ../../../libs/i386-win32/libr3d.dll.a

cd ../../
