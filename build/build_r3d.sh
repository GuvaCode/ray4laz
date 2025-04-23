#!/bin/bash
#rm -rvf r3d
rm -f ../libs/x86_64-linux/libr3d*
rm -f ../libs/x86_32-linux/libr3d*
rm -f ../libs/x86_64-win64/libr3d*
rm -f ../libs/i386-win32/libr3d*

git clone https://github.com/Bigfoot71/r3d
#git clone https://github.com/GuvaCode/r3d

cp mingw-w32-x86_64.cmake r3d/mingw-w32-x86_64.cmake
cp mingw-w64-x86_64.cmake r3d/mingw-w64-x86_64.cmake

cp -r raylib r3d/external

cd r3d
mkdir build
cd build

rm -rf *
echo "Build x86_64_LINUX statics" 
cmake -DR3D_BUILD_EXAMPLES=OFF -DR3D_RAYLIB_VENDORED=ON .. 
cmake --build .
cp libr3d.a ../../../libs/x86_64-linux/libr3d.a

rm -rf *
echo "Build x86_32_Linux statics" 
cmake -DR3D_BUILD_EXAMPLES=OFF -DR3D_RAYLIB_VENDORED=ON -D CMAKE_CXX_FLAGS=-m32 .. 
cmake --build .
cp libr3d.a ../../../libs/x86_32-linux/libr3d.a


rm -rf *
echo " build x64 windows"
cmake -DCMAKE_TOOLCHAIN_FILE=mingw-w64-x86_64.cmake -DR3D_BUILD_SHARED=ON -DR3D_RAYLIB_VENDORED=ON -DR3D_BUILD_EXAMPLES=OFF ..
cmake --build .
cp libr3d.dll ../../../libs/x86_64-win64/libr3d.dll
#cp libr3d.dll.a ../../../libs/x86_64-win64/libr3d.dll.a

rm -rf *
echo " build x32 windows"
cmake -DCMAKE_TOOLCHAIN_FILE=mingw-w32-x86_64.cmake -DR3D_BUILD_SHARED=ON -DR3D_RAYLIB_VENDORED=ON -DR3D_BUILD_EXAMPLES=OFF ..
cmake --build .
cp libr3d.dll ../../../libs/i386-win32/libr3d.dll
#cp libr3d.dll.a ../../../libs/i386-win32/libr3d.dll.a

cd ../../
