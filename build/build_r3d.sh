#!/bin/bash

rm -f ../libs/x86_64-linux/libr3d*
rm -f ../libs/x86_64-linux/libassimp*
rm -f ../libs/x86_32-linux/libr3d*
rm -f ../libs/x86_32-linux/libassimp*
rm -f ../libs/x86_64-win64/libr3d*
rm -f ../libs/x86_64-win64/libassimp*
rm -f ../libs/i386-win32/libr3d*
rm -f ../libs/i386-win32/libassimp*

git clone https://github.com/Bigfoot71/r3d
cd r3d/external

git clone https://github.com/assimp/assimp

cd ../../
cp -r raylib r3d/external

cd r3d

if ! grep -q "RAYLIB_MODULE_RAYGUI" CMakeLists.txt; then
    sed -i '/add_subdirectory("${R3D_RAYLIB_SUBMODULE_PATH}")/i \        set(RAYLIB_MODULE_RAYGUI TRUE CACHE BOOL "Enable RayGUI module")' CMakeLists.txt
fi

cp cmake/mingw-w32-x86_64.cmake mingw-w32-x86_64.cmake
cp cmake/mingw-w64-x86_64.cmake mingw-w64-x86_64.cmake

mkdir build
cd build

rm -rf *

echo " "
echo " -------------------------- "
echo " Build R3D x86_64_LINUX     "
echo " -------------------------- "
echo " "

cmake .. \
    -DCMAKE_POSITION_INDEPENDENT_CODE=ON \
    -DBUILD_SHARED_LIBS=ON \
    -DRAYLIB_MODULE_RAYGUI=ON \
    -DR3D_BUILD_EXAMPLES=OFF \
    -DR3D_RAYLIB_VENDORED=ON \
    -DR3D_ASSIMP_VENDORED=ON \
    -DR3D_BUILD_DOCS=OFF 

cmake --build .

cp libr3d.so ../../../libs/x86_64-linux/libr3d.so
# Переходим в директорию с библиотеками для создания символических ссылок
cp -P external/assimp/bin/libassimp.so* ../../../libs/x86_64-linux/



rm -rf *
echo " "
echo " -------------------------- "
echo " Build R3D x86_32_LINUX     "
echo " -------------------------- "
echo " "

cmake .. \
    -DCMAKE_POSITION_INDEPENDENT_CODE=ON \
    -DBUILD_SHARED_LIBS=ON \
    -DRAYLIB_MODULE_RAYGUI=ON \
    -DR3D_BUILD_EXAMPLES=OFF \
    -DR3D_RAYLIB_VENDORED=ON \
    -DR3D_ASSIMP_VENDORED=ON \
    -DCMAKE_CXX_FLAGS="-m32 -L/usr/lib32" \
    -DCMAKE_C_FLAGS="-m32 -L/usr/lib32" \
    -DR3D_BUILD_DOCS=OFF 

cmake --build .

cp libr3d.so ../../../libs/x86_32-linux/libr3d.so
# Копируем, сохраняя структуру ссылок (рекомендуется)
cp -P external/assimp/bin/libassimp.so* ../../../libs/x86_32-linux/


rm -rf *
echo " "
echo " -------------------------- "
echo " Build R3D x64 WINDOWS      "
echo " -------------------------- "
echo " "

cmake .. \
    -DCMAKE_TOOLCHAIN_FILE=mingw-w64-x86_64.cmake \
    -DCMAKE_POSITION_INDEPENDENT_CODE=ON \
    -DBUILD_SHARED_LIBS=ON \
    -DRAYLIB_MODULE_RAYGUI=ON \
    -DR3D_BUILD_EXAMPLES=OFF \
    -DR3D_RAYLIB_VENDORED=ON \
    -DR3D_ASSIMP_VENDORED=ON \
    -DR3D_BUILD_DOCS=OFF 

cmake --build .
cp libr3d.dll ../../../libs/x86_64-win64/libr3d.dll
#cp external/assimp/bin/libassimp-6.dll ../../../libs/x86_64-win64/libassimp-6.dll

wget https://github.com/assimp/assimp/releases/download/v6.0.2/windows-x64-v6.0.2.zip
unzip windows-x64-v6.0.2.zip 
cp Release/assimp-vc143-mt.dll ../../../libs/x86_64-win64/libassimp-6.dll


rm -rf *
echo " "
echo " -------------------------- "
echo " Build R3D x32 WINDOWS      "
echo " -------------------------- "
echo " "

cmake .. \
    -DCMAKE_TOOLCHAIN_FILE=mingw-w32-x86_64.cmake \
    -DCMAKE_POSITION_INDEPENDENT_CODE=ON \
    -DBUILD_SHARED_LIBS=ON \
    -DRAYLIB_MODULE_RAYGUI=ON \
    -DR3D_BUILD_EXAMPLES=OFF \
    -DR3D_RAYLIB_VENDORED=ON \
    -DR3D_ASSIMP_VENDORED=ON \
    -DR3D_BUILD_DOCS=OFF 

cmake --build .
cp libr3d.dll ../../../libs/i386-win32/libr3d.dll
#cp external/assimp/bin/libassimp-6.dll ../../../libs/i386-win32/libassimp-6.dll
#cp external/raylib/raylib/libraylib.dll ../../../libs/i386-win32/libraylib.dll

wget https://github.com/assimp/assimp/releases/download/v6.0.2/windows-x86-v6.0.2.zip
unzip windows-x86-v6.0.2.zip
cp Release/assimp-vc143-mt.dll ../../../libs/x86_64-win64/libassimp-6.dll


cd ../../

#https://github.com/assimp/assimp/releases/download/v6.0.2/windows-x64-v6.0.2.zip
#https://github.com/assimp/assimp/releases/download/v6.0.2/windows-x86-v6.0.2.zip



