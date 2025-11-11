#!/bin/bash

rm -f ../libs/x86_64-linux/libr3d*
rm -f ../libs/x86_64-linux/libassimp*



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



#https://github.com/assimp/assimp/releases/download/v6.0.2/windows-x64-v6.0.2.zip
#https://github.com/assimp/assimp/releases/download/v6.0.2/windows-x86-v6.0.2.zip



