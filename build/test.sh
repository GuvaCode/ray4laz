#!/bin/bash
#rm -rvf r3d
rm -f ../libs/x86_64-linux/libr3d*
rm -f ../libs/x86_32-linux/libr3d*


git clone --recurse-submodules https://github.com/Bigfoot71/r3d
cd r3d
git submodule update --init --recursive




mkdir build
cd build

rm -rf *

cmake .. -DCMAKE_BUILD_TYPE=Release -DBUILD_SHARED_LIBS=ON -DR3D_BUILD_EXAMPLES=OFF -DR3D_RAYLIB_VENDORED=ON -DR3D_ASSIMP_VENDORED=ON 
cmake --build .
#cp libr3d.a ../../../libs/x86_64-linux/libr3d.a
#cp libr3d.a ../../../libs/x86_64-linux/libr3d.a
