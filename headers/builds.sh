 #!/bin/bash

sudo apt-get install mingw-w64-x86-64-dev
sudo apt-get install mingw-w64-i686-dev

sudo apt-get install gcc-mingw-w64-x86-64     
sudo apt-get install gcc-mingw-w64-i686
sudo apt-get install gcc-mingw-w64-i686-posix
sudo apt-get install gcc-mingw-w64-i686-win32
sudo apt-get install build-essential libc6-dev-i386
sudo apt-get install libgl1-mesa-dev:i386

clear
echo "build raylib ...."


echo " build x64 linux ..."
cd raylib/src
make clean
make PLATFORM=PLATFORM_DESKTOP RAYLIB_LIBTYPE=SHARED RAYLIB_MODULE_RAYGUI=TRUE RAYLIB_MODULE_PHYSAC=TRUE

echo " copy libs x86_64-linux ..."
cp libraylib.so.5.0.0 /home/vadim/Проекты/Ray4Laz/libs/x86_64-linux
cp libraylib.so.500 /home/vadim/Проекты/Ray4Laz/libs/x86_64-linux
cp libraylib.so /home/vadim/Проекты/Ray4Laz/libs/x86_64-linux

make clean

make PLATFORM=PLATFORM_DESKTOP RAYLIB_MODULE_RAYGUI=TRUE RAYLIB_MODULE_PHYSAC=TRUE
cp libraylib.a /home/vadim/Проекты/Ray4Laz/libs/x86_64-linux


echo " build x86_32 linux ..."
cd raylib/src
make clean
make PLATFORM=PLATFORM_DESKTOP RAYLIB_LIBTYPE=SHARED RAYLIB_MODULE_RAYGUI=TRUE RAYLIB_MODULE_PHYSAC=TRUE LDFLAG=-m32


echo " copy libs x86_32-linux ..."
cp libraylib.so.5.0.0 /home/vadim/Проекты/Ray4Laz/libs/x86_32-linux
cp libraylib.so.500 /home/vadim/Проекты/Ray4Laz/libs/x86_32-linux
cp libraylib.so /home/vadim/Проекты/Ray4Laz/libs/x86_32-linux

make clean

make PLATFORM=PLATFORM_DESKTOP RAYLIB_MODULE_RAYGUI=TRUE RAYLIB_MODULE_PHYSAC=TRUE LDFLAG=-m32
cp libraylib.a /home/vadim/Проекты/Ray4Laz/libs/x86_32-linux
#--------------------------------------------------------------------------------------------------------

make clean 
echo " build x64 windows ..."

cp ../../raygui/src/raygui.h raygui.c
cp ../../physac/src/physac.h physac.h

make PLATFORM=PLATFORM_DESKTOP RAYLIB_LIBTYPE=SHARED RAYLIB_MODULE_RAYGUI=TRUE RAYLIB_MODULE_PHYSAC=TRUE OS=Windows_NT CC=x86_64-w64-mingw32-gcc AR=x86_64-w64-mingw32-ar 

cp libraylibdll.a /home/vadim/Проекты/Ray4Laz/libs/x86_64-windows
cp raylib.dll /home/vadim/Проекты/Ray4Laz/libs/x86_64-windows
make clean

cp ../../raygui/src/raygui.h raygui.c
cp ../../physac/src/physac.h physac.h

echo " build x32 windows ..."
make PLATFORM=PLATFORM_DESKTOP RAYLIB_LIBTYPE=SHARED RAYLIB_MODULE_RAYGUI=TRUE RAYLIB_MODULE_PHYSAC=TRUE OS=Windows_NT CC=i686-w64-mingw32-gcc AR=i686-w64-mingw32-ar

cp libraylibdll.a /home/vadim/Проекты/Ray4Laz/libs/x86_32-windows
cp raylib.dll /home/vadim/Проекты/Ray4Laz/libs/x86_32-windows

rm raygui.c
rm physac.h

make clean
