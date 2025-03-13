#!/bin/bash
#rm -rvf raylib
#rm -rvf raygui

echo "raylib build scripts "
read -p "Install dependencies (y/n)?" answer
case ${answer:0:1} in y|Y )
sudo apt install -y libasound2-dev libx11-dev libxrandr-dev libxi-dev libgl1-mesa-dev libglu1-mesa-dev libxcursor-dev libxinerama-dev libwayland-dev libxkbcommon-dev
sudo apt-get install -y mingw-w64-x86-64-dev
sudo apt-get install -y mingw-w64-i686-dev
sudo apt-get install -y unzip
sudo apt-get install -y gcc-mingw-w64-x86-64     
sudo apt-get install -y gcc-mingw-w64-i686
sudo apt-get install -y gcc-mingw-w64-i686-posix
sudo apt-get install -y gcc-mingw-w64-i686-win32
sudo apt-get install -y build-essential #libc6-dev-i386
sudo apt-get install -y libgl1-mesa-dev:i386
    ;;
    * )
        echo skiping
        echo -e "\e[0"

    ;;
esac
        echo -e "\e[0m"  

clear

mkdir libs
mkdir libs/x86_64-linux
mkdir libs/x86_32-linux
mkdir libs/x86_64-win64
mkdir libs/i386-win32

echo "Download raylib master branch"

git clone https://github.com/raysan5/raylib.git
git clone https://github.com/raysan5/raygui.git

cd raylib/src

rm -f ../../libs/x86_64-linux/*
rm -f ../../libs/x86_32-linux/*
rm -f ../../libs/x86_64-win64/*
rm -f ../../libs/i386-win32/*

echo "Build x86_64_LINUX dynamic" 
make clean  
make PLATFORM=PLATFORM_DESKTOP RAYLIB_LIBTYPE=SHARED RAYLIB_MODULE_RAYGUI=TRUE 
cp libraylib.so.5.5.0 ../../libs/x86_64-linux/libraylib.so.550

echo "Build x86_64_LINUX statics" 
make clean
make PLATFORM=PLATFORM_DESKTOP RAYLIB_MODULE_RAYGUI=TRUE 
cp libraylib.a ../../libs/x86_64-linux/libraylib.a

echo "Build x86_32_Linux dynamic"
make clean
make PLATFORM=PLATFORM_DESKTOP RAYLIB_LIBTYPE=SHARED RAYLIB_MODULE_RAYGUI=TRUE LDFLAG=-m32
cp libraylib.so.5.5.0 ../../libs/x86_32-linux/libraylib.so.550 

echo "Build x86_32_Linux statics"
make clean
make PLATFORM=PLATFORM_DESKTOP RAYLIB_MODULE_RAYGUI=TRUE LDFLAG=-m32
cp libraylib.a ../../libs/x86_32-linux/libraylib.a

echo " build x64 windows"
x86_64-w64-mingw32-windres raylib.rc -o raylib.rc.data
x86_64-w64-mingw32-windres raylib.dll.rc -o raylib.dll.rc.data
make clean
cp ../../raygui/src/raygui.h raygui.c 
make PLATFORM=PLATFORM_DESKTOP RAYLIB_LIBTYPE=SHARED RAYLIB_MODULE_RAYGUI=TRUE OS=Windows_NT CC=x86_64-w64-mingw32-gcc AR=x86_64-w64-mingw32-ar 
cp raylib.dll ../../libs/x86_64-win64/libraylib.dll

echo " build x32 windows"
i686-w64-mingw32-windres raylib.rc -o raylib.rc.data
i686-w64-mingw32-windres raylib.dll.rc -o raylib.dll.rc.data
make clean 
cp ../../raygui/src/raygui.h raygui.c 
make PLATFORM=PLATFORM_DESKTOP RAYLIB_LIBTYPE=SHARED RAYLIB_MODULE_RAYGUI=TRUE OS=Windows_NT CC=i686-w64-mingw32-gcc AR=i686-w64-mingw32-ar
cp raylib.dll ../../libs/i386-win32/libraylib.dll

cd ../../
rm -rvf raylib
rm -rvf raygui

echo "All done ............"










