#!/bin/bash

echo -e "\e[91m \e[1m"
echo "raylib build scripts "
echo -e "\e[92m \e[1m"
read -p "Install dependencies (y/n)?" answer
case ${answer:0:1} in y|Y )
echo -e "\e[0"
sudo apt-get install mingw-w64-x86-64-dev
sudo apt-get install mingw-w64-i686-dev
sudo apt-get install unzip
sudo apt-get install gcc-mingw-w64-x86-64     
sudo apt-get install gcc-mingw-w64-i686
sudo apt-get install gcc-mingw-w64-i686-posix
sudo apt-get install gcc-mingw-w64-i686-win32
sudo apt-get install build-essential libc6-dev-i386
sudo apt-get install libgl1-mesa-dev:i386
    ;;
    * )
        echo skiping
        echo -e "\e[0"

    ;;
esac
        echo -e "\e[0m"  

clear
rm -f master

mkdir libs
mkdir libs/x86_64-linux
mkdir libs/x86_32-linux
mkdir libs/x86_64-windows
mkdir libs/x86_32-windows

echo -e "\e[92m \e[1m"
echo "Download raylib master branch"
echo -e "\e[0m"  

wget https://codeload.github.com/raysan5/raylib/zip/refs/heads/master -q --show-progress

mv master master.zip
echo -e "\e[92m \e[1m"
echo "unpack ..."
echo -e "\e[0m"  
unzip master.zip
mv raylib-master raylib_tmp

rm master.zip
echo -e "\e[92m \e[1m"
echo "build raylib ...."
echo "build x64 linux ..."

mkdir raylib_tmp/src/extras

echo "Download raygui"
wget https://raw.githubusercontent.com/raysan5/raygui/master/src/raygui.h -q --show-progress
echo "Download raymath "
wget https://raw.githubusercontent.com/raysan5/physac/master/src/physac.h -q --show-progress

mv physac.h raylib_tmp/src/extras/physac.h
mv raygui.h raylib_tmp/src/extras/raygui.h
cd raylib_tmp/src
echo -e "\e[0m"  
echo -e "\e[34m \e[1m"
echo "Build x86_64_LINUX dynlib" 
echo "#define RAYGUI_IMPLEMENTATION" > raygui.c && echo "#include <extras/raygui.h>" >> raygui.c
echo "#define PHYSAC_IMPLEMENTATION" > physac.c && echo "#include <extras/physac.h>" >> physac.c
echo -e "\e[0m"  
make PLATFORM=PLATFORM_DESKTOP RAYLIB_LIBTYPE=SHARED RAYLIB_MODULE_RAYGUI=TRUE RAYLIB_MODULE_PHYSAC=TRUE

echo -e "\e[92m \e[1m"
echo " copy libs x86_64-linux ..."
rm -f ../../libs/x86_64-linux/*
cp libraylib.so.5.0.0 ../../libs/x86_64-linux/libraylib.so
echo -e "\e[0m"

echo -e "\e[34m \e[1m"  
echo "Build x86_64_LINUX Statics ---------------------------------------------" 
echo -e "\e[0m"
make clean
echo "#define RAYGUI_IMPLEMENTATION" > raygui.c && echo "#include <extras/raygui.h>" >> raygui.c
echo "#define PHYSAC_IMPLEMENTATION" > physac.c && echo "#include <extras/physac.h>" >> physac.c
make PLATFORM=PLATFORM_DESKTOP RAYLIB_MODULE_RAYGUI=TRUE RAYLIB_MODULE_PHYSAC=TRUE
cp libraylib.a ../../libs/x86_64-linux/libraylib.a

echo -e "\e[34m \e[1m"  
echo "build x86_32 linux"
echo -e "\e[0m"
make clean
echo "#define RAYGUI_IMPLEMENTATION" > raygui.c && echo "#include <extras/raygui.h>" >> raygui.c
echo "#define PHYSAC_IMPLEMENTATION" > physac.c && echo "#include <extras/physac.h>" >> physac.c
make PLATFORM=PLATFORM_DESKTOP RAYLIB_LIBTYPE=SHARED RAYLIB_MODULE_RAYGUI=TRUE RAYLIB_MODULE_PHYSAC=TRUE LDFLAG=-m32
rm -f ../../libs/x86_32-linux/*
cp libraylib.so.5.0.0 ../../libs/x86_32-linux/libraylib.so
echo -e "\e[34m \e[1m"  
echo "Build x86_32_LINUX Statics" 
echo -e "\e[0m"
make clean
echo "#define RAYGUI_IMPLEMENTATION" > raygui.c && echo "#include <extras/raygui.h>" >> raygui.c
echo "#define PHYSAC_IMPLEMENTATION" > physac.c && echo "#include <extras/physac.h>" >> physac.c
make PLATFORM=PLATFORM_DESKTOP RAYLIB_MODULE_RAYGUI=TRUE RAYLIB_MODULE_PHYSAC=TRUE LDFLAG=-m32
cp libraylib.a ../../libs/x86_32-linux
#--------------------------------------------------------------------------------------------------------

make clean 
echo -e "\e[34m \e[1m"  
echo " build x64 windows"
echo -e "\e[0m"
x86_64-w64-mingw32-windres raylib.rc -o raylib.rc.data
x86_64-w64-mingw32-windres raylib.dll.rc -o raylib.dll.rc.data

echo "#define RAYGUI_IMPLEMENTATION" > raygui.c && echo "#include <extras/raygui.h>" >> raygui.c
echo "#define PHYSAC_IMPLEMENTATION" > physac.c && echo "#include <extras/physac.h>" >> physac.c

make PLATFORM=PLATFORM_DESKTOP RAYLIB_LIBTYPE=SHARED RAYLIB_MODULE_RAYGUI=TRUE RAYLIB_MODULE_PHYSAC=TRUE OS=Windows_NT CC=x86_64-w64-mingw32-gcc AR=x86_64-w64-mingw32-ar 

rm -f ../../libs/x86_64-windows/*
cp libraylibdll.a ../../libs/x86_64-windows
cp raylib.dll ../../libs/x86_64-windows

make clean
echo -e "\e[34m \e[1m"  
echo " build x32 windows"
echo -e "\e[0m"
i686-w64-mingw32-windres raylib.rc -o raylib.rc.data
i686-w64-mingw32-windres raylib.dll.rc -o raylib.dll.rc.data

echo "#define RAYGUI_IMPLEMENTATION" > raygui.c && echo "#include <extras/raygui.h>" >> raygui.c
echo "#define PHYSAC_IMPLEMENTATION" > physac.c && echo "#include <extras/physac.h>" >> physac.c

make PLATFORM=PLATFORM_DESKTOP RAYLIB_LIBTYPE=SHARED RAYLIB_MODULE_RAYGUI=TRUE RAYLIB_MODULE_PHYSAC=TRUE OS=Windows_NT CC=i686-w64-mingw32-gcc AR=i686-w64-mingw32-ar

rm -f ../../libs/x86_32-windows/*
cp libraylibdll.a ../../libs/x86_32-windows
cp raylib.dll ../../libs/x86_32-windows

cd ../../
rm -rvf raylib_tmp

echo -e "\e[92m \e[1m"
echo "--------------------"
echo "| All done ..      |"
echo "--------------------"
echo -e "\e[0m"

