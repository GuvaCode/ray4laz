#!/bin/bash
rm -rvf raylib_tmp
rm -Rfv raygui
rm -master.zip
clear
echo -e "\e[91m \e[1m"
echo "raylib build scripts "
echo -e "\e[92m \e[1m"
read -p "Install dependencies (y/n)?" answer
case ${answer:0:1} in y|Y )
echo -e "\e[0"
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
sudo apt-get install -y libavcodec-dev libavformat-dev libavutil-dev libswresample-dev libswscale-dev
sudo apt-get install -y libavcodec-dev:386 libavformat-dev:386 libavutil-dev:386 libswresample-dev:386 libswscale-dev:386
sudo apt-get install mingw-w64-tools
# sudo apt-get install -y nasm
# ./configure --arch=x86 --target-os=mingw32 --enable-shared --cross-prefix=i686-w64-mingw32-

#sudo apt install -y emscripten
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
mkdir libs/x86_64-linux/include_raymedia
mkdir libs/x86_32-linux
mkdir libs/x86_32-linux/include_raymedia

mkdir libs/x86_64-win64
mkdir libs/i386-win32
mkdir libs/x86_64-win64/include_raymedia
mkdir libs/i386-win32/include_raymedia
#mkdir libs/wasm32-wasi

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

rm raylib_tmp/src/Makefile
cp headers/Makefile raylib_tmp/src

cp headers/extras/ray-gizmo/raygizmo.c raylib_tmp/src
cp headers/extras/ray-gizmo/raygizmo.h raylib_tmp/src

cp headers/extras/ray-media/rmedia.c raylib_tmp/src
cp headers/extras/ray-media/raymedia.h raylib_tmp/src
mkdir raylib_tmp/src/raygui
cp headers/extras/ray-gui/raygui.h raylib_tmp/src
cp headers/extras/ray-gui/raygui.h raylib_tmp/src/raygui
 
##cp headers/extras/ray-gui/raygui.h raylib_tmp/src

# download ffmpeg source

#echo "Download raygui"
#wget https://raw.githubusercontent.com/raysan5/raygui/master/src/raygui.h -q --show-progress
#wget https://github.com/raysan5/raygui/archive/refs/heads/master.zip -q --show-progress
#unzip master.zip
#mv raygui-master raygui

##cp raygui/src/raygui.h raylib_tmp/src
#mv raygui/src/raygui.h raygui/src/raygui.c
#cp raygui/src/raygui.h raylib_tmp/src

#raygui-master.zip

#./configure --arch=x86_64 --target-os=mingw32 --disable-shared --cross-prefix=x86_64-w64-mingw32-
#make

#echo "Download physac "
#wget https://raw.githubusercontent.com/raysan5/physac/master/src/physac.h -q --show-progress

#mv physac.h raylib_tmp/src/extras/physac.h
#mv raygui.h raylib_tmp/src/extras/raygui.h

cd raylib_tmp/src
echo -e "\e[0m"  
echo -e "\e[34m \e[1m"
echo "Build x86_64_LINUX dynlib" 
echo -e "\e[0m"  
#echo "#define RAYGUI_IMPLEMENTATION" > raygui.c && echo "#include <extras/raygui.h>" >> raygui.c
#echo "#define PHYSAC_IMPLEMENTATION" > physac.c && echo "#include <extras/physac.h>" >> physac.c


rm -f ../../libs/x86_64-linux/*
rm -f ../../libs/x86_64-linux/include_raymedia/*
rm -f ../../libs/x86_32-linux/*
rm -f ../../libs/x86_32-linux/include_raymedia/*
rm -f ../../libs/x86_64-win64/*
rm -f ../../libs/i386-win32/*
rm -f ../../libs/x86_64-win64/include_raymedia/*
rm -f ../../libs/i386-win32/include_raymedia/*

make clean  
make PLATFORM=PLATFORM_DESKTOP RAYLIB_LIBTYPE=SHARED RAYLIB_MODULE_RAYGUI=TRUE RAYLIB_MODULE_GIZMO=TRUE  #RAYLIB_MODULE_RAYMEDIA=TRUE
cp libraylib.so.5.5.0 ../../libs/x86_64-linux/libraylib.so.550

make clean 
make PLATFORM=PLATFORM_DESKTOP RAYLIB_LIBTYPE=SHARED RAYLIB_MODULE_RAYGUI=TRUE RAYLIB_MODULE_GIZMO=TRUE RAYLIB_MODULE_RAYMEDIA=TRUE 
cp libraylib.so.5.5.0 ../../libs/x86_64-linux/include_raymedia/libraylib.so.550 


echo -e "\e[34m \e[1m"  
echo "Build x86_64_LINUX Statics ---------------------------------------------" 
echo -e "\e[0m"
make clean

#echo "#define RAYGUI_IMPLEMENTATION" > raygui.c && echo "#include <extras/raygui.h>" >> raygui.c
#echo "#define PHYSAC_IMPLEMENTATION" > physac.c && echo "#include <extras/physac.h>" >> physac.c

make PLATFORM=PLATFORM_DESKTOP RAYLIB_MODULE_RAYGUI=TRUE RAYLIB_MODULE_GIZMO=TRUE #RAYLIB_MODULE_RAYGIZMO=TRUE 
cp libraylib.a ../../libs/x86_64-linux/libraylib.a

make clean 
make PLATFORM=PLATFORM_DESKTOP RAYLIB_MODULE_RAYGUI=TRUE RAYLIB_MODULE_GIZMO=TRUE RAYLIB_MODULE_RAYMEDIA=TRUE 
cp libraylib.a ../../libs/x86_64-linux/include_raymedia/libraylib.a


echo -e "\e[34m \e[1m"  
echo "build x86_32 linux"
echo -e "\e[0m"
make clean
#echo "#define RAYGUI_IMPLEMENTATION" > raygui.c && echo "#include <extras/raygui.h>" >> raygui.c
#echo "#define PHYSAC_IMPLEMENTATION" > physac.c && echo "#include <extras/physac.h>" >> physac.c
make PLATFORM=PLATFORM_DESKTOP RAYLIB_LIBTYPE=SHARED RAYLIB_MODULE_RAYGUI=TRUE RAYLIB_MODULE_GIZMO=TRUE LDFLAG=-m32
cp libraylib.so.5.5.0 ../../libs/x86_32-linux/libraylib.so.550 

make clean 
make PLATFORM=PLATFORM_DESKTOP RAYLIB_LIBTYPE=SHARED RAYLIB_MODULE_RAYGUI=TRUE RAYLIB_MODULE_GIZMO=TRUE RAYLIB_MODULE_RAYMEDIA=TRUE LDFLAG=-m32
cp libraylib.so.5.5.0 ../../libs/x86_32-linux/include_raymedia/libraylib.so.550 


echo -e "\e[34m \e[1m"  
echo "Build x86_32_LINUX Statics" 
echo -e "\e[0m"
make clean
#echo "#define RAYGUI_IMPLEMENTATION" > raygui.c && echo "#include <extras/raygui.h>" >> raygui.c
#echo "#define PHYSAC_IMPLEMENTATION" > physac.c && echo "#include <extras/physac.h>" >> physac.c
make PLATFORM=PLATFORM_DESKTOP RAYLIB_MODULE_RAYGUI=TRUE RAYLIB_MODULE_GIZMO=TRUE LDFLAG=-m32
cp libraylib.a ../../libs/x86_32-linux

make clean
make PLATFORM=PLATFORM_DESKTOP RAYLIB_MODULE_RAYGUI=TRUE RAYLIB_MODULE_GIZMO=TRUE RAYLIB_MODULE_RAYMEDIA=TRUE LDFLAG=-m32
cp libraylib.a ../../libs/x86_32-linux/include_raymedia/libraylib.a

#--------------------------------------------------------------------------------------------------------

#echo -e "\e[34m \e[1m"  
#echo "Build WebAssembly Statics" 
#echo -e "\e[0m"
#make clean

#make PLATFORM=PLATFORM_WEB
#cp libraylib.a ../../libs/wasm32-wasi

#--------------------------------------------------------------------------------------------------------



echo -e "\e[34m \e[1m"  
echo " build x64 windows"
echo -e "\e[0m"
x86_64-w64-mingw32-windres raylib.rc -o raylib.rc.data
x86_64-w64-mingw32-windres raylib.dll.rc -o raylib.dll.rc.data

#echo "#define RAYGUI_IMPLEMENTATION" > raygui.c && echo "#include <extras/raygui.h>" >> raygui.c
#echo "#define PHYSAC_IMPLEMENTATION" > physac.c && echo "#include <extras/physac.h>" >> physac.c
make clean 
make PLATFORM=PLATFORM_DESKTOP RAYLIB_LIBTYPE=SHARED RAYLIB_MODULE_RAYGUI=TRUE RAYLIB_MODULE_GIZMO=TRUE OS=Windows_NT CC=x86_64-w64-mingw32-gcc AR=x86_64-w64-mingw32-ar 
#cp libraylibdll.a ../../libs/x86_64-win64
cp raylib.dll ../../libs/x86_64-win64/libraylib.dll

make clean 
x86_64-w64-mingw32-windres raylib.rc -o raylib.rc.data
x86_64-w64-mingw32-windres raylib.dll.rc -o raylib.dll.rc.data
make PLATFORM=PLATFORM_DESKTOP RAYLIB_LIBTYPE=SHARED RAYLIB_MODULE_RAYGUI=TRUE RAYLIB_MODULE_GIZMO=TRUE RAYLIB_MODULE_RAYMEDIA=TRUE OS=Windows_NT CC=x86_64-w64-mingw32-gcc AR=x86_64-w64-mingw32-ar 
#cp libraylibdll.a ../../libs/x86_64-win64
cp raylib.dll ../../libs/x86_64-win64/include_raymedia/libraylib.dll

#---------------------------------------------------------------------------------------------------------

make clean
echo -e "\e[34m \e[1m"  
echo " build x32 windows"
echo -e "\e[0m"
i686-w64-mingw32-windres raylib.rc -o raylib.rc.data
i686-w64-mingw32-windres raylib.dll.rc -o raylib.dll.rc.data

#echo "#define RAYGUI_IMPLEMENTATION" > raygui.c && echo "#include <extras/raygui.h>" >> raygui.c
#echo "#define PHYSAC_IMPLEMENTATION" > physac.c && echo "#include <extras/physac.h>" >> physac.c

make PLATFORM=PLATFORM_DESKTOP RAYLIB_LIBTYPE=SHARED RAYLIB_MODULE_RAYGUI=TRUE RAYLIB_MODULE_GIZMO=TRUE OS=Windows_NT CC=i686-w64-mingw32-gcc AR=i686-w64-mingw32-ar
#cp libraylibdll.a ../../libs/i386-win32
cp raylib.dll ../../libs/i386-win32/libraylib.dll


make clean
i686-w64-mingw32-windres raylib.rc -o raylib.rc.data
i686-w64-mingw32-windres raylib.dll.rc -o raylib.dll.rc.data

#echo "#define RAYGUI_IMPLEMENTATION" > raygui.c && echo "#include <extras/raygui.h>" >> raygui.c
#echo "#define PHYSAC_IMPLEMENTATION" > physac.c && echo "#include <extras/physac.h>" >> physac.c

make PLATFORM=PLATFORM_DESKTOP RAYLIB_LIBTYPE=SHARED RAYLIB_MODULE_RAYGUI=TRUE RAYLIB_MODULE_GIZMO=TRUE RAYLIB_MODULE_RAYMEDIA=TRUE OS=Windows_NT CC=i686-w64-mingw32-gcc AR=i686-w64-mingw32-ar
#cp libraylibdll.a ../../libs/i386-win32
cp raylib.dll ../../libs/i386-win32/include_raymedia/libraylib.dll


#---------------------------------------------------------------------------------------------------
cd ../../
rm -rvf raylib_tmp
rm -Rfv raygui
rm -master.zip
#---------------------------------------------------------------------------------------------------
echo -e "\e[92m \e[1m"
echo "--------------------"
echo "| All done ..      |"
echo "--------------------"
echo -e "\e[0m"

