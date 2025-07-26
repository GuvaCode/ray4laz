#!/bin/bash
#rm -rvf raylib
#rm -rvf raylib-gizmo
#rm -rvf r3d
#rm -rvf raylib-media

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
sudo apt-get install zlib1g-dev:i386
# sudo apt-get install -y clang llvm bmake binutils-x86-64-kfreebsd-gnu binutils-x86-64-kfreebsd-gnu 
    ;;
    * )
        echo skiping
        echo -e "\e[0"

    ;;
esac
        echo -e "\e[0m"  

clear


read -p "Remove old download library (y/n)?" answer
case ${answer:0:1} in y|Y )
rm -rvf raylib
rm -rvf raylib-gizmo
rm -rvf r3d
rm -rvf raylib-media
    ;;
    * )
        echo skiping
        echo -e "\e[0"

    ;;
esac
        echo -e "\e[0m"  

clear

mkdir ../libs
mkdir ../libs/x86_64-linux
mkdir ../libs/x86_32-linux
mkdir ../libs/x86_64-win64
mkdir ../libs/i386-win32
mkdir ../libs/x86_32-freebsd
mkdir ../libs/x86_64-freebsd

echo "Download raylib master branch"

git clone https://github.com/raysan5/raylib.git
#git clone https://github.com/raysan5/raygui.git


echo "Download raygui"
wget https://raw.githubusercontent.com/raysan5/raygui/master/src/raygui.h -q --show-progress

mkdir raylib/src/extras
mv raygui.h raylib/src/extras/raygui.h

cp ../headers/extras/shader_compiler.c raylib/src
cp compiler_linux raylib/src
cp compiler_windows raylib/src


cd raylib/src
rm -f ../../../libs/x86_64-linux/libraylib*
rm -f ../../../libs/x86_32-linux/libraylib*
rm -f ../../../libs/x86_64-win64/libraylib*
rm -f ../../../libs/i386-win32/libraylib*
cd ../../
echo ""
read -p "Build shader compiler  (y/n)?" answer
case ${answer:0:1} in y|Y )
make clean
rm -f ../tool/shader_compiler_linux32
rm -f ../tool/shader_compiler_linux64
rm -f ../tool/shader_compiler_windows32.exe
rm -f ../tool/shader_compiler_windows64.exe
echo " "
echo "-------------------------------"
echo "Build shader compiler linux-x64"
echo "-------------------------------"
echo " "
make PLATFORM=PLATFORM_DESKTOP
make -f compiler_linux 
cp libraylib.a ../../../libs/x86_64-linux/libraylib.a
cp shader_compiler ../../../tool/shader_compiler_linux64
rm shader_compiler

make clean
echo " "
echo "-------------------------------"
echo "Build shader compiler linux-x32"
echo "-------------------------------"
echo " "

make PLATFORM=PLATFORM_DESKTOP LDFLAG=-m32
make -f compiler_linux LDFLAG=-m32
cp shader_compiler ../../../tool/shader_compiler_linux32
rm shader_compiler

make clean
echo " "
echo "---------------------------------"
echo "Build shader compiler windows-x64"
echo "---------------------------------"
echo " "
make PLATFORM=PLATFORM_DESKTOP OS=Windows_NT CC=x86_64-w64-mingw32-gcc AR=x86_64-w64-mingw32-ar 
make -f compiler_windows OS=Windows_NT CC=x86_64-w64-mingw32-gcc AR=x86_64-w64-mingw32-ar 
cp shader_compiler.exe ../../../tool/shader_compiler_windows64.exe
rm shader_compiler.exe

make clean
echo " "
echo "---------------------------------"
echo "Build shader compiler windows-x32"
echo "---------------------------------"
echo " "

make PLATFORM=PLATFORM_DESKTOP OS=Windows_NT CC=i686-w64-mingw32-gcc AR=i686-w64-mingw32-ar 
i686-w64-mingw32-ranlib libraylib.a
make -f compiler_windows OS=Windows_NT CC=i686-w64-mingw32-gcc AR=i686-w64-mingw32-ar
cp shader_compiler.exe ../../../tool/shader_compiler_windows32.exe
rm shader_compiler.exe

make clean
    ;;
    * )
        echo skiping
        echo -e "\e[0"

    ;;
esac
        echo -e "\e[0m"  

clear


cd raylib/src
sed -i 's|//#define SUPPORT_FILEFORMAT_HDR      1|#define SUPPORT_FILEFORMAT_HDR      1|g' config.h

echo " "
echo "---------------------------------"
echo "Build x86_64_LINUX dynamic" 
echo "---------------------------------"
echo " "
make clean  
echo "#define RAYGUI_IMPLEMENTATION" > raygui.c && echo "#include <extras/raygui.h>" >> raygui.c
make PLATFORM=PLATFORM_DESKTOP RAYLIB_LIBTYPE=SHARED RAYLIB_MODULE_RAYGUI=TRUE 
#cp libraylib.so.5.5.0 ../../libs/x86_64-linux/libraylib.so.550
cp -P libraylib.so* ../../../libs/x86_64-linux/

echo " "
echo "---------------------------------"
echo "Build x86_64_LINUX statics" 
echo "---------------------------------"
echo " "
make clean
echo "#define RAYGUI_IMPLEMENTATION" > raygui.c && echo "#include <extras/raygui.h>" >> raygui.c
make PLATFORM=PLATFORM_DESKTOP RAYLIB_MODULE_RAYGUI=TRUE 
cp libraylib.a ../../../libs/x86_64-linux/libraylib.a


echo " "
echo "---------------------------------"
echo "Build x86_32_Linux dynamic" 
echo "---------------------------------"
echo " "
make clean
echo "#define RAYGUI_IMPLEMENTATION" > raygui.c && echo "#include <extras/raygui.h>" >> raygui.c
make PLATFORM=PLATFORM_DESKTOP RAYLIB_LIBTYPE=SHARED RAYLIB_MODULE_RAYGUI=TRUE LDFLAG=-m32
cp -P libraylib.so* ../../../libs/x86_32-linux/


echo " "
echo "---------------------------------"
echo "Build x86_32_Linux statics" 
echo "---------------------------------"
echo " "
make clean
echo "#define RAYGUI_IMPLEMENTATION" > raygui.c && echo "#include <extras/raygui.h>" >> raygui.c
make PLATFORM=PLATFORM_DESKTOP RAYLIB_MODULE_RAYGUI=TRUE LDFLAG=-m32
cp libraylib.a ../../../libs/x86_32-linux/libraylib.a

echo " "
echo "---------------------------------"
echo "Build x64 Windows dynamic" 
echo "---------------------------------"
echo " "
x86_64-w64-mingw32-windres raylib.rc -o raylib.rc.data
x86_64-w64-mingw32-windres raylib.dll.rc -o raylib.dll.rc.data

make clean
echo "#define RAYGUI_IMPLEMENTATION" > raygui.c && echo "#include <extras/raygui.h>" >> raygui.c
make PLATFORM=PLATFORM_DESKTOP RAYLIB_LIBTYPE=SHARED RAYLIB_MODULE_RAYGUI=TRUE OS=Windows_NT CC=x86_64-w64-mingw32-gcc AR=x86_64-w64-mingw32-ar 
make -f compiler_windows OS=Windows_NT CC=x86_64-w64-mingw32-gcc AR=x86_64-w64-mingw32-ar 
cp raylib.dll ../../../libs/x86_64-win64/libraylib.dll
#cp libraylibdll.a ../../../libs/x86_64-win64/libraylibdll.a 

echo " "
echo "---------------------------------"
echo "Build x32 Windows dynamic" 
echo "---------------------------------"
echo " "
i686-w64-mingw32-windres raylib.rc -o raylib.rc.data
i686-w64-mingw32-windres raylib.dll.rc -o raylib.dll.rc.data

make clean 
echo "#define RAYGUI_IMPLEMENTATION" > raygui.c && echo "#include <extras/raygui.h>" >> raygui.c
make PLATFORM=PLATFORM_DESKTOP RAYLIB_LIBTYPE=SHARED RAYLIB_MODULE_RAYGUI=TRUE OS=Windows_NT CC=i686-w64-mingw32-gcc AR=i686-w64-mingw32-ar
cp raylib.dll ../../../libs/i386-win32/libraylib.dll
#cp libraylibdll.a ../../../libs/i386-win32/libraylibdll.a

cd ../../

sh build_r3d.sh  
sh build_gizmo.sh 
sh build_media.sh

echo " "
echo "---------------------------------"
echo "All done." 
echo "---------------------------------"
echo " "










