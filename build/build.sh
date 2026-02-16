#!/bin/bash

# Цвета для вывода
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

echo -e "${GREEN}raylib build scripts ${NC}"
echo -e "${YELLOW}"
read -p "Installing dependencies...(y/n)?" answer
echo -e "${NC}"

case ${answer:0:1} in y|Y )
sudo apt install -y libasound2-dev libx11-dev libxrandr-dev libxi-dev libgl1-mesa-dev libglu1-mesa-dev libxcursor-dev libxinerama-dev libwayland-dev libxkbcommon-dev
sudo apt-get install -y mingw-w64-x86-64-dev

sudo apt-get install -y unzip
sudo apt-get install -y gcc-mingw-w64-x86-64     
    ;;
    * )
        echo skiping
        echo -e "\e[0"
    ;;
esac
echo -e "\e[0m"  

clear

echo -e "${RED}"
read -p "Remove old download library (y/n)?" answer
echo -e "${NC}"
case ${answer:0:1} in y|Y )
rm -rvf raylib
rm -rvf raylib-gizmo
rm -rvf raygui
    ;;
    * )
        echo skiping
        echo -e "\e[0"
    ;;
esac
echo -e "\e[0m"  

clear

mkdir -p ../libs
mkdir -p ../libs/x86_64-linux
mkdir -p ../libs/x86_64-win64
mkdir -p ../libs/i686-win32   # Для Win32 динамики

echo -e "${GREEN}Download raylib master branch${NC}"

git clone https://github.com/raysan5/raylib.git

# Раскомментировать поддержку HDR в config.h
echo -e "${YELLOW}Enabling HDR file format support${NC}"
sed -i 's|//#define SUPPORT_FILEFORMAT_HDR|#define SUPPORT_FILEFORMAT_HDR|g' raylib/src/config.h

# Скачиваем raygui
git clone https://github.com/raysan5/raygui.git

# Определяем путь к raygui (аналогично Makefile)
RAYGUI_PATH="../../raygui/src"

# Создаем raygui.c файл для сборки
echo "#define RAYGUI_IMPLEMENTATION" > raylib/src/raygui.c
echo "#include \"$RAYGUI_PATH/raygui.h\"" >> raylib/src/raygui.c

# Копируем только raygui.h
cp raygui/src/raygui.h raylib/src/

# compiler shader
cp ../headers/extras/shader_compiler.c raylib/src
cp compiler_linux raylib/src
cp compiler_windows raylib/src

cd raylib/src

rm -f ../../../libs/x86_64-linux/libraylib*
rm -f ../../../libs/x86_64-win64/libraylib*
rm -f ../../../libs/i686-win32/libraylib*

echo ""

clear

echo -e "${YELLOW}"
read -p "build shader compilers (y/n)?" answer
echo -e "${NC}"

case ${answer:0:1} in y|Y )
make clean
rm -f ../tool/shader_compiler_linux64
rm -f ../tool/shader_compiler_windows64.exe
echo " "
echo "-------------------------------"
echo -e "${YELLOW}Build shader compiler linux-x64${NC}"
echo "-------------------------------"
echo " "
make PLATFORM=PLATFORM_DESKTOP 
make -f compiler_linux 
cp libraylib.a ../../../libs/x86_64-linux/libraylib.a
cp shader_compiler ../../../tool/shader_compiler_linux64
rm shader_compiler

make clean
echo " "
echo "---------------------------------"
echo -e "${YELLOW}Build shader compiler windows-x64${NC}"
echo "---------------------------------"
echo " "
make PLATFORM=PLATFORM_DESKTOP OS=Windows_NT CC=x86_64-w64-mingw32-gcc AR=x86_64-w64-mingw32-ar 
make -f compiler_windows OS=Windows_NT CC=x86_64-w64-mingw32-gcc AR=x86_64-w64-mingw32-ar 
cp shader_compiler.exe ../../../tool/shader_compiler_windows64.exe
rm shader_compiler.exe
    ;;
    * )
        echo skiping
        echo -e "\e[0"
    ;;
esac
echo -e "\e[0m"  

clear

echo " "
echo "---------------------------------"
echo -e "${YELLOW}Build x86_64_LINUX statics${NC}"
echo "---------------------------------"
echo " "
make clean

make PLATFORM=PLATFORM_MEMORY GRAPHICS=GRAPHICS_API_OPENGL_11_SOFTWARE RAYLIB_MODULE_RAYGUI=TRUE
cp libraylib.a ../../../libs/x86_64-linux/libraylib_membuffer.a

make clean
make PLATFORM=PLATFORM_DESKTOP RAYLIB_MODULE_RAYGUI=TRUE 
cp libraylib.a ../../../libs/x86_64-linux/libraylib.a

echo " "
echo "---------------------------------"
echo -e "${YELLOW}Build x86_64_LINUX dynamic${NC}"
echo "---------------------------------"
echo " "
make clean
make PLATFORM=PLATFORM_DESKTOP RAYLIB_LIBTYPE=SHARED RAYLIB_MODULE_RAYGUI=TRUE
cp libraylib.so* ../../../libs/x86_64-linux/
# Создаем симлинк для libraylib.so без версии
ln -sf libraylib.so.*.?.? ../../../libs/x86_64-linux/libraylib.so

echo " "
echo "---------------------------------"
echo -e "${YELLOW}Build x64 Windows statics${NC}"
echo "---------------------------------"
echo " "

make clean
x86_64-w64-mingw32-windres raylib.rc -o raylib.rc.data
x86_64-w64-mingw32-windres raylib.dll.rc -o raylib.dll.rc.data

# Используем RAYLIB_MODULE_RAYGUI_PATH вместо прямых путей
RAYLIB_MODULE_RAYGUI_PATH="../../raygui/src"
# Обновляем raygui.c с правильным путем
echo "#define RAYGUI_IMPLEMENTATION" > raygui.c
echo "#include \"$RAYLIB_MODULE_RAYGUI_PATH/raygui.h\"" >> raygui.c
cp $RAYLIB_MODULE_RAYGUI_PATH/raygui.h .

make PLATFORM=PLATFORM_MEMORY GRAPHICS=GRAPHICS_API_OPENGL_11_SOFTWARE RAYLIB_MODULE_RAYGUI=TRUE OS=Windows_NT CC=x86_64-w64-mingw32-gcc AR=x86_64-w64-mingw32-ar 
cp libraylib.a ../../../libs/x86_64-win64/libraylib_membuffer.a

make clean
x86_64-w64-mingw32-windres raylib.rc -o raylib.rc.data
x86_64-w64-mingw32-windres raylib.dll.rc -o raylib.dll.rc.data

# Используем RAYLIB_MODULE_RAYGUI_PATH вместо прямых путей
echo "#define RAYGUI_IMPLEMENTATION" > raygui.c
echo "#include \"$RAYLIB_MODULE_RAYGUI_PATH/raygui.h\"" >> raygui.c
cp $RAYLIB_MODULE_RAYGUI_PATH/raygui.h .

make PLATFORM=PLATFORM_DESKTOP RAYLIB_MODULE_RAYGUI=TRUE OS=Windows_NT CC=x86_64-w64-mingw32-gcc AR=x86_64-w64-mingw32-ar 
cp libraylib.a ../../../libs/x86_64-win64/libraylib.a

echo " "
echo "---------------------------------"
echo -e "${YELLOW}Build x64 Windows dynamic${NC}"
echo "---------------------------------"
echo " "

make clean
x86_64-w64-mingw32-windres raylib.rc -o raylib.rc.data
x86_64-w64-mingw32-windres raylib.dll.rc -o raylib.dll.rc.data

# Используем RAYLIB_MODULE_RAYGUI_PATH вместо прямых путей
echo "#define RAYGUI_IMPLEMENTATION" > raygui.c
echo "#include \"$RAYLIB_MODULE_RAYGUI_PATH/raygui.h\"" >> raygui.c
cp $RAYLIB_MODULE_RAYGUI_PATH/raygui.h .

make PLATFORM=PLATFORM_DESKTOP RAYLIB_LIBTYPE=SHARED RAYLIB_MODULE_RAYGUI=TRUE OS=Windows_NT CC=x86_64-w64-mingw32-gcc AR=x86_64-w64-mingw32-ar 
cp raylib.dll ../../../libs/x86_64-win64/libraylib.dll
# Копируем также .dll.a для линковки
cp libraylib.dll.a ../../../libs/x86_64-win64/

echo " "
echo "---------------------------------"
echo -e "${YELLOW}Build Win32 dynamic only${NC}"
echo "---------------------------------"
echo " "

make clean
i686-w64-mingw32-windres raylib.rc -o raylib.rc.data
i686-w64-mingw32-windres raylib.dll.rc -o raylib.dll.rc.data

# Используем RAYLIB_MODULE_RAYGUI_PATH вместо прямых путей
echo "#define RAYGUI_IMPLEMENTATION" > raygui.c
echo "#include \"$RAYLIB_MODULE_RAYGUI_PATH/raygui.h\"" >> raygui.c
cp $RAYLIB_MODULE_RAYGUI_PATH/raygui.h .

make PLATFORM=PLATFORM_DESKTOP RAYLIB_LIBTYPE=SHARED RAYLIB_MODULE_RAYGUI=TRUE OS=Windows_NT CC=i686-w64-mingw32-gcc AR=i686-w64-mingw32-ar 
cp raylib.dll ../../../libs/i686-win32/libraylib.dll
# Копируем также .dll.a для линковки
cp libraylib.dll.a ../../../libs/i686-win32/

cd ../../

sh build_gizmo.sh 

echo " "
echo "---------------------------------"
echo -e "${GREEN}All done.${NC}"
echo "---------------------------------"
echo " "
echo -e "${RED}"
read -p "Clean download library (y/n)?" answer
echo -e "${NC}"
case ${answer:0:1} in y|Y )
rm -rvf raylib
rm -rvf raylib-gizmo
rm -rvf raygui
    ;;
    * )
        echo skiping
        echo -e "\e[0"
    ;;
esac
echo -e "\e[0m"
