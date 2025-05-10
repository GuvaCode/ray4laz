#!/bin/bash


# sudo apt install gcc-multilib
# sudo apt install g++-multilib



make clean
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

