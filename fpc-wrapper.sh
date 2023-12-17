#!/bin/sh

# a wrapper for fpc to automatically add enough flags

get_lib_name()
{
	# return 64 if 64
	if [ `uname -m` -eq "x86_64" ]
	then
		return "x86_64-linux"
	else
		return "x86_32-linux"
	fi
}
PLATFORM=get_lib_name;

BASE_FLAGS="-MObjFPC -Scghi -Cg -l -vewnhibq"
INCLUDE_FLAGS="-Fl./libs/$PLATFORM -Fu./source -Fu./headers"

fpc $BASE_FLAGS $INCLUDE_FLAGS $*
