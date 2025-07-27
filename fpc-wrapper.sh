#!/usr/bin/env bash

# a wrapper for fpc to automatically add enough flags

set -e

if [[ `command -v fpc` == "" ]]
then
	echo "Could not find fpc on your system."
	exit 1
fi

HERE=$(readlink -f $(dirname $0))

PLATFORM="x86_32-linux";

if [[ `uname` == "Linux" ]] && [[ `uname -m` -eq "x86_64" ]];
then
	PLATFORM="x86_64-linux"
fi

if [[ `uname` == "FreeBSD" ]] && [[ `uname -m` -eq "x86_64" ]];
then
	PLATFORM="x86_64-freebsd"
fi

if [[ `uname` == "OpenBSD" ]] && [[ `uname -m` -eq "x86_64" ]];
then
	PLATFORM="x86_64-openbsd"
fi

if [[ `uname` == "NetBSD" ]] && [[ `uname -m` -eq "x86_64" ]];
then
	PLATFORM="x86_64-netbsd"
fi

echo "PLATFORM=" $PLATFORM
	

BASE_FLAGS="-MObjFPC -Scghi -Cg -l -B -vewnhibq"
INCLUDE_FLAGS="-Fl$HERE/libs/$PLATFORM -Fu$HERE/source -Fu$HERE/headers"

fpc $BASE_FLAGS $INCLUDE_FLAGS $*
