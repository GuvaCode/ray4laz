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

if [[ `uname -m` -eq "x86_64" ]]
then
	PLATFORM="x86_64-linux"
fi

BASE_FLAGS="-MObjFPC -Scghi -Cg -l -vewnhibq"
INCLUDE_FLAGS="-Fl$HERE/libs/$PLATFORM -Fu$HERE/source -Fu$HERE/headers"

fpc $BASE_FLAGS $INCLUDE_FLAGS $*
