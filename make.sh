#!/bin/sh

set -x -e

export CSC=csc
export CHICKEN_INSTALL=chicken-install

$CHICKEN_INSTALL -init $PWD/deps/.repository/
export CHICKEN_REPOSITORY=$PWD/deps/.repository/

export CFLAGS="-I$HOME/target/win32/usr/include/"
#export LDFLAGS="-C -static-libgcc"

export CSC_OPTIONS="$CFLAGS"
for dep in deps/*; do
	cd $dep
	./make.sh
	cd -
done

export CSC_OPTIONS="$CFLAGS $LDFLAGS"
$CSC -o LisP graphics.scm deps/*/*.o -uses sdl2 -uses sdl2-internals -uses sdl2-image -uses sdl2-mixer -uses miscmacros -uses utf8 -uses vector-lib -lSDL2 -lSDL2main -lSDL2_mixer -lSDL2_image
