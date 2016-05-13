#!/bin/sh

set -x -e

export CSC=csc
export CHICKEN_INSTALL=chicken-install

$CHICKEN_INSTALL -init $PWD/deps/.repository/
export CHICKEN_REPOSITORY=$PWD/deps/.repository/

export LDFLAGS="-Wl,-rpath -Wl,@loader_path/../Frameworks -framework SDL2 -framework SDL2_mixer -framework SDL2_image"
export CFLAGS=" \
  -I/Library/Frameworks/SDL2.framework/Headers \
  -I/Library/Frameworks/SDL2_mixer.framework/Headers \
  -I/Library/Frameworks/SDL2_image.framework/Headers \
  "

export CSC_OPTIONS="-v $CFLAGS"
for dep in deps/*; do
	cd $dep
	./make.sh
	cd -
done

export CSC_OPTIONS="$CFLAGS $LDFLAGS"
$CSC -o LisP graphics.scm deps/*/*.o -uses sdl2 -uses sdl2-internals -uses sdl2-image -uses sdl2-mixer -uses miscmacros -uses utf8 -uses vector-lib

rm -rf LisP.app LisP.iconset
cp -a osx-app-template LisP.app
cp -a /Library/Frameworks/SDL2{,_mixer,_image}.framework LisP.app/Contents/Frameworks/
cp LisP LisP.app/Contents/Resources/
cp -a assets LisP.app/Contents/Resources/

zip -ry9q LisP.app.zip LisP.app
