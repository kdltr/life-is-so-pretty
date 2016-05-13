#!/bin/sh

set -e -x

eggname=`basename $PWD`

$CSC $eggname.scm -c -J -unit $eggname -uses sdl2 -uses sdl2-internals -debug M
cp *.import.scm $CHICKEN_REPOSITORY
