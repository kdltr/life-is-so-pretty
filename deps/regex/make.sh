#!/bin/sh

set -e -x

eggname=`basename $PWD`

$CSC $eggname.scm -c -J -unit $eggname -debug M
cp *.import.scm $CHICKEN_REPOSITORY
