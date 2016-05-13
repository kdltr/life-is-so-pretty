#!/bin/sh

set -e -x

eggname=`basename $PWD`

$CSC $eggname.scm -c -J -unit $eggname
cp *.import.scm $CHICKEN_REPOSITORY
