#!/bin/sh

set -e -x

eggname=`basename $PWD`

$CSC $eggname-internals.scm -c -J -unit $eggname-internals -debug M
$CSC $eggname.scm -c -J -unit $eggname -uses $eggname-internals -debug M
cp *.import.scm $CHICKEN_REPOSITORY
