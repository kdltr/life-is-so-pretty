#!/bin/sh

set -e -x

eggname=`basename $PWD`

$CSC $eggname-lolevel.scm -c -J -unit $eggname-lolevel -debug M
$CSC $eggname.scm -c -J -unit $eggname -uses $eggname-lolevel -uses regex -debug M

cp *.import.scm $CHICKEN_REPOSITORY
