#!/bin/sh

pushd ${1%/*}/

EXE="$1-verifier.o"

spin -a $1
gcc -o $EXE pan.c
$EXE -a -f -n -N $2
rm $EXE pan.c

popd
