#!/bin/sh

pushd ${1%/*}/ > /dev/null

EXE="$1-verifier.o"

spin -a $1
if [[ $? -ne 0 ]]; then
    popd > /dev/null
    exit 1
fi

gcc -o $EXE pan.c
$EXE -a -f -n -N $2
rm $EXE pan.*

popd > /dev/null
