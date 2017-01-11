#!/bin/bash

#$1 executable to try the readwrite test with (should either be python or c++)
#$2 directory where to put tmp files


TESTDIR=$2/test_tmp

mkdir -p $TESTDIR

for f in "${BASH_SOURCE%/*}"/mnrl_files/*.mnrl; do
    TESTOUT=$TESTDIR/$(basename $f)
    
    $1 $f $TESTOUT
    TESTRES=$("${BASH_SOURCE%/*}"/same.sh $f $TESTOUT)
    
    printf "%s: %s\n" $(basename $f) $TESTRES
done