#! /bin/bash

# compile and run Standard ML programs with all of MLton's profiling options

function prof {
    fname=$1
    bname=${1%%.*}

    mlton -output $bname.$2 -profile $2 $fname 
    if [ $? -eq 0 ]; then
        echo "program ===="
        ./$bname.$2
        echo "==== output"
        mlprof -raw true $bname.$2 mlmon.out 
    fi
}

source=$1
shift

while (( "$#" )); do
    prof $source $1
    shift
done
