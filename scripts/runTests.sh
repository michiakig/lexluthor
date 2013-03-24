#! /bin/bash

function smlnj {
    fullname=$1
    filename=$(basename "$fullname")
    dirname=$(dirname "$fullname")
    shortname="${filename%.*}"
    ml-build $fullname Main.main $shortname
    if [ $? -eq 0 ]; then
        echo "*** build successful, running tests ***"
        time sml @SMLload=$shortname.x86-darwin
        rm $shortname.x86-darwin
    fi
}

function mlton_build {
    fullname=$1
    filename=$(basename "$fullname")
    dirname=$(dirname "$fullname")
    shortname="${filename%.*}"
    mlton $fullname
    if [ $? -eq 0 ]; then
        echo "*** build successful, running tests ***"
        time ./$dirname/$shortname
        rm ./$dirname/$shortname
    fi
}

if [[ $# -gt 1 && $1 = smlnj ]]; then
    smlnj $2
elif [[ $# -gt 1 && $1 = mlton ]]; then
    mlton_build $2
fi
