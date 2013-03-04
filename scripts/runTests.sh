#! /bin/bash

function smlnj {
    pushd tests
    ml-build tests.cm Main.main tests
    if [ $? -eq 0 ]; then
        echo "*** build successful, running tests ***"
        sml @SMLload=tests.x86-darwin
        rm tests.x86-darwin
    fi
    popd
}

function mlton_build {
    mlton lexluthor.mlb
    if [ $? -eq 0 ]; then
        echo "*** build successful, running tests ***"
        ./lexluthor
        rm lexluthor
    fi
}

if [ $1 = smlnj ]; then
    smlnj
elif [ $1 = mlton ]; then
    mlton_build
else
    smlnj
    mlton_build
fi
