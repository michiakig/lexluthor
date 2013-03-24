#! /bin/bash

function smlnj {
    pushd tests
    ml-build all.cm Main.main tests
    if [ $? -eq 0 ]; then
        echo "*** build successful, running tests ***"
        sml @SMLload=tests.x86-darwin
        rm tests.x86-darwin
    fi
    popd
}

function mlton_build {
    mlton tests/all.mlb
    if [ $? -eq 0 ]; then
        echo "*** build successful, running tests ***"
        ./tests/all
        rm tests/all
    fi
}

if [[ $# -gt 1 && $1 = smlnj ]]; then
    smlnj
elif [[ $# -gt 1 && $1 = mlton ]]; then
    mlton_build
else
    smlnj
    mlton_build
fi
