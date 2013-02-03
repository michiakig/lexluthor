#! /bin/bash

ml-build tests.cm Main.main tests

if [ $? -eq 0 ]; then
    echo "*** build successful, running tests ***"
    sml @SMLload=tests.x86-darwin
    rm tests.x86-darwin
fi
