#!/bin/sh

set -e

# OPERATION="cabal build --disable-tests"
# OPERATION="cabal build --enable-tests"
OPERATION="cabal test --enable-tests"

testbuild() {
    echo "===================================================================="
    echo "== ghc-$1 time-$2"
    echo "===================================================================="

	$OPERATION --builddir="dist-newstyle/ghc-$1-time-$2" --with-compiler="ghc-$1" --constraint="time == $2"
}

testbuild 8.10.2 1.11
testbuild 8.8.4  1.11
testbuild 8.6.5  1.11
testbuild 8.4.4  1.11
testbuild 8.2.2  1.11
testbuild 8.0.2  1.11

testbuild 8.10.2 1.10
testbuild 8.8.4  1.10
testbuild 8.6.5  1.10
testbuild 8.4.4  1.10
testbuild 8.2.2  1.10
testbuild 8.0.2  1.10

testbuild 8.10.2 1.9.3
testbuild 8.8.4  1.9.3
testbuild 8.6.5  1.9.3
testbuild 8.4.4  1.9.3
testbuild 8.2.2  1.9.3
testbuild 8.0.2  1.9.3
testbuild 7.10.3 1.9.3
testbuild 7.8.4  1.9.3

testbuild 8.10.2 1.9.2
testbuild 8.8.4  1.9.2
testbuild 8.6.5  1.9.2
testbuild 8.4.4  1.9.2
testbuild 8.2.2  1.9.2
testbuild 8.0.2  1.9.2
testbuild 7.10.3 1.9.2
testbuild 7.8.4  1.9.2

testbuild 8.6.5  1.8.0.3
testbuild 8.4.4  1.8.0.3
testbuild 8.2.2  1.8.0.3
testbuild 8.0.2  1.8.0.3
testbuild 7.10.3 1.8.0.3
testbuild 7.8.4  1.8.0.3

testbuild 8.6.5  1.6.0.1
testbuild 8.4.4  1.6.0.1
testbuild 8.2.2  1.6.0.1
testbuild 8.0.2  1.6.0.1
testbuild 7.10.3 1.6.0.1
testbuild 7.8.4  1.6.0.1

testbuild 8.6.5  1.5.0.1
testbuild 8.4.4  1.5.0.1
testbuild 8.2.2  1.5.0.1
testbuild 8.0.2  1.5.0.1
testbuild 7.10.3 1.5.0.1
testbuild 7.8.4  1.5.0.1
testbuild 7.6.3  1.5.0.1
testbuild 7.4.2  1.5.0.1

testbuild 7.8.4  1.4.2
testbuild 7.6.3  1.4.2
testbuild 7.4.2  1.4.2
