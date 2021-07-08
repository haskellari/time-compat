#!/bin/sh

set -e

STOREDIR=$(pwd)/dist-newstyle/store

if [ "$1" = "build" ]; then
	OPERATION="cabal --store-dir=$STOREDIR build --disable-tests"
elif [ "$1" = "buildtest" ]; then
	OPERATION="cabal --store-dir=$STOREDIR build --enable-tests"
else
	OPERATION="cabal --store-dir=$STOREDIR test --enable-tests"
fi

echo "===================================================================="
echo "== $OPERATION"
echo "===================================================================="

testbuild() {
    echo "===================================================================="
    echo "== ghc-$1 time-$2"
    echo "===================================================================="

	echo "$OPERATION" --builddir="dist-newstyle/ghc-$1-time-$2" --with-compiler="ghc-$1" --constraint="time == $2"
	$OPERATION --builddir="dist-newstyle/ghc-$1-time-$2" --with-compiler="ghc-$1" --constraint="time == $2"
}

testbuild 9.0.1  1.12
testbuild 8.10.4 1.12
testbuild 8.8.4  1.12

testbuild 9.0.1  1.11.1
testbuild 8.10.4 1.11.1
testbuild 8.8.4  1.11.1
testbuild 8.6.5  1.11.1
testbuild 8.4.4  1.11.1
testbuild 8.2.2  1.11.1
testbuild 8.0.2  1.11.1

testbuild 9.0.1  1.11
testbuild 8.10.4 1.11
testbuild 8.8.4  1.11
testbuild 8.6.5  1.11
testbuild 8.4.4  1.11
testbuild 8.2.2  1.11
testbuild 8.0.2  1.11

testbuild 9.0.1  1.10
testbuild 8.10.4 1.10
testbuild 8.8.4  1.10
testbuild 8.6.5  1.10
testbuild 8.4.4  1.10
testbuild 8.2.2  1.10
testbuild 8.0.2  1.10

testbuild 9.0.1  1.9.3
testbuild 8.10.4 1.9.3
testbuild 8.8.4  1.9.3
testbuild 8.6.5  1.9.3
testbuild 8.4.4  1.9.3
testbuild 8.2.2  1.9.3
testbuild 8.0.2  1.9.3
testbuild 7.10.3 1.9.3
testbuild 7.8.4  1.9.3

testbuild 8.10.4 1.9.2
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
