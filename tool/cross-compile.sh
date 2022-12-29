#!/usr/bin/env bash

set -euxo pipefail

cd /usr/local/src/libuv
mkdir -p build
cd build

CMAKE_COMMON_VARS="-DBUILD_TESTING=OFF \
    -DCMAKE_BUILD_TYPE=Release \
    -DCMAKE_INSTALL_PREFIX=/usr/local/libuv \
    -DBUILD_SHARED_LIBS=ON \
    -DCMAKE_INSTALL_LIBDIR=lib"

if [ "$BUILD_TARGET" == "x86_64-unknown-linux-gnu" ]; then
    CMAKE_EXTRA_VARS=
else
    CMAKE_EXTRA_VARS="-DCMAKE_SYSTEM_NAME=${CROSS_CMAKE_SYSTEM_NAME} \
        -DCMAKE_CRT=${CROSS_CMAKE_CRT} \
        -DCMAKE_SYSTEM_PROCESSOR=${CROSS_CMAKE_SYSTEM_PROCESSOR} \
        -DCMAKE_TOOLCHAIN_FILE=/opt/toolchain.cmake"
fi

if [ "$BUILD_TARGET" == "i686-*" ]; then
    CMAKE_EXTRA_VARS="-DCMAKE_C_FLAGS=-m32 \
        $CMAKE_EXTRA_VARS"
fi

cmake ../ ${CMAKE_COMMON_VARS} ${CMAKE_EXTRA_VARS}
cmake --build .
cmake --install .
