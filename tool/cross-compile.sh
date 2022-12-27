#!/usr/bin/env bash

set -euxo pipefail

cd /usr/local/src/libuv
mkdir -p build
cd build

if [ "$BUILD_TARGET" == "x86_64-unknown-linux-gnu" ]; then
    CMAKE_EXTRA_VARS=
else
    CMAKE_EXTRA_VARS="-DCMAKE_SYSTEM_NAME=${CROSS_CMAKE_SYSTEM_NAME} \
        -DCMAKE_CRT=${CROSS_CMAKE_CRT} \
        -DCMAKE_SYSTEM_PROCESSOR=${CROSS_CMAKE_SYSTEM_PROCESSOR} \
        -DCMAKE_TOOLCHAIN_FILE=/opt/toolchain.cmake"
fi

cmake ../ -DBUILD_TESTING=OFF \
    -DCMAKE_BUILD_TYPE=Release \
    -DCMAKE_INSTALL_PREFIX=/usr/local/libuv \
    -DBUILD_SHARED_LIBS=ON \
    ${CMAKE_EXTRA_VARS}

cmake --build .
cmake --install .

cd /usr/local/src
cc -o uv-pickaxe \
    -I/usr/local/libuv/include \
    -luv -L/usr/local/libuv/lib -L/usr/local/libuv/lib64 \
    uv-pickaxe.c

[ -z ${LD_LIBRARY_PATH+x} ] && export LD_LIBRARY_PATH=
export LD_LIBRARY_PATH=/usr/local/libuv/lib:/usr/local/libuv/lib64:$LD_LIBRARY_PATH
./uv-pickaxe > /usr/local/libuv/share/uv-constants.cl
