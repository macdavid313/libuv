#!/usr/bin/env bash

set -euxo pipefail

CFLAGS="-g -Wall -I${UV_PATH}/include"

# 2. uv-pickaxe
# FIXME: it only works on linux now
cc ${CFLAGS} -c tool/uv-pickaxe.c  -o tool/uv-pickaxe.o
cc tool/uv-pickaxe.o -L${UV_PATH}/lib -luv -o tool/uv-pickaxe
./tool/uv-pickaxe src/uv-constants.cl
rm tool/*.o

# 3. compile, load, and combine fasls
${LISP} -q -W -L load.cl --kill
