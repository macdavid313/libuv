#!/usr/bin/env bash

# 1. build libuv

# 2. uv-pickaxe
clang -o tool/uv-pickaxe -I$HOME/.local/include -luv -L$HOME/.local/lib tool/uv-pickaxe.c
./tool/uv-pickaxe src/uv-constants.cl

# 3. compile, load, and combine fasls
${LISP} -q -W -L load.cl --kill
