#!/usr/bin/env bash

# 1. build libuv

# 2. uv_sniffer
clang -o bin/uv_sniffer -I$HOME/.local/include -luv -L$HOME/.local/lib bin/uv_sniffer.c
./bin/uv_sniffer libuv/uv-constants.cl

# 3. compile, load, and combine fasls
${LISP} -q -W -L load.cl --kill
