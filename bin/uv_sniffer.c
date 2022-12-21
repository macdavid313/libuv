#include <stdio.h>
#include <uv.h>

int main(int argc, char** argv) {
  puts(";;;; uv-constants.cl");
  puts(";;;; DO NOT MODIFY: this file has been automatically generated by uv_constants.c");
  puts("(in-package #:libuv)\n\n");

  puts(";;; uv_loop_t");
  printf("(def-foreign-type uv_loop_t (:array :unsigned-char %lu))\n", uv_loop_size());
  puts("\n");

  puts(";;; C Enum: uv_run_mode");
  puts(";;; http://docs.libuv.org/en/v1.x/loop.html#c.uv_run_mode");
  printf("(defconstant +UV_RUN_DEFAULT+ %d)\n", UV_RUN_DEFAULT);
  printf("(defconstant +UV_RUN_ONCE+ %d)\n", UV_RUN_ONCE);
  printf("(defconstant +UV_RUN_NOWAIT+ %d)\n", UV_RUN_NOWAIT);
  puts("\n");

  puts(";;; C Enum: uv_loop_option");
  printf("(defconstant +UV_LOOP_BLOCK_SIGNAL+ %d)\n", UV_LOOP_BLOCK_SIGNAL);
  printf("(defconstant +UV_METRICS_IDLE_TIME+ %d)\n", UV_METRICS_IDLE_TIME);
  puts("\n");

  puts(";;; Error constants");
  puts(";;; http://docs.libuv.org/en/v1.x/errors.html#error-constants");
#define XX(name, str) printf("(defconstant +UV_%s+ %d %s)\n", #name, UV_##name, #str);
  UV_ERRNO_MAP(XX);
#undef XX
  puts("\n");

  puts(";;; Handle types");
  puts(";;; http://docs.libuv.org/en/v1.x/handle.html#c.uv_handle_type");
  printf("(def-foreign-type uv_handle_t (:array :unsigned-char %lu))\n", sizeof(uv_handle_t));
  printf("(defconstant +UV_UNKNOWN_HANDLE+ %d)\n", UV_UNKNOWN_HANDLE);
#define XX(name, type) printf("(defconstant +UV_%s+ %d)\n", #name, UV_##name); \
  printf("(def-foreign-type uv_%s_t (:array :unsigned-char %lu))\n", #type, uv_handle_size(UV_##name));
  UV_HANDLE_TYPE_MAP(XX);
#undef XX
  printf("(defconstant +UV_HANDLE_TYPE_MAX+ %d)\n", UV_HANDLE_TYPE_MAX);
  puts("\n");

  puts(";;; Req types");
  puts(";;; http://docs.libuv.org/en/v1.x/request.html#c.uv_req_t.type");
  printf("(defconstant +UV_UNKNOWN_REQ+ %d)\n", UV_UNKNOWN_REQ);
#define XX(name, type) printf("(defconstant +UV_%s+ %d)\n", #name, UV_##name); \
  printf("(def-foreign-type uv_%s_t (:array :unsigned-char %lu))\n", #type, uv_req_size(UV_##name));
  UV_REQ_TYPE_MAP(XX);
#undef XX
  printf("(defconstant +UV_REQ_TYPE_MAX+ %d)\n", UV_REQ_TYPE_MAX);
  puts("");
}
