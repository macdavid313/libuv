#include <stdio.h>
#include <uv.h>

#define TYPE_SIGNED_P(type) (((type)-1)<0LL)

void type_name(FILE *output, int signed_p, int size) {
  if (signed_p) {
    switch (size) {
    case 1: fprintf(output, ":char"); break;
    case 2: fprintf(output, ":short"); break;
    case 4: fprintf(output, ":int"); break;
    case 8: fprintf(output, ":long-long"); break;
    default: goto error;
    }
  } else {
    switch(size) {
    case 1: fprintf(output, ":unsigned-char"); break;
    case 2: fprintf(output, ":unsigned-short"); break;
    case 4: fprintf(output, ":unsigned-int"); break;
    case 8: fprintf(output, ":unsigned-long-long"); break;
    default: goto error;
    }
  }

  return;

 error:
  fprintf(output, "(error \"No type of size ~D.\" %i)\n", size);
}

int main(int argc, char** argv) {
  FILE *output = argc > 1 ? fopen(argv[1], "w") : stdout;

  fputs(";;;; uv-constants.cl\n", output);
  fputs(";;;; DO NOT MODIFY: this file has been automatically generated by uv_constants.c\n", output);
  fputs("(in-package #:libuv)\n\n", output);

  fputs(";;; Error constants\n", output);
  fputs(";;; http://docs.libuv.org/en/v1.x/errors.html#error-constants\n", output);
#define XX(name, _) fprintf(output, "\n  (:UV_%s %d)", #name, UV_##name);
  fputs("(def-foreign-enum uv_error_t", output);
  UV_ERRNO_MAP(XX);
  fputs(")\n", output);
#undef XX
  fputs("\n\n", output);

  fputs(";;; Handle types\n", output);
  fputs(";;; http://docs.libuv.org/en/v1.x/handle.html#c.uv_handle_type\n", output);
#define XX(name, _) fprintf(output, "\n  (:UV_%s %d)", #name, UV_##name);
  fputs("(def-foreign-enum uv_handle_type", output);
  fprintf(output, "\n  (:UV_UNKNOWN_HANDLE %d)", UV_UNKNOWN_HANDLE);
  UV_HANDLE_TYPE_MAP(XX);
  fprintf(output, "\n  (:UV_HANDLE_TYPE_MAX %d)", UV_HANDLE_TYPE_MAX);
  fputs(")\n", output);
#undef XX
#define XX(name, type) fprintf(output, "(def-foreign-type uv_%s_t (:array :unsigned-char %lu))\n", #type, uv_handle_size(UV_##name));
  UV_HANDLE_TYPE_MAP(XX);
#undef XX
  fputs("\n\n", output);

  fputs(";;; Req types\n", output);
  fputs(";;; http://docs.libuv.org/en/v1.x/request.html#c.uv_req_t.type\n", output);
#define XX(name, _) fprintf(output, "\n  (:UV_%s %d)", #name, UV_##name);
  fputs("(def-foreign-enum uv_req_type", output);
  fprintf(output, "\n  (:UV_UNKNOWN_REQ %d)", UV_UNKNOWN_REQ);
  UV_REQ_TYPE_MAP(XX);
  fprintf(output, "\n  (:UV_REQ_TYPE_MAX %d)", UV_REQ_TYPE_MAX);
  fputs(")\n", output);
#undef XX
#define XX(name, type) fprintf(output, "(def-foreign-type uv_%s_t (:array :unsigned-char %lu))\n", #type, uv_req_size(UV_##name));
  UV_REQ_TYPE_MAP(XX);
#undef XX
  fputs("\n\n", output);

  fputs(";;; Version-checking macros and functions\n", output);
  fprintf(output, "(defconstant +UV_VERSION_MAJOR+ %d)\n", UV_VERSION_MAJOR);
  fprintf(output, "(defconstant +UV_VERSION_MINOR+ %d)\n", UV_VERSION_MINOR);
  fprintf(output, "(defconstant +UV_VERSION_PATCH+ %d)\n", UV_VERSION_PATCH);
  fprintf(output, "(defconstant +UV_VERSION_IS_RELEASE+ %d)\n", UV_VERSION_IS_RELEASE);
  fprintf(output, "(defconstant +UV_VERSION_SUFFIX+ \"%s\")\n", UV_VERSION_SUFFIX);
  fprintf(output, "(defconstant +UV_VERSION_HEX+ %d)\n\n\n", UV_VERSION_HEX);

  fputs(";;; Shared library handling\n", output);
  fprintf(output, "(def-foreign-type uv_lib_t (:array :unsigned-char %lu))\n", sizeof(uv_lib_t));
  fputs("\n\n", output);

  fputs(";;; Threading and synchronization utilities\n", output);
  fprintf(output, "(def-foreign-type uv_thread_t (:array :unsigned-char %lu))\n", sizeof(uv_thread_t));
  fprintf(output, "(def-foreign-type uv_key_t (:array :unsigned-char %lu))\n", sizeof(uv_key_t));
  fprintf(output, "(def-foreign-type uv_once_t (:array :unsigned-char %lu))\n", sizeof(uv_once_t));
  fprintf(output, "(def-foreign-type uv_mutex_t (:array :unsigned-char %lu))\n", sizeof(uv_mutex_t));
  fprintf(output, "(def-foreign-type uv_rwlock_t (:array :unsigned-char %lu))\n", sizeof(uv_rwlock_t));
  fprintf(output, "(def-foreign-type uv_sem_t (:array :unsigned-char %lu))\n", sizeof(uv_sem_t));
  fprintf(output, "(def-foreign-type uv_cond_t (:array :unsigned-char %lu))\n", sizeof(uv_cond_t));
  fprintf(output, "(def-foreign-type uv_barrier_t (:array :unsigned-char %lu))\n", sizeof(uv_barrier_t));
  fputs("\n\n", output);

  fputs(";;; Types and other constants\n", output);
  fprintf(output, "(def-foreign-type uv_loop_t (:array :unsigned-char %lu))\n", uv_loop_size());
  fputs("(def-foreign-type uv_uid_t ", output);
  type_name(output, TYPE_SIGNED_P(uv_uid_t), sizeof(uv_uid_t));
  fputs(")\n", output);
  fputs("(def-foreign-type uv_gid_t ", output);
  type_name(output, TYPE_SIGNED_P(uv_gid_t), sizeof(uv_gid_t));
  fputs(")\n", output);
  fputs("(def-foreign-type uv_file ", output);
  type_name(output, TYPE_SIGNED_P(uv_file), sizeof(uv_file));
  fputs(")\n", output);
  fputs("(def-foreign-type uv_os_sock_t ", output);
  type_name(output, TYPE_SIGNED_P(uv_os_sock_t), sizeof(uv_os_sock_t));
  fputs(")\n", output);
  fputs("(def-foreign-type uv_os_fd_t ", output);
  type_name(output, TYPE_SIGNED_P(uv_os_fd_t), sizeof(uv_os_fd_t));
  fputs(")\n", output);
  fputs("(def-foreign-type uv_pid_t ", output);
  type_name(output, TYPE_SIGNED_P(uv_pid_t), sizeof(uv_pid_t));
  fputs(")\n", output);
  fprintf(output, "(defconstant +UV_IF_NAMESIZE+ %d)\n", UV_IF_NAMESIZE);
  fprintf(output, "(defconstant +UV_MAXHOSTNAMESIZE+ %d)\n", UV_MAXHOSTNAMESIZE);
}
