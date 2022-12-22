;;;; error.cl
(in-package #:libuv)

(def-foreign-call uv_strerror ((err uv_error_t))
  :returning ((* :char) string)
  :strings-convert t)

(def-foreign-call uv_err_name ((err uv_error_t))
  :returning ((* :char) string)
  :strings-convert t)

(def-foreign-call uv_translate_sys_error ((sys_errno :int))
  :returning :int)
