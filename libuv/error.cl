;;;; error.cl
(in-package #:libuv)

(def-foreign-call uv_strerror ((err :int))
  :returning ((* :char) string)
  :strings-convert t)

(def-foreign-call uv_err_name ((err :int))
  :returning ((* :char) string)
  :strings-convert t)

(def-foreign-call uv_translate_sys_error ((sys_errno))
  :returning :int)
