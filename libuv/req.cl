;;;; req.cl
(in-package #:libuv)

;;; Base request
(def-foreign-call uv_cancel ((req (* uv_req_t)))
  :returning :int)

(def-foreign-call uv_req_get_data ((req (* uv_req_t)))
  :returning ((* :void)))

(def-foreign-call uv_req_set_data ((req (* uv_req_t)) (data (* :void)))
  :returning ((* :void)))

(def-foreign-call uv_req_get_type ((req (* uv_req_t)))
  :returning :int)

(def-foreign-call uv_req_type_name ((type :int fixnum))
  :returning ((* :char) simple-string)
  :strings-convert t)

(def-foreign-call uv_req_size ((type :int fixnum))
  :returning size_t)
