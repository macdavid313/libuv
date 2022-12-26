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

(def-foreign-call uv_req_type_name ((type uv_req_type fixnum))
  :returning ((* :char) simple-string)
  :strings-convert t)

(def-foreign-call uv_req_size ((type uv_req_type fixnum))
  :returning size_t)

;;; Thread pool work scheduling¶
(def-foreign-call uv_queue_work ((event-loop (* uv_loop_t))
                                 (req (* uv_work_t))
                                 (work_cb :foreign-address)
                                 (after_work_cb :foreign-address))
  :returning :int)

;;; DNS utility functions
(def-foreign-call uv_getaddrinfo ((event-loop (* uv_loop_t))
                                  (req (* uv_getaddrinfo_t))
                                  (getaddrinfo_cb :foreign-address)
                                  (node (* :char) simple-string)
                                  (server (* :char) simple-string)
                                  (hints (* addrinfo)))
  :returning :int
  :strings-convert t)

(def-foreign-call uv_freeaddrinfo ((ai (* addrinfo)))
  :returning :void)

(def-foreign-call uv_getnameinfo ((event-loop (* uv_loop_t))
                                  (req (* uv_getnameinfo_t))
                                  (getnameinfo_cb :foreign-address)
                                  (addr (* sockaddr))
                                  (flags :int))
  :returning :int)


;;; Shared library handling
(def-foreign-call uv_dlopen ((filename (* :char)) (lib (* uv_lib_t)))
  :returning :int
  :strings-convert t)

(def-foreign-call uv_dlclose ((lib (* uv_lib_t)))
  :returning :void
  :arg-checking nil
  :call-direct t)

(def-foreign-call uv_dlsym ((lib (* uv_lib_t)) (name (* :char)) (ptr (:array (* :void))))
  :returning :int
  :strings-convert t)

(def-foreign-call uv_dlerror ((lib (* uv_lib_t)))
  :returning ((* :char))
  :strings-convert t)
