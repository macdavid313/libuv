;;;; event-loop.cl
(in-package #:libuv)

(def-foreign-call uv_loop_init ((loop (* uv_loop_t)))
  :returning :int)

(def-foreign-call uv_loop_close ((loop (* uv_loop_t)))
  :returning :int)

(def-foreign-call uv_default_loop (:void)
  :returning ((* uv_loop_t))
  :strings-convert nil)

(def-foreign-call uv_run ((loop (* uv_loop_t)) (uv_run_mode :int))
  :returning :int)

(def-foreign-call uv_loop_alive ((loop (* uv_loop_t)))
  :returning :int)

(def-foreign-call uv_stop ((loop (* uv_loop_t)))
  :returning :void)

(def-foreign-call uv_backend_fd ((loop (* uv_loop_t)))
  :returning :int)

(def-foreign-call uv_backend_timeout ((loop (* uv_loop_t)))
  :returning :int)

(def-foreign-call uv_now ((loop (* uv_loop_t)))
  :returning uint64_t)

(def-foreign-call uv_update_time ((loop (* uv_loop_t)))
  :returning :void)

(def-foreign-call uv_walk ((loop (* uv_loop_t)) (walk_cb :foreign-address) (arg (* :void)))
  :returning :void)

(def-foreign-call uv_loop_fork ((loop (* uv_loop_t)))
  :returning :int)

(def-foreign-call uv_loop_get_data ((loop (* uv_loop_t)))
  :returning ((* :void)))

(def-foreign-call uv_loop_set_data ((loop (* uv_loop_t)) (data (* :void)))
  :returning ((* :void)))
