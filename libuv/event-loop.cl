;;;; event-loop.cl
(in-package #:libuv)

(def-foreign-call uv_loop_init ((event-loop (* uv_loop_t)))
  :returning :int)

(def-foreign-call uv_loop_close ((event-loop (* uv_loop_t)))
  :returning :int)

(def-foreign-call uv_default_loop (:void)
  :returning ((* uv_loop_t))
  :strings-convert nil)

(def-foreign-call uv_run ((event-loop (* uv_loop_t)) (uv_run_mode :int))
  :returning :int)

(def-foreign-call uv_loop_alive ((event-loop (* uv_loop_t)))
  :returning :int)

(def-foreign-call uv_stop ((event-loop (* uv_loop_t)))
  :returning :void)

(def-foreign-call uv_backend_fd ((event-loop (* uv_loop_t)))
  :returning :int)

(def-foreign-call uv_backend_timeout ((event-loop (* uv_loop_t)))
  :returning :int)

(def-foreign-call uv_now ((event-loop (* uv_loop_t)))
  :returning uint64_t)

(def-foreign-call uv_update_time ((event-loop (* uv_loop_t)))
  :returning :void)

(def-foreign-call uv_walk ((event-loop (* uv_loop_t)) (walk_cb :foreign-address) (arg (* :void)))
  :returning :void)

(def-foreign-call uv_loop_fork ((event-loop (* uv_loop_t)))
  :returning :int)

(def-foreign-call uv_loop_get_data ((event-loop (* uv_loop_t)))
  :returning ((* :void)))

(def-foreign-call uv_loop_set_data ((event-loop (* uv_loop_t)) (data (* :void)))
  :returning ((* :void)))

;;; Metrics operations
(def-foreign-type uv_metrics_t
    (:struct
     (loop_count uint64_t)
     (events uint64_t)
     (events_waiting uint64_t)
     (reserved (:array uint64_t 13))))

(def-foreign-call uv_metrics_idle_time ((event-loop (* uv_loop_t)))
  :returning uint64_t)

(def-foreign-call uv_metrics_info ((event-loop (* uv_loop_t)) (metrics (* uv_metrics_t)))
  :returning :int)
