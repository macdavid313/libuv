;;;; handle.cl
(in-package #:libuv)

;;; Base handle
(def-foreign-call uv_is_active ((handle (* uv_handle_t)))
  :returning :int)

(def-foreign-call uv_is_closing ((handle (* uv_handle_t)))
  :returning :int)

(def-foreign-call uv_close ((handle (* uv_handle_t)))
  :returning :void)

(def-foreign-call uv_ref ((handle (* uv_handle_t)))
  :returning :void)

(def-foreign-call uv_unref ((handle (* uv_handle_t)))
  :returning :void)

(def-foreign-call uv_has_ref ((handle (* uv_handle_t)))
  :returning :int)

(def-foreign-call uv_handle_get_loop ((handle (* uv_handle_t)))
  :returning ((* uv_loop_t)))

(def-foreign-call uv_handle_get_data ((handle (* uv_handle_t)))
  :returning ((* :void)))

(def-foreign-call uv_handle_set_data ((handle (* uv_handle_t)) (data (* :void)))
  :returning ((* :void)))

(def-foreign-call uv_handle_get_type ((handle (* uv_handle_t)))
  :returning :int)

(def-foreign-call uv_handle_type_name ((type :int))
  :returning ((* :char) string)
  :strings-convert t)

;;; Idle handle
(def-foreign-call uv_idle_init ((loop (* uv_loop_t)) (idle (* uv_idle_t)))
  :returning :int)

(def-foreign-call uv_idle_start ((idle (* uv_idle_t)) (cb :foreign-address))
  :returning :int)

(def-foreign-call uv_idle_stop ((idle (* uv_idle_t)))
  :returning :int)
