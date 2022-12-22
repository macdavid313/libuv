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

(def-foreign-call uv_send_buffer_size ((handle (* uv_handle_t)) (value (* :int)))
  :returning :int)

(def-foreign-call uv_recv_buffer_size ((handle (* uv_handle_t)) (value (* :int)))
  :returning :int)

(def-foreign-call uv_fileno ((handle (* uv_hanle_t)) (fd (* uv_os_fd_t)))
  :returning :int)

(def-foreign-call uv_handle_get_loop ((handle (* uv_handle_t)))
  :returning ((* uv_loop_t)))

(def-foreign-call uv_handle_get_data ((handle (* uv_handle_t)))
  :returning ((* :void)))

(def-foreign-call uv_handle_set_data ((handle (* uv_handle_t)) (data (* :void)))
  :returning ((* :void)))

(def-foreign-call uv_handle_get_type ((handle (* uv_handle_t)))
  :returning :int)

(def-foreign-call uv_handle_type_name ((type uv_handle_type))
  :returning ((* :char) string)
  :strings-convert t)

(def-foreign-call uv_handle_size ((type uv_handle_type))
  :returning size_t)

;;; Timer handle
(def-foreign-call uv_timer_init ((event-loop (* uv_loop_t)) (handle (* uv_timer_t)))
  :returning :int)

(def-foreign-call uv_timer_start ((handle (* uv_timer_t))
                                  (cb :foreign-address)
                                  (timeout uint64_t)
                                  (repeat uint64_t))
  :returning :int)

(def-foreign-call uv_timer_stop ((handle (* uv_timer_t)))
  :returning :int)

(def-foreign-call uv_timer_again ((handle (* uv_timer_t)))
  :returning :int)

(def-foreign-call uv_timer_set_repeat ((handle (* uv_timer_t)) (repeat uint64_t))
  :returning :void)

(def-foreign-call uv_timer_get_repeat ((handle (* uv_timer_t)))
  :returning uint64_t)

(def-foreign-call uv_timer_get_due_in ((handle (* uv_timer_t)))
  :returning uint64_t)

;;; Prepare handle
(def-foreign-call uv_prepare_init ((event-loop (* uv_loop_t)) (handle (* uv_prepare_t)))
  :returning :int)

(def-foreign-call uv_prepare_start ((handle (* uv_prepare_t)) (cb :foreign-address))
  :returning :int)

(def-foreign-call uv_prepare_stop ((handle (* uv_prepare_t)))
  :returning :int)

;;; Check handle
(def-foreign-call uv_check_init ((event-loop (* uv_loop_t)) (handle (* uv_check_t)))
  :returning :int)

(def-foreign-call uv_check_start ((handle (* uv_check_t)) (cb :foreign-address))
  :returning :int)

(def-foreign-call uv_check_stop ((handle (* uv_check_t)))
  :returning :int)

;;; Idle handle
(def-foreign-call uv_idle_init ((event-loop (* uv_loop_t)) (handle (* uv_idle_t)))
  :returning :int)

(def-foreign-call uv_idle_start ((handle (* uv_idle_t)) (cb :foreign-address))
  :returning :int)

(def-foreign-call uv_idle_stop ((handle (* uv_idle_t)))
  :returning :int)

;;; Async handle
(def-foreign-call uv_async_init ((event-loop (* uv_loop_t)) (handle (* uv_async_t)) (cb :foreign-address))
  :returning :int)

(def-foreign-call uv_async_send ((handle (* uv_async_t)))
  :returning :int)

;;; Signal handle
(def-foreign-call uv_signal_init ((event-loop (* uv_loop_t)) (handle (* uv_signal_t)))
  :returning :int)

(def-foreign-call uv_signal_start ((handle (* uv_signal_t)) (cb :foreign-address) (signum :int))
  :returning :int)

(def-foreign-call uv_signal_start_oneshot ((handle (* uv_signal_t)) (cb :foreign-address) (signum :int))
  :returning :int)

(def-foreign-call uv_signal_stop ((handle (* uv_signal_t)))
  :returning :int)

;;; FS Poll handle
(def-foreign-call uv_fs_poll_init ((event-loop (* uv_loop_t)) (handle (* uv_fs_poll_t)))
  :returning :int)

(def-foreign-call uv_fs_poll_start ((handle (* uv_fs_poll_t))
                                    (cb :foreign-address)
                                    (path (* :char) simple-string)
                                    (iterval :unsigned-int))
  :returning :int
  :strings-convert t)

(def-foreign-call uv_fs_poll_stop ((handle (* uv_fs_poll_t)))
  :returning :int)

(def-foreign-call uv_fs_poll_getpath ((handle (* uv_fs_poll_t))
                                      (buffer (* :char))
                                      (size (* size_t)))
  :returning :int
  :strings-convert nil)
