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

;;; Poll handle
(defcenum uv_poll_event
  (:UV_READABLE 1)
  (:UV_WRITABLE  2)
  (:UV_DISCONNECT 4)
  (:UV_PRIORITIZED 8))

(def-foreign-call uv_poll_init ((event-loop (* uv_loop_t)) (handle (* uv_poll_t)) (fd :int))
  :returning :int)

(def-foreign-call uv_poll_init_socket ((event-loop (* uv_loop_t)) (handle (* uv_poll_t)) (socket uv_os_sock_t))
  :returning :int
  :pass-structs-by-value t)

(def-foreign-call uv_poll_start ((handle (* uv_poll_t)) (events :int) (cb :foreign-address))
  :returning :int)

(def-foreign-call uv_poll_stop ((handle (* uv_poll_t)))
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

;;; Process handle
(defcenum uv_process_flags
  (:UV_PROCESS_SETUID #.(ash 1 0))
  (:UV_PROCESS_SETGID #.(ash 1 1))
  (:UV_PROCESS_WINDOWS_VERBATIM_ARGUMENTS #.(ash 1 2))
  (:UV_PROCESS_DETACHED #.(ash 1 3))
  (:UV_PROCESS_WINDOWS_HIDE #.(ash 1 4))
  (:UV_PROCESS_WINDOWS_HIDE_CONSOLE #.(ash 1 5))
  (:UV_PROCESS_WINDOWS_HIDE_GUI #.(ash 1 6)))

(defcenum uv_stdio_flags
  (:UV_IGNORE        #x00)
  (:UV_CREATE_PIPE   #x01)
  (:UV_INHERIT_FD    #x02)
  (:UV_INHERIT_STREAM #x04)
  (:UV_READABLE_PIPE  #x10)
  (:UV_WRITABLE_PIPE  #x20)
  (:UV_NONBLOCK_PIPE  #x40))

(def-foreign-type uv_stdio_container_t
    (:struct
     (flags uv_stdio_flags)
     (data (:union (stream (* uv_stream_t))
                   (fd :int)))))

(def-foreign-type uv_process_options_t
    (:struct
     (exit_cb (* :void))
     (file (* :char))
     (args (:array (* :char)))
     (env (:array (* :char)))
     (cwd (* :char))
     (flags :unsigned-int)
     (stdio_count :int)
     (stdio (* uv_stdio_container_t))
     (uid uv_uid_t)
     (gid uv_gid_t)))

(def-foreign-call uv_disable_stdio_inheritance (:void)
  :returning :void)

(def-foreign-call uv_spawn ((event-loop (* uv_loop_t)) (handle (* uv_process_t)) (options (* uv_process_options_t)))
  :returning :int)

(def-foreign-call uv_process_kill ((handle (* uv_process_t)) (signum :int))
  :returning :int)

(def-foreign-call uv_kill ((pid :int) (signum :int))
  :returning :int)

(def-foreign-call uv_process_get_pid ((handle (* uv_process_t)))
  :returning uv_pid_t)

;;; Stream handle
(def-foreign-call uv_shutdown ((req (* uv_shutdown_t)) (handle (* uv_stream_t)) (cb :foreign-address))
  :returning :int)

(def-foreign-call uv_listen ((stream (* uv_stream_t)) (backlog :int) (cb :foreign-address))
  :returning :int)

(def-foreign-call uv_accept ((server (* uv_stream_t)) (client (* uv_stream_t)))
  :returning :int)

(def-foreign-call uv_read_start ((stream (* uv_stream_t)) (alloc_cb :foreign-address) (read_cb :foreign-address))
  :returning :int)

(def-foreign-call uv_read_stop ((handle (* uv_stream_t)))
  :returning :int)

(def-foreign-call uv_write ((req (* uv_write_t))
                            (handle (* uv_stream_t))
                            (bufs (:array uv_buf_t))
                            (nbufs :unsigned-int)
                            (cb :foreign-address))
  :returning :int)

(def-foreign-call uv_write2 ((req (* uv_write_t))
                             (handle (* uv_stream_t))
                             (bufs (:array uv_buf_t))
                             (nbufs :unsigned-int)
                             (send_handle (* uv_stream_t))
                             (cb :foreign-address))
  :returning :int)

(def-foreign-call uv_try_write ((handle (* uv_stream_t))
                                (bufs (:array uv_buf_t))
                                (nbufs :unsigned-int))
  :returning :int)

(def-foreign-call uv_try_write2 ((handle (* uv_stream_t))
                                 (bufs (:array uv_buf_t))
                                 (nbufs :unsigned-int)
                                 (send_handle (* uv_stream_t)))
  :returning :int)

(def-foreign-call uv_is_readable ((handle (* uv_stream_t)))
  :returning :int)

(def-foreign-call uv_is_writable ((handle (* uv_stream_t)))
  :returning :int)

(def-foreign-call uv_stream_set_blocking ((handle (* uv_stream_t)) (blocking :int))
  :returning :int)

(def-foreign-call uv_stream_get_write_queue_size ((stream (* uv_stream_t)))
  :returning size_t)

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
