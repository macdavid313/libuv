;;;; libuv.cl
(in-package #:libuv)

;;; Error handling
;; Error constants generated in uv-constants.cl
(def-foreign-call uv_strerror ((err uv_error_t))
  :returning ((* :char) string)
  :strings-convert t)

(def-foreign-call uv_err_name ((err uv_error_t))
  :returning ((* :char) string)
  :strings-convert t)

(def-foreign-call uv_translate_sys_error ((sys_errno :int))
  :returning :int)

;;; Version-checking macros and functions
;; TODO: add constants
(ff:def-foreign-call uv_version (:void)
  :returning :unsigned-int)

(ff:def-foreign-call uv_version_string (:void)
  :returning ((* :char) simple-string)
  :strings-convert t
  :documentation "Returns the libuv version number as a string. For non-release versions the version suffix is included.")

;;; uv_loop_t — Event loop
(def-foreign-enum uv_loop_option
  (:UV_LOOP_BLOCK_SIGNAL 0)
  (:UV_METRICS_IDLE_TIME 1))

(def-foreign-enum uv_run_mode
  (:UV_RUN_DEFAULT 0)
  (:UV_RUN_ONCE 1)
  (:UV_RUN_NOWAIT 2))

(def-foreign-call uv_loop_init ((event-loop (* uv_loop_t)))
  :returning :int)

(def-foreign-call uv_loop_close ((event-loop (* uv_loop_t)))
  :returning :int)

(def-foreign-call uv_default_loop (:void)
  :returning ((* uv_loop_t))
  :strings-convert nil)

(def-foreign-call uv_run ((event-loop (* uv_loop_t)) (mode uv_run_mode))
  :returning :int)

(def-foreign-call uv_loop_alive ((event-loop (* uv_loop_t)))
  :returning :int)

(def-foreign-call uv_stop ((event-loop (* uv_loop_t)))
  :returning :void)

(def-foreign-call uv_loop_size (:void)
  :returning size_t)

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

;;; uv_handle_t — Base handle
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

;;; uv_req_t — Base request
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

;;; uv_timer_t — Timer handle
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

;;; uv_prepare_t — Prepare handle
(def-foreign-call uv_prepare_init ((event-loop (* uv_loop_t)) (handle (* uv_prepare_t)))
  :returning :int)

(def-foreign-call uv_prepare_start ((handle (* uv_prepare_t)) (cb :foreign-address))
  :returning :int)

(def-foreign-call uv_prepare_stop ((handle (* uv_prepare_t)))
  :returning :int)

;;; uv_check_t — Check handle
(def-foreign-call uv_check_init ((event-loop (* uv_loop_t)) (handle (* uv_check_t)))
  :returning :int)

(def-foreign-call uv_check_start ((handle (* uv_check_t)) (cb :foreign-address))
  :returning :int)

(def-foreign-call uv_check_stop ((handle (* uv_check_t)))
  :returning :int)

;;; uv_idle_t — Idle handle
(def-foreign-call uv_idle_init ((event-loop (* uv_loop_t)) (handle (* uv_idle_t)))
  :returning :int)

(def-foreign-call uv_idle_start ((handle (* uv_idle_t)) (cb :foreign-address))
  :returning :int)

(def-foreign-call uv_idle_stop ((handle (* uv_idle_t)))
  :returning :int)

;;; uv_async_t — Async handle
(def-foreign-call uv_async_init ((event-loop (* uv_loop_t)) (handle (* uv_async_t)) (cb :foreign-address))
  :returning :int)

(def-foreign-call uv_async_send ((handle (* uv_async_t)))
  :returning :int)

;;; uv_poll_t — Poll handle
(def-foreign-enum uv_poll_event
  (:UV_READABLE    1)
  (:UV_WRITABLE    2)
  (:UV_DISCONNECT  4)
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

;;; uv_signal_t — Signal handle
(def-foreign-call uv_signal_init ((event-loop (* uv_loop_t)) (handle (* uv_signal_t)))
  :returning :int)

(def-foreign-call uv_signal_start ((handle (* uv_signal_t)) (cb :foreign-address) (signum :int))
  :returning :int)

(def-foreign-call uv_signal_start_oneshot ((handle (* uv_signal_t)) (cb :foreign-address) (signum :int))
  :returning :int)

(def-foreign-call uv_signal_stop ((handle (* uv_signal_t)))
  :returning :int)

;;; uv_process_t — Process handle
(def-foreign-enum uv_process_flags
  (:UV_PROCESS_SETUID                     #.(ash 1 0))
  (:UV_PROCESS_SETGID                     #.(ash 1 1))
  (:UV_PROCESS_WINDOWS_VERBATIM_ARGUMENTS #.(ash 1 2))
  (:UV_PROCESS_DETACHED                   #.(ash 1 3))
  (:UV_PROCESS_WINDOWS_HIDE               #.(ash 1 4))
  (:UV_PROCESS_WINDOWS_HIDE_CONSOLE       #.(ash 1 5))
  (:UV_PROCESS_WINDOWS_HIDE_GUI           #.(ash 1 6)))

(def-foreign-enum uv_stdio_flags
  (:UV_IGNORE         #x00)
  (:UV_CREATE_PIPE    #x01)
  (:UV_INHERIT_FD     #x02)
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

;;; uv_stream_t — Stream handle
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

;;; uv_tcp_t — TCP handle
(def-foreign-call uv_tcp_init ((event-loop (* uv_loop_t)) (handle (* uv_tcp_t)))
  :returning :int)

(def-foreign-call uv_tcp_init_ex ((event-loop (* uv_loop_t)) (handle (* uv_tcp_t)) (flags :unsigned-int))
  :returning :int)

(def-foreign-call uv_tcp_open ((handle (* uv_tcp_t)) (sock uv_os_sock_t))
  :returning :int)

(def-foreign-call uv_tcp_nodelay ((handle (* uv_tcp_t)) (enable :int))
  :returning :int)

(def-foreign-call uv_tcp_keepalive ((handle (* uv_tcp_t)) (enable :int) (delay :unsigned-int))
  :returning :int)

(def-foreign-call uv_tcp_simultaneous_accepts ((handle (* uv_tcp_t)) (enable :int))
  :returning :int)

(def-foreign-call uv_tcp_bind ((handle (* uv_tcp_t)) (addr (* sockaddr)) (flags :unsigned-int))
  :returning :int)

(def-foreign-call uv_tcp_getsockname ((handle (* uv_tcp_t)) (name (* sockaddr)) (namelen (* :int)))
  :returning :int)

(def-foreign-call uv_tcp_getpeername ((handle (* uv_tcp_t)) (name (* sockaddr)) (namelen (* :int)))
  :returning :int)

(def-foreign-call uv_tcp_connect ((handle (* uv_tcp_t)) (addr (* sockaddr)) (cb :foreign-address))
  :returning :int)

(def-foreign-call uv_tcp_close_reset ((handle (* uv_tcp_t)) (cb :foreign-address))
  :returning :int)

(def-foreign-call uv_socketpair ((type :int)
                                 (protocol :int)
                                 (socket_vector (:array uv_os_sock_t 2))
                                 (flags0 :int)
                                 (flags1 :int))
  :returning :int)

;;; uv_pipe_t — Pipe handle
(def-foreign-call uv_pipe_init ((event-loop (* uv_loop_t)) (handle (* uv_pipe_t)) (ipc :int))
  :returning :int)

(def-foreign-call uv_pipe_open ((handle (* uv_pipe_t)) (file uv_file))
  :returning :int)

(def-foreign-call uv_pipe_bind ((handle (* uv_pipe_t)) (name (* :char)))
  :returning :int
  :strings-convert t)

(def-foreign-call uv_pipe_connect ((req (* uv_connect_t)) (handle (* uv_pipe_t)) (name (* :char)) (cb :foreign-address))
  :returning :void
  :strings-convert t)

(def-foreign-call uv_pipe_getsockname ((handle (* uv_pipe_t)) (buffer (* :char)) (size (* size_t)))
  :returning :int
  :strings-convert nil)

(def-foreign-call uv_pipe_getpeername ((handle (* uv_pipe_t)) (buffer (* :char)) (size (* size_t)))
  :returning :int
  :strings-convert nil)

#+windows
(def-foreign-call uv_pipe_pending_instances ((handle (* uv_pipe_t)) (count :int))
  :returning :void)

(def-foreign-call uv_pipe_pending_count ((handle (* uv_pipe_t)))
  :returning :int)

(def-foreign-call uv_pipe_chmod ((handle (* uv_pipe_t)) (flags :int))
  :returning :int)

(def-foreign-call uv_pipe ((fds (:array uv_file 2)) (read_flags :int) (write_flags :int))
  :returning :int)

;;; uv_tty_t — TTY handle
(def-foreign-enum uv_tty_mode_t
  (:UV_TTY_MODE_NORMAL 0)               ; Initial/normal terminal mode
  (:UV_TTY_MODE_RAW    1) ; Raw input mode (On Windows, ENABLE_WINDOW_INPUT is also enabled)
  (:UV_TTY_MODE_IO     2) ; Binary-safe I/O mode for IPC (Unix-only)
  )

(def-foreign-enum uv_tty_vtermstate_t
  ;; The console supports handling of virtual terminal sequences
  ;; (Windows10 new console, ConEmu)
  (:UV_TTY_SUPPORTED 0)
  ;; The console cannot process virtual terminal sequences.  (Legacy console)
  (:UV_TTY_UNSUPPORTED 1))

(def-foreign-call uv_tty_init ((event-loop (* uv_loop_t)) (handle (* uv_tty_t)) (fd uv_file) (unused :int))
  :returning :int)

(def-foreign-call uv_tty_set_mode ((handle (* uv_tty_t)) (mode uv_tty_mode_t))
  :returning :int)

(def-foreign-call uv_tty_reset_mode (:void)
  :returning :int)

(def-foreign-call uv_tty_get_winsize ((handle (* uv_tty_t)) (width (* :int)) (height (* :int)))
  :returning :int)

(def-foreign-call uv_tty_set_vterm_state ((state uv_tty_vtermstate_t))
  :returning :int)

(def-foreign-call uv_tty_get_vterm_state ((state (* uv_tty_vtermstate_t)))
  :returning :int)

;;; uv_udp_t — UDP handle
(def-foreign-enum uv_udp_flags
  (:UV_UDP_IPV6ONLY      1)
  (:UV_UDP_PARTIAL       2)
  (:UV_UDP_REUSEADDR     4)
  (:UV_UDP_MMSG_CHUNK    8)
  (:UV_UDP_MMSG_FREE     16)
  (:UV_UDP_LINUX_RECVERR 32)
  (:UV_UDP_RECVMMSG      256))

(def-foreign-enum uv_membership
  (:UV_LEAVE_GROUP 0)
  (:UV_JOIN_GROUP  1))

(def-foreign-call uv_udp_init ((event-loop (* uv_loop_t)) (handle (* uv_udp_t)))
  :returning :int)

(def-foreign-call uv_udp_init_ex ((event-loop (* uv_loop_t)) (handle (* uv_udp_t)) (flags :unsigned-int))
  :returning :int)

(def-foreign-call uv_udp_open ((handle (* uv_udp_t)) (sock uv_os_sock_t))
  :returning :int)

(def-foreign-call uv_udp_bind ((handle (* uv_udp_t)) (addr (* sockaddr)) (flags :unsigned-int))
  :returning :int)

(def-foreign-call uv_udp_connect ((handle (* uv_udp_t)) (addr (* sockaddr)))
  :returning :int)

(def-foreign-call uv_udp_getpeername ((handle (* uv_udp_t)) (name (* sockaddr)) (namelen (* :int)))
  :returning :int)

(def-foreign-call uv_udp_getsockname ((handle (* uv_udp_t)) (name (* sockaddr)) (namelen (* :int)))
  :returning :int)

(def-foreign-call uv_udp_set_membership ((handle (* uv_udp_t)) (multicast_addr (* :char)) (interface_addr (* :char)) (membership uv_membership))
  :returning :int
  :strings-convert t)

(def-foreign-call uv_udp_set_source_membership ((handle (* uv_udp_t))
                                                (multicast_addr (* :char))
                                                (interface_addr (* :char))
                                                (source_addr (* :char))
                                                (membership uv_membership))
  :returning :int
  :strings-convert t)

(def-foreign-call uv_udp_set_multicast_loop ((handle (* uv_udp_t)) (on :int))
  :returning :int)

(def-foreign-call uv_udp_set_multicast_ttl ((handle (* uv_udp_t)) (ttl :int))
  :returning :int)

(def-foreign-call uv_udp_set_multicast_interface ((handle (* uv_udp_t)) (interface_addr (* :char)))
  :returning :int
  :strings-convert t)

(def-foreign-call uv_udp_set_broadcast ((handle (* uv_udp_t)) (on :int))
  :returning :int)

(def-foreign-call uv_udp_set_ttl ((handle (* uv_udp_t)) (ttl :int))
  :returning :int)

(def-foreign-call uv_udp_send ((req (* uv_udp_send_t))
                               (handle (* uv_udp_t))
                               (bufs (:array uv_buf_t))
                               (nbufs :unsigned-int)
                               (addr (* sockaddr))
                               (send_cb :foreign-address))
  :returning :int)

(def-foreign-call uv_udp_try_send ((handle (* uv_udp_t))
                                   (bufs (:array uv_buf_t))
                                   (nbufs :unsigned-int)
                                   (addr (* sockaddr)))
  :returning :int)

(def-foreign-call uv_udp_recv_start ((handle (* uv_udp_t)) (alloc_cb :foreign-address) (recv_cb :foreign-address))
  :returning :int)

(def-foreign-call uv_udp_using_recvmmsg ((handle (* uv_udp_t)))
  :returning :int)

(def-foreign-call uv_udp_recv_stop ((handle (* uv_udp_t)))
  :returning :int)

(def-foreign-call uv_udp_get_send_queue_size ((handle (* uv_udp_t)))
  :returning size_t)

(def-foreign-call uv_udp_get_send_queue_count ((handle (* uv_udp_t)))
  :returning size_t)

;;; uv_fs_event_t — FS Event handle
(def-foreign-enum uv_fs_event
  (:UV_RENAME 1)
  (:UV_CHANGE 2))

(def-foreign-enum uv_fs_event_flags
  (:UV_FS_EVENT_WATCH_ENTRY 1)
  (:UV_FS_EVENT_STAT        2)
  (:UV_FS_EVENT_RECURSIVE   4))

(def-foreign-call uv_fs_event_init ((event-loop (* uv_loop_t)) (handle (* uv_fs_event_t)))
  :returning :int)

(def-foreign-call uv_fs_event_start ((handle (* uv_fs_event_t))
                                     (cb :foreign-address)
                                     (path (* :char) simple-string)
                                     (flags :unsigned-int))
  :returning :int
  :strings-convert t)

(def-foreign-call uv_fs_event_stop ((handle (* uv_fs_event_t)))
  :returning :int)

(def-foreign-call uv_fs_event_getpath ((handle (* uv_fs_event_t)) (buffer (* :char)) (size (* szie_t)))
  :returning :int
  :strings-convert nil)

;;; uv_fs_poll_t — FS Poll handle
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

;;; File system operations
;;; TODO

;;; Thread pool work scheduling
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

;;; Threading and synchronization utilities
(def-foreign-type uv_thread_options_t
    ;; http://docs.libuv.org/en/v1.x/threading.html#c.uv_thread_options_t
    (:struct
     (flags :int)
     (stack_size size_t)))

(def-foreign-call uv_thread_create ((tid (* uv_thread_t)) (entry :foreign-address) (arg (* :void)))
  :returning :int
  :arg-checking nil
  :call-direct t)

(def-foreign-call uv_thread_create_ex ((tid (* uv_thread_t)) (params (* uv_thread_options_t)) (entry :foreign-address) (arg (* :void)))
  :returning :int
  :arg-checking nil
  :call-direct t)

(def-foreign-call uv_thread_setaffinity ((tid (* uv_thread_t)) (cpumask (* :char)) (oldmask (* :char)) (mask_size size_t))
  :returning :int
  :strings-convert nil
  :arg-checking nil
  :call-direct t)

(def-foreign-call uv_thread_getaffinity ((tid (* uv_thread_t)) (cpumask (* :char)) (mask_size size_t))
  :returning :int
  :strings-convert nil
  :arg-checking nil
  :call-direct t)

(def-foreign-call uv_thread_getcpu (:void)
  :returning :int
  :strings-convert nil
  :arg-checking nil
  :call-direct t)

(def-foreign-call uv_thread_self (:void)
  :returning uv_thread_t
  :pass-structs-by-value t
  :arg-checking nil
  :call-direct t)

(def-foreign-call uv_thread_join ((tid (* uv_thread_t)))
  :returning :int
  :arg-checking nil
  :call-direct t)

(def-foreign-call uv_thread_equal ((t1 (* uv_thread_t)) (t2 (* uv_thread_t)))
  :returning :int
  :arg-checking nil
  :call-direct t)

(def-foreign-call uv_key_create ((key (* uv_key_t)))
  :returning :int
  :arg-checking nil
  :call-direct t)

(def-foreign-call uv_key_delete ((key (* uv_key_t)))
  :returning :void
  :arg-checking nil
  :call-direct t)

(def-foreign-call uv_key_get ((key (* uv_key_t)))
  :returning ((* :void))
  :arg-checking nil
  :call-direct t)

(def-foreign-call uv_key_set ((key (* uv_key_t)))
  :returning :void
  :arg-checking nil
  :call-direct t)

(def-foreign-call uv_once ((guard (* uv_once_t)) (cb :foreign-address))
  :returning :void
  :arg-checking nil
  :call-direct t)

(def-foreign-call uv_mutex_init ((handle (* uv_mutex_t)))
  :returning :int
  :arg-checking nil
  :call-direct t)

(def-foreign-call uv_mutex_init_recursive ((handle (* uv_mutex_t)))
  :returning :int
  :arg-checking nil
  :call-direct t)

(def-foreign-call uv_mutex_destroy ((handle (* uv_mutex_t)))
  :returning :void
  :arg-checking nil
  :call-direct t)

(def-foreign-call uv_mutex_lock ((handle (* uv_mutex_t)))
  :returning :void
  :arg-checking nil
  :call-direct t)

(def-foreign-call uv_mutex_trylock ((handle (* uv_mutex_t)))
  :returning :int
  :arg-checking nil
  :call-direct t)

(def-foreign-call uv_mutex_unlock ((handle (* uv_mutex_t)))
  :returning :void
  :arg-checking nil
  :call-direct t)

(def-foreign-call uv_rwlock_init ((rwlock (* uv_rwlock_t)))
  :returning :int
  :arg-checking nil
  :call-direct t)

(def-foreign-call uv_rwlock_destroy ((rwlock (* uv_rwlock_t)))
  :returning :void
  :arg-checking nil
  :call-direct t)

(def-foreign-call uv_rwlock_rdlock ((rwlock (* uv_rwlock_t)))
  :returning :void
  :arg-checking nil
  :call-direct t)

(def-foreign-call uv_rwlock_tryrdlock ((rwlock (* uv_rwlock_t)))
  :returning :int
  :arg-checking nil
  :call-direct t)

(def-foreign-call uv_rwlock_rdunlock ((rwlock (* uv_rwlock_t)))
  :returning :void
  :arg-checking nil
  :call-direct t)

(def-foreign-call uv_rwlock_wrlock ((rwlock (* uv_rwlock_t)))
  :returning :void
  :arg-checking nil
  :call-direct t)

(def-foreign-call uv_rwlock_trywrlock ((rwlock (* uv_rwlock_t)))
  :returning :int
  :arg-checking nil
  :call-direct t)

(def-foreign-call uv_rwlock_wrunlock ((rwlock (* uv_rwlock_t)))
  :returning :void
  :arg-checking nil
  :call-direct t)

(def-foreign-call uv_sem_init ((sem (* uv_sem_t)) (value :unsigned-int))
  :returning :int
  :arg-checking nil
  :call-direct t)

(def-foreign-call uv_sem_destroy ((sem (* uv_sem_t)))
  :returning :void
  :arg-checking nil
  :call-direct t)

(def-foreign-call uv_sem_post ((sem (* uv_sem_t)))
  :returning :void
  :arg-checking nil
  :call-direct t)

(def-foreign-call uv_sem_wait ((sem (* uv_sem_t)))
  :returning :void
  :arg-checking nil
  :call-direct t)

(def-foreign-call uv_sem_trywait ((sem (* uv_sem_t)))
  :returning :int
  :arg-checking nil
  :call-direct t)

(def-foreign-call uv_cond_init ((condition (* uv_cond_t)))
  :returning :int
  :arg-checking nil
  :call-direct t)

(def-foreign-call uv_cond_destroy ((condition (* uv_cond_t)))
  :returning :void
  :arg-checking nil
  :call-direct t)

(def-foreign-call uv_cond_signal ((condition (* uv_cond_t)))
  :returning :void
  :arg-checking nil
  :call-direct t)

(def-foreign-call uv_cond_broadcast ((condition (* uv_cond_t)))
  :returning :void
  :arg-checking nil
  :call-direct t)

(def-foreign-call uv_cond_wait ((condition (* uv_cond_t)) (mutex (* uv_mutex_t)))
  :returning :void
  :arg-checking nil
  :call-direct t)

(def-foreign-call uv_cond_timedwait ((condition (* uv_cond_t)) (mutex (* uv_mutex_t)) (timeout uint64_t))
  :returning :int
  :arg-checking nil
  :call-direct t)

(def-foreign-call uv_barrier_init ((barrier (* uv_barrier_t)) (count :unsigned-int))
  :returning :int
  :arg-checking nil
  :call-direct t)

(def-foreign-call uv_barrier_destroy ((barrier (* uv_barrier_t)))
  :returning :void
  :arg-checking nil
  :call-direct t)

(def-foreign-call uv_barrier_wait ((barrier (* uv_barrier_t)))
  :returning :int
  :arg-checking nil
  :call-direct t)

;;; Miscellaneous utilities
;; Data types
;; uv_buf_t defined in ctype.cl
;; uv_file generated in uv-constants.cl
;; uv_os_sock_t generated in uv-constants.cl
;; uv_os_fd_t generated in uv-constants.cl
;; uv_pid_t generated in uv-constants.cl

(def-foreign-type uv_timeval_t
    (:struct
     (tv_sec :long)
     (tv_usec :long)))

(def-foreign-type uv_timeval64_t
    (:struct
     (tv_sec int64_t)
     (tv_usec int32_t)))

(def-foreign-type uv_rusage_t
    (:struct
     (ru_utime uv_timeval_t)            ; user CPU time used
     (ru_stime uv_timeval_t)            ; system CPU time used
     (ru_maxrss uint64_t)               ; maximum resident set size
     (ru_ixrss uint64_t)                ; integral shared memory size (X)
     (ru_idrss uint64_t)                ;  integral unshared data size (X)
     (ru_isrss uint64_t)                ;  integral unshared stack size (X)
     (ru_minflt uint64_t)               ;  page reclaims (soft page faults) (X)
     (ru_majflt uint64_t)               ;  page faults (hard page faults)
     (ru_nswap uint64_t)                ;  swaps (X)
     (ru_inblock uint64_t)              ;  block input operations
     (ru_oublock uint64_t)              ;  block output operations
     (ru_msgsnd uint64_t)               ;  IPC messages sent (X)
     (ru_msgrcv uint64_t)               ;  IPC messages received (X)
     (ru_nsignals uint64_t)             ;  signals received (X)
     (ru_nvcsw uint64_t)                ;  voluntary context switches (X)
     (ru_nivcsw uint64_t)               ;  involuntary context switches (X)
     ))

(def-foreign-type uv_cpu_info_t
    (:struct
     (model (* :char))
     (speed :int)
     (cpu_times (:struct
                 (user uint64_t)
                 (nice uint64_t)
                 (sys uint64_t)
                 (idle uint64_t)
                 (irq uint64_t)))))

(def-foreign-type uv_interface_address_t
    (:struct
     (name (* :char))
     (phys_addr (:array :char 6) simple-string)
     (is_internal :int)
     (address (:union (address4 sockaddr_in)
                      (address6 sockaddr_in6)))
     (netmask (:union (netmask4 sockaddr_in)
                      (netmask6 sockaddr_in6)))))

(def-foreign-type uv_passwd_t
    (:struct
     (username (* :char))
     (uid :long)
     (gid :long)
     (shell (* :char))
     (homedir (* :char))))

(def-foreign-type uv_utsname_t
    (:struct
     (sysname (:array :char 256) simple-string)
     (release (:array :char 256) simple-string)
     (version (:array :char 256) simple-string)
     (machine (:array :char 256) simple-string)))

(def-foreign-type uv_env_item_t
    (:struct
     (name (* :char) simple-string)
     (value (* :char) simple-string)))

;; API
(def-foreign-call uv_guess_handle ((file uv_file))
  :returning :int)


(def-foreign-call uv_replace_allocator ((malloc_func :foreign-address)
                                        (realloc_func :foreign-address)
                                        (calloc_func :foreign-address)
                                        (free_fun :foreign-address))
  :returning :int)

(def-foreign-call uv_library_shutdown (:void)
  :returning :void)

;; we don't really need it ...
;; (def-foreign-call uv_buf_init ((base (* :char)) (len :unsigned-int))
;;   :strings-convert nil
;;   :returning uv_buf_t
;;   :pass-structs-by-value t)

(def-foreign-call uv_setup_args ((argc :int)
                                 (argv (:array (* :char)) (simple-array simple-string)))
  :returning ((:array (* :char)) (simple-array simple-string)))

(def-foreign-call uv_get_process_title ((buffer (* :char)) (size size_t))
  :returning :int
  :strings-convert nil)

(def-foreign-call uv_set_process_title ((title (* :char) simple-string))
  :returning :int
  :strings-convert t)

(def-foreign-call uv_resident_set_memory ((rss (* size_t)))
  :returning :int)

(def-foreign-call uv_uptime ((uptime (* :double)))
  :returning :int)

(def-foreign-call uv_getrusage ((rusage (* uv_rusage_t)))
  :returning :int)

(def-foreign-call uv_os_getpid (:void)
  :returning uv_pid_t)

(def-foreign-call uv_os_getppid (:void)
  :returning uv_pid_t)

(def-foreign-call uv_available_parallelism (:void)
  :returning :unsigned-int)

(def-foreign-call uv_cpu_info ((cpu_infos (:array uv_cpu_info_t)) (count (* :int)))
  :returning :int)

(def-foreign-call uv_free_cpu_info ((cpu_infos (* uv_cpu_info_t)) (count :int))
  :returning :int)

(def-foreign-call uv_cpumask_size (:void)
  :returning :int)

(def-foreign-call uv_interface_addresses ((addresses (:array uv_interface_address_t))
                                          (count (* :int)))
  :returning :int)

(def-foreign-call uv_free_interface_addresses ((addresses (* uv_interface_address_t))
                                               (count :int))
  :returning :void)

(def-foreign-call uv_loadavg ((avg (:array :double 3)))
  :returning :void
  :strings-convert nil)

(def-foreign-call uv_ip4_addr ((ip (* :char) simple-string)
                               (port :int)
                               (addr (* sockaddr_in)))
  :returning :int
  :strings-convert t)

(def-foreign-call uv_ip6_addr ((ip (* :char) simple-string)
                               (port :int)
                               (addr (* sockaddr_in6)))
  :returning :int
  :strings-convert t)

(def-foreign-call uv_ip4_name ((src (* sockaddr_in))
                               (dst (* :char))
                               (size (* size_t)))
  :returning :int
  :strings-convert nil)

(def-foreign-call uv_ip6_name ((src (* sockaddr_in6))
                               (dst (* :char))
                               (size (* size_t)))
  :returning :int
  :strings-convert nil)

(def-foreign-call uv_ip_name ((src (* sockaddr))
                              (dst (* :char))
                              (size (* size_t)))
  :returning :int
  :strings-convert nil)

(def-foreign-call uv_inet_ntop ((af :int)
                                (src (* :void))
                                (dst (* :char))
                                (size (* size_t)))
  :returning :int
  :strings-convert nil)

(def-foreign-call uv_inet_pton ((af :int)
                                (src (* :char) simple-string)
                                (dst (* :void)))
  :returning :int
  :strings-convert t)

(def-foreign-call uv_if_indextoname ((ifindex :unsigned-int) (buffer (* :char)) (size (* size_t)))
  :returning :int
  :strings-convert nil)

(def-foreign-call uv_if_indextoiid ((ifindex :unsigned-int) (buffer (* :char)) (size (* size_t)))
  :returning :int
  :strings-convert nil)

(def-foreign-call uv_exepath ((buffer (* :char)) (size (* size_t)))
  :returning :int
  :strings-convert nil)

(def-foreign-call uv_cwd ((buffer (* :char)) (size (* size_t)))
  :returning :int
  :strings-convert nil)

(def-foreign-call uv_chdir ((dir (* :char) simple-string))
  :returning :int
  :strings-convert t)

(def-foreign-call uv_os_homedir ((buffer (* :char)) (size (* size_t)))
  :returning :int
  :strings-convert nil)

(def-foreign-call uv_os_tmpdir ((buffer (* :char)) (size (* size_t)))
  :returning :int
  :strings-convert nil)

(def-foreign-call uv_os_get_passwd ((pwd (* uv_passwd_t)))
  :returning :int)

(def-foreign-call uv_os_free_passwd ((pwd (* uv_passwd_t)))
  :returning :void)

(def-foreign-call uv_get_free_memory (:void)
  :returning uint64_t)

(def-foreign-call uv_get_total_memory (:void)
  :returning uint64_t)

(def-foreign-call uv_get_constrained_memory (:void)
  :returning uint64_t)

(def-foreign-call uv_get_available_memory (:void)
  :returning uint64_t)

(def-foreign-call uv_hrtime (:void)
  :returning uint64_t)

(def-foreign-call uv_print_all_handles ((event-loop (* uv_loop_t)) (stream :foreign-address))
  :returning :void)

(def-foreign-call uv_print_active_handles ((event-loop (* uv_loop_t)) (stream :foreign-address))
  :returning :void)

(def-foreign-call uv_os_environ ((envitems (:array uv_env_item_t) (simple-array uv_env_item_t))
                                 (count (* :int)))
  :returning :int)

(def-foreign-call uv_os_free_environ ((envitems (:array uv_env_item_t) (simple-array uv_env_item_t))
                                      (count :int))
  :returning :void)

(def-foreign-call uv_os_getenv ((name (* :char) simple-string)
                                (buffer (* :char))
                                (size (* size_t)))
  :returning :int
  :strings-convert t)

(def-foreign-call uv_os_setenv ((name (* :char) simple-string)
                                (value (* :char) simple-string))
  :returning :int
  :strings-convert t)

(def-foreign-call uv_os_unsetenv ((name (* :char) simple-string))
  :returning :int
  :strings-convert nil)

(def-foreign-call uv_os_gethostname ((buffer (* :char))
                                     (size (* size_t)))
  :returning :int
  :strings-convert nil)

(def-foreign-call uv_os_getpriority ((pid uv_pid_t) (priority (* :int)))
  :returning :int)

(def-foreign-call uv_os_setpriority ((pid uv_pid_t) (priority :int))
  :returning :int)

(def-foreign-call uv_os_uname ((buffer (* uv_utsname_t)))
  :returning :int)

(def-foreign-call uv_gettimeofday ((tv (* uv_timeval64_t)))
  :returning :int)

(def-foreign-call uv_random ((event-loop (* uv_loop_t))
                             (req (* uv_random_t))
                             (buf (* :void))
                             (buflen size_t)
                             (flags :unsigned-int)
                             (cb :foreign-address))
  :returning :int)

(def-foreign-call uv_sleep ((msec :unsigned-int))
  :returning :void)

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
