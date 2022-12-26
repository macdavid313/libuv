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

;;; TCP handle
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

;;; Pipe handle
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

;;; TTY handle
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

;;; UDP handle
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

;;; FS Event handle
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
