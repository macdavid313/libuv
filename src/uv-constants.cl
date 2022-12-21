;;;; uv-constants.cl
;;;; DO NOT MODIFY: this file has been automatically generated by uv_constants.c
(in-package #:libuv)


;;; uv_loop_t
(def-foreign-type uv_loop_t (:array :unsigned-char 848))


;;; C Enum: uv_run_mode
;;; http://docs.libuv.org/en/v1.x/loop.html#c.uv_run_mode
(defconstant +UV_RUN_DEFAULT+ 0)
(defconstant +UV_RUN_ONCE+ 1)
(defconstant +UV_RUN_NOWAIT+ 2)


;;; C Enum: uv_loop_option
(defconstant +UV_LOOP_BLOCK_SIGNAL+ 0)
(defconstant +UV_METRICS_IDLE_TIME+ 1)


;;; Error constants
;;; http://docs.libuv.org/en/v1.x/errors.html#error-constants
(defconstant +UV_E2BIG+ -7 "argument list too long")
(defconstant +UV_EACCES+ -13 "permission denied")
(defconstant +UV_EADDRINUSE+ -98 "address already in use")
(defconstant +UV_EADDRNOTAVAIL+ -99 "address not available")
(defconstant +UV_EAFNOSUPPORT+ -97 "address family not supported")
(defconstant +UV_EAGAIN+ -11 "resource temporarily unavailable")
(defconstant +UV_EAI_ADDRFAMILY+ -3000 "address family not supported")
(defconstant +UV_EAI_AGAIN+ -3001 "temporary failure")
(defconstant +UV_EAI_BADFLAGS+ -3002 "bad ai_flags value")
(defconstant +UV_EAI_BADHINTS+ -3013 "invalid value for hints")
(defconstant +UV_EAI_CANCELED+ -3003 "request canceled")
(defconstant +UV_EAI_FAIL+ -3004 "permanent failure")
(defconstant +UV_EAI_FAMILY+ -3005 "ai_family not supported")
(defconstant +UV_EAI_MEMORY+ -3006 "out of memory")
(defconstant +UV_EAI_NODATA+ -3007 "no address")
(defconstant +UV_EAI_NONAME+ -3008 "unknown node or service")
(defconstant +UV_EAI_OVERFLOW+ -3009 "argument buffer overflow")
(defconstant +UV_EAI_PROTOCOL+ -3014 "resolved protocol is unknown")
(defconstant +UV_EAI_SERVICE+ -3010 "service not available for socket type")
(defconstant +UV_EAI_SOCKTYPE+ -3011 "socket type not supported")
(defconstant +UV_EALREADY+ -114 "connection already in progress")
(defconstant +UV_EBADF+ -9 "bad file descriptor")
(defconstant +UV_EBUSY+ -16 "resource busy or locked")
(defconstant +UV_ECANCELED+ -125 "operation canceled")
(defconstant +UV_ECHARSET+ -4080 "invalid Unicode character")
(defconstant +UV_ECONNABORTED+ -103 "software caused connection abort")
(defconstant +UV_ECONNREFUSED+ -111 "connection refused")
(defconstant +UV_ECONNRESET+ -104 "connection reset by peer")
(defconstant +UV_EDESTADDRREQ+ -89 "destination address required")
(defconstant +UV_EEXIST+ -17 "file already exists")
(defconstant +UV_EFAULT+ -14 "bad address in system call argument")
(defconstant +UV_EFBIG+ -27 "file too large")
(defconstant +UV_EHOSTUNREACH+ -113 "host is unreachable")
(defconstant +UV_EINTR+ -4 "interrupted system call")
(defconstant +UV_EINVAL+ -22 "invalid argument")
(defconstant +UV_EIO+ -5 "i/o error")
(defconstant +UV_EISCONN+ -106 "socket is already connected")
(defconstant +UV_EISDIR+ -21 "illegal operation on a directory")
(defconstant +UV_ELOOP+ -40 "too many symbolic links encountered")
(defconstant +UV_EMFILE+ -24 "too many open files")
(defconstant +UV_EMSGSIZE+ -90 "message too long")
(defconstant +UV_ENAMETOOLONG+ -36 "name too long")
(defconstant +UV_ENETDOWN+ -100 "network is down")
(defconstant +UV_ENETUNREACH+ -101 "network is unreachable")
(defconstant +UV_ENFILE+ -23 "file table overflow")
(defconstant +UV_ENOBUFS+ -105 "no buffer space available")
(defconstant +UV_ENODEV+ -19 "no such device")
(defconstant +UV_ENOENT+ -2 "no such file or directory")
(defconstant +UV_ENOMEM+ -12 "not enough memory")
(defconstant +UV_ENONET+ -64 "machine is not on the network")
(defconstant +UV_ENOPROTOOPT+ -92 "protocol not available")
(defconstant +UV_ENOSPC+ -28 "no space left on device")
(defconstant +UV_ENOSYS+ -38 "function not implemented")
(defconstant +UV_ENOTCONN+ -107 "socket is not connected")
(defconstant +UV_ENOTDIR+ -20 "not a directory")
(defconstant +UV_ENOTEMPTY+ -39 "directory not empty")
(defconstant +UV_ENOTSOCK+ -88 "socket operation on non-socket")
(defconstant +UV_ENOTSUP+ -95 "operation not supported on socket")
(defconstant +UV_EOVERFLOW+ -75 "value too large for defined data type")
(defconstant +UV_EPERM+ -1 "operation not permitted")
(defconstant +UV_EPIPE+ -32 "broken pipe")
(defconstant +UV_EPROTO+ -71 "protocol error")
(defconstant +UV_EPROTONOSUPPORT+ -93 "protocol not supported")
(defconstant +UV_EPROTOTYPE+ -91 "protocol wrong type for socket")
(defconstant +UV_ERANGE+ -34 "result too large")
(defconstant +UV_EROFS+ -30 "read-only file system")
(defconstant +UV_ESHUTDOWN+ -108 "cannot send after transport endpoint shutdown")
(defconstant +UV_ESPIPE+ -29 "invalid seek")
(defconstant +UV_ESRCH+ -3 "no such process")
(defconstant +UV_ETIMEDOUT+ -110 "connection timed out")
(defconstant +UV_ETXTBSY+ -26 "text file is busy")
(defconstant +UV_EXDEV+ -18 "cross-device link not permitted")
(defconstant +UV_UNKNOWN+ -4094 "unknown error")
(defconstant +UV_EOF+ -4095 "end of file")
(defconstant +UV_ENXIO+ -6 "no such device or address")
(defconstant +UV_EMLINK+ -31 "too many links")
(defconstant +UV_EHOSTDOWN+ -112 "host is down")
(defconstant +UV_EREMOTEIO+ -121 "remote I/O error")
(defconstant +UV_ENOTTY+ -25 "inappropriate ioctl for device")
(defconstant +UV_EFTYPE+ -4028 "inappropriate file type or format")
(defconstant +UV_EILSEQ+ -84 "illegal byte sequence")
(defconstant +UV_ESOCKTNOSUPPORT+ -94 "socket type not supported")


;;; Handle types
;;; http://docs.libuv.org/en/v1.x/handle.html#c.uv_handle_type
(def-foreign-type uv_handle_t (:array :unsigned-char 96))
(defconstant +UV_UNKNOWN_HANDLE+ 0)
(defconstant +UV_ASYNC+ 1)
(def-foreign-type uv_async_t (:array :unsigned-char 128))
(defconstant +UV_CHECK+ 2)
(def-foreign-type uv_check_t (:array :unsigned-char 120))
(defconstant +UV_FS_EVENT+ 3)
(def-foreign-type uv_fs_event_t (:array :unsigned-char 136))
(defconstant +UV_FS_POLL+ 4)
(def-foreign-type uv_fs_poll_t (:array :unsigned-char 104))
(defconstant +UV_HANDLE+ 5)
(def-foreign-type uv_handle_t (:array :unsigned-char 96))
(defconstant +UV_IDLE+ 6)
(def-foreign-type uv_idle_t (:array :unsigned-char 120))
(defconstant +UV_NAMED_PIPE+ 7)
(def-foreign-type uv_pipe_t (:array :unsigned-char 264))
(defconstant +UV_POLL+ 8)
(def-foreign-type uv_poll_t (:array :unsigned-char 160))
(defconstant +UV_PREPARE+ 9)
(def-foreign-type uv_prepare_t (:array :unsigned-char 120))
(defconstant +UV_PROCESS+ 10)
(def-foreign-type uv_process_t (:array :unsigned-char 136))
(defconstant +UV_STREAM+ 11)
(def-foreign-type uv_stream_t (:array :unsigned-char 248))
(defconstant +UV_TCP+ 12)
(def-foreign-type uv_tcp_t (:array :unsigned-char 248))
(defconstant +UV_TIMER+ 13)
(def-foreign-type uv_timer_t (:array :unsigned-char 152))
(defconstant +UV_TTY+ 14)
(def-foreign-type uv_tty_t (:array :unsigned-char 312))
(defconstant +UV_UDP+ 15)
(def-foreign-type uv_udp_t (:array :unsigned-char 216))
(defconstant +UV_SIGNAL+ 16)
(def-foreign-type uv_signal_t (:array :unsigned-char 152))
(defconstant +UV_HANDLE_TYPE_MAX+ 18)


;;; Req types
;;; http://docs.libuv.org/en/v1.x/request.html#c.uv_req_t.type
(defconstant +UV_UNKNOWN_REQ+ 0)
(defconstant +UV_REQ+ 1)
(def-foreign-type uv_req_t (:array :unsigned-char 64))
(defconstant +UV_CONNECT+ 2)
(def-foreign-type uv_connect_t (:array :unsigned-char 96))
(defconstant +UV_WRITE+ 3)
(def-foreign-type uv_write_t (:array :unsigned-char 192))
(defconstant +UV_SHUTDOWN+ 4)
(def-foreign-type uv_shutdown_t (:array :unsigned-char 80))
(defconstant +UV_UDP_SEND+ 5)
(def-foreign-type uv_udp_send_t (:array :unsigned-char 320))
(defconstant +UV_FS+ 6)
(def-foreign-type uv_fs_t (:array :unsigned-char 440))
(defconstant +UV_WORK+ 7)
(def-foreign-type uv_work_t (:array :unsigned-char 128))
(defconstant +UV_GETADDRINFO+ 8)
(def-foreign-type uv_getaddrinfo_t (:array :unsigned-char 160))
(defconstant +UV_GETNAMEINFO+ 9)
(def-foreign-type uv_getnameinfo_t (:array :unsigned-char 1320))
(defconstant +UV_RANDOM+ 10)
(def-foreign-type uv_random_t (:array :unsigned-char 144))
(defconstant +UV_REQ_TYPE_MAX+ 11)

