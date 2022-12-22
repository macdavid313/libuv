;;;; misc.cl
(in-package #:libuv)

;;; Data types
(def-foreign-type uv_buf_t
    (:struct
     (base (* :char))
     (len #+windows :unisgned-long
          #-windows size_t)))

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

;;; API
(def-foreign-call uv_guess_handle ((file uv_file))
  :returning :int)


(def-foreign-call uv_replace_allocator ((malloc_func :foreign-address)
                                        (realloc_func :foreign-address)
                                        (calloc_func :foreign-address)
                                        (free_fun :foreign-address))
  :returning :int)

(def-foreign-call uv_library_shutdown (:void)
  :returning :void)

;;; FIXME: `ff::build-struct-pass-spec' is broken on uv_buf_init, 22/12/2022
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
