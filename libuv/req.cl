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
