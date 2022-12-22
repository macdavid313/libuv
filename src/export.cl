;;;; export.cl
(in-package #:libuv)

(eval-when (:load-toplevel :execute)
  (let ((*package* (find-package :libuv)))
    (export 'sszie_t)
    (export 'size_t)

    (export 'uint8_t)
    (export 'uint16_t)
    (export 'uint32_t)
    (export 'uint64_t)
    (export 'int8_t)
    (export 'int16_t)
    (export 'int32_t)
    (export 'int64_t)

    (export 'in_addr)
    (export 'in6_addr)
    (export 'sockaddr)
    (export 'sockaddr_in)
    (export 'sockaddr_in6)
    (export 'addrinfo)

    (do-symbols (sym)
      (when (match-re "^\\+?uv[_|-]" (string sym) :case-fold t)
        (export sym)))))
