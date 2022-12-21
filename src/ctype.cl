;;;; ctypes.cl
(in-package #:libuv)

;;; stddef.h
(ff:def-foreign-type ssize_t  :nat)
(ff:def-foreign-type size_t   :unsigned-nat)

;;; stdint.h
(ff:def-foreign-type uint8_t  :unsigned-char)
(ff:def-foreign-type uint16_t :unsigned-short)
(ff:def-foreign-type uint32_t :unsigned-int)
(ff:def-foreign-type uint64_t #+windows :unsigned-nat #-windows :unsigned-long)
