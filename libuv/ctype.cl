;;;; ctypes.cl
(in-package #:libuv)

;;; stddef.h
(def-foreign-type ssize_t  :nat)
(def-foreign-type size_t   :unsigned-nat)

;;; stdint.h
(def-foreign-type uint8_t  :unsigned-char)
(def-foreign-type uint16_t :unsigned-short)
(def-foreign-type uint32_t :unsigned-int)
(def-foreign-type uint64_t :unsigned-long-long)
(def-foreign-type int8_t   :char)
(def-foreign-type int16_t  :short)
(def-foreign-type int32_t  :int)
(def-foreign-type int64_t  :long-long)

;;; socket struct
(def-foreign-type in_addr
    (:struct
     (s-addr :unsigned-long)))

(def-foreign-type in6_addr
    (:struct
     (s6-addr (:array :unsigned-char 16))))

(def-foreign-type sockaddr
    (:struct
     (sa_family :unsigned-short)
     (sa_data (:array :char 14))))

(def-foreign-type sockaddr_in
    (:struct
     (sin_family :short)
     (sin_addr :unsigned-short)
     (sin_addr in_addr)
     (sin_zero (:array :char 8))))

(def-foreign-type sockaddr_in6
    (:struct
     (sin6_family :unsigned-short)
     (sin6_port :unsigned-short)
     (sin6_flowinfo :unsigned-int)
     (sin6_addr in6_addr)
     (sin6_scope_id :unsigned-int)))

(def-foreign-type addrinfo
    (:struct
     (ai_flags :int)
     (ai_family :int)
     (ai_socktype :int)
     (ai_protocol :int)
     (ai_addrlen size_t)
     (ai_addr (* :void))
     (ai_canonname (* :void))
     (ai_next (* addrinfo))))

;;; C enums
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter .cenum-table. (make-hash-table :test 'eq)))

(defmacro def-foreign-enum (name &body enum-list)
  (let* ((table (gensym))
         (items (loop for (k v) in enum-list
                      collect `(setf (gethash ,k ,table) ,v))))
    `(let ((,table (make-hash-table :test 'eq)))
       ,@items
       (setf (gethash ',name .cenum-table.) ,table)
       (def-foreign-type ,name :int)
       ',name)))

(defun foreign-enum-value (type k)
  (multiple-value-bind (v exists-p)
      (gethash k (gethash type .cenum-table.))
    (when (not exists-p)
      (error "cannot find value in enum '~a by keword: :~a" type k))
    v))

(defun foreign-enum-keyword (type v)
  (maphash (lambda (k val)
             (when (= val v)
               (return-from foreign-enum-keyword k)))
           (gethash type .cenum-table.))
  (error "Cannot find keyword in enum '~a by value ~d" type v))
