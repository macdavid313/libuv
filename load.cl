;;;; load.cl
(in-package #:cl-user)

(eval-when (:load-toplevel :execute)
  #-libuv
  (progn
    (load "libuv.so" :foreign t :search-list (list "~/.local/lib"))
    (pushnew :libuv *features*))

  (defparameter *libuv-files* (list "package"
                                    "prelude"
                                    "uv-constants"
                                    "apis"
                                    "export"))

  ;; compile and load
  (dolist (file *libuv-files*)
    (compile-file (merge-pathnames (format nil "src/~a.cl" file) *load-pathname*)
                  :load-after-compile t))

  ;; write to single fasl
  (with-open-file (out (merge-pathnames "libuv.fasl" *load-pathname*)
                       :element-type '(unsigned-byte 8)
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
    (loop with buffer = (make-array 4096 :element-type '(unsigned-byte 8))
          for file in *libuv-files*
          do (with-open-file (in (merge-pathnames (format nil "src/~a.fasl" file) *load-pathname*)
                                 :direction :input
                                 :element-type '(unsigned-byte 8))
               (let ((count (read-sequence buffer in)))
                 (while (plusp count)
                   (write-sequence buffer out :end count)
                   (setq count (read-sequence buffer in))))))))
