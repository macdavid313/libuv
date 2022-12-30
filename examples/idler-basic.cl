;;;; idler-basic.cl
(in-package #:cl-user)

(defparameter *counter* 0)

(ff:defun-foreign-callable wait-for-a-while ((handle (* libuv:uv_idle_t)))
  (declare (special *counter*))
  (incf *counter*)
  (when (>= *counter* (expt 10 6))
    (format t "Stop idling ...")
    (libuv:uv_idle_stop handle)))

(eval-when (:load-toplevel :execute)
  (defparameter *cb* (ff:register-foreign-callable 'wait-for-a-while)))

(defun main ()
  (declare (special *cb*))
  (let ((event-loop (ff:allocate-fobject 'libuv:uv_loop_t))
        (idler (ff:allocate-fobject 'libuv:uv_idle_t)))
    (libuv:uv_loop_init event-loop)
    (libuv:uv_idle_init event-loop idler)
    (libuv:uv_idle_start idler *cb*)
    (format t "Idling...~%")
    (libuv:uv_run event-loop (libuv:foreign-enum-value 'libuv:uv_run_mode :UV_RUN_DEFAULT))
    (libuv:uv_loop_close event-loop)))
