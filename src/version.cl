;;;; version.cl
(in-package #:libuv)

(ff:def-foreign-call uv_version (:void)
  :returning :unsigned-int)

;; (defun uv_version ()
;;   "Return major, minor and patch number as three values."
;;   (let ((version (%uv_version)))
;;     (values (ldb (byte 8 16) version)   ; major
;;             (ldb (byte 8 8) version)    ; minor
;;             (ldb (byte 8 0) version)))) ; patch

(ff:def-foreign-call uv_version_string (:void)
  :returning ((* :char) string)
  :strings-convert t
  :documentation "Returns the libuv version number as a string. For non-release versions the version suffix is included.")
