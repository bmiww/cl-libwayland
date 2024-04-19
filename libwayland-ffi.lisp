
(defpackage :bm-cl-libwayland
  (:use :cl :cffi))

(in-package :bm-cl-libwayland)
(define-foreign-library wayland-server
  (t (:default "libwayland-server")))

(defcfun ("wl_display_create" wl-display-create) :pointer)
