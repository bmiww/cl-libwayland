
;; ███████╗███████╗██╗
;; ██╔════╝██╔════╝██║
;; █████╗  █████╗  ██║
;; ██╔══╝  ██╔══╝  ██║
;; ██║     ██║     ██║
;; ╚═╝     ╚═╝     ╚═╝
(defpackage :bm-cl-libwayland
  (:use :cl :cffi)
  (:export display-create global-create))

(in-package :bm-cl-libwayland)
(define-foreign-library wayland-server
  (t (:default "libwayland-server")))

(defcfun ("wl_display_create" display-create) :pointer)

(defcfun ("wl_global_create" global-create) :pointer
  (display :pointer)
  (interface :pointer)
  (version :int32)
  (data :pointer)
  (func :pointer))
