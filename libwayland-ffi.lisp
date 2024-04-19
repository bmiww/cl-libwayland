
;; ███████╗███████╗██╗
;; ██╔════╝██╔════╝██║
;; █████╗  █████╗  ██║
;; ██╔══╝  ██╔══╝  ██║
;; ██║     ██║     ██║
;; ╚═╝     ╚═╝     ╚═╝
(defpackage :bm-cl-libwayland
  (:use :cl :cffi)
  (:nicknames :wl-ffi)
  (:export display-create global-create global-get-name))

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

(defcfun ("wl_global_get_name" global-get-name) :uint32
  (global :pointer))

(defcfun ("wl_client_get_credentials" client-get-credentials) :void
  (client :pointer)
  (pid :pointer)
  (uid :pointer)
  (gid :pointer))

(defcfun ("wl_client_create" client-create) :pointer
  (display :pointer)
  (fd :int32))
