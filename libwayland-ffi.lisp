
;; ███████╗███████╗██╗
;; ██╔════╝██╔════╝██║
;; █████╗  █████╗  ██║
;; ██╔══╝  ██╔══╝  ██║
;; ██║     ██║     ██║
;; ╚═╝     ╚═╝     ╚═╝
(defpackage :bm-cl-libwayland
  (:use :cl :cffi)
  (:nicknames :wl-ffi)
  (:export display-create global-create global-get-name resource-get-id resource-create
	   client-create wl_message))

(in-package :bm-cl-libwayland)
(define-foreign-library wayland-server
  (t (:default "libwayland-server")))

(defcstruct wl_message
  (name :string)
  (signature :string)
  (types :pointer))

(defcfun ("wl_display_create" display-create) :pointer)

(defcfun ("wl_global_create" global-create) :pointer
  (display :pointer)
  (interface :pointer)
  (version :int32)
  (data :pointer)
  (func :pointer))

(defcfun ("wl_global_get_name" global-get-name) :uint32
  (global :pointer)
  (client :pointer))

(defcfun ("wl_client_get_credentials" client-get-credentials) :void
  (client :pointer)
  (pid :pointer)
  (uid :pointer)
  (gid :pointer))

(defcfun ("wl_client_create" client-create) :pointer
  (display :pointer)
  (fd :int32))


(defcfun ("wl_resource_create" resource-create) :pointer
  (client :pointer)
  (interface :pointer)
  (version :int32)
  (id :uint32))

(defcfun ("wl_resource_get_id" resource-get-id) :uint32
  (resource :pointer))
