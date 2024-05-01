
;; ███████╗███████╗██╗
;; ██╔════╝██╔════╝██║
;; █████╗  █████╗  ██║
;; ██╔══╝  ██╔══╝  ██║
;; ██║     ██║     ██║
;; ╚═╝     ╚═╝     ╚═╝
;; Contains cffi definitions towards the libwayland-server library.
(defpackage :cl-wl.ffi
  (:use :cl :cffi)
  (:nicknames :wl-ffi)
  (:export display-create display-destroy global-create global-get-name resource-get-id resource-create
	   client-create wl_message display-add-socket-fd display-run display-get-event-loop event-loop-get-fd
	   event-loop-dispatch display-flush-clients resource-set-dispatcher wl_resource
	   wl_argument resource-post-event-array name version method_count methods event_count events
	   signature types wl_array))

(in-package :cl-wl.ffi)
(define-foreign-library wayland-server
  (t (:default "libwayland-server")))

;; ┌─┐┌┬┐┬─┐┬ ┬┌─┐┌┬┐┌─┐
;; └─┐ │ ├┬┘│ ││   │ └─┐
;; └─┘ ┴ ┴└─└─┘└─┘ ┴ └─┘
(defcstruct wl_message
  (name :string)
  (signature :string)
  (types :pointer))

(defcstruct wl_resource
  (object :pointer)
  (destroy :pointer)
  (link :pointer)
  (deprecated_destroy_signal :pointer)
  (client :pointer)
  (data :pointer)
  (version :int)
  (dispatcher :pointer)
  (destroy_signal :pointer))

(defcstruct wl_array
  (size :size)
  (alloc :size)
  (data :pointer))

(defcunion wl_argument
  (i :int)                          ;; integer
  (u :uint)                         ;; unsigned integer
  (f :float)                        ;; float maybe fixed -check and update this comment and type :)
  (s :string)                       ;; string
  (o :pointer)                      ;; object - usually reference to some wl_resource
  (n :uint)                         ;; new_id
  (a (:pointer (:struct wl_array))) ;; array
  (h :int))                         ;; file descriptor

;; ┌─┐┬ ┬┌┐┌┌─┐┌─┐
;; ├┤ │ │││││  └─┐
;; └  └─┘┘└┘└─┘└─┘

(cffi:defcfun memset :pointer
  (ptr :pointer)
  (val :int)
  (size :int))

(defcfun ("wl_display_create" display-create) :pointer)
(defcfun ("wl_display_destroy" display-destroy) :void
  (display :pointer))
(defcfun ("wl_display_add_socket_fd" display-add-socket-fd) :int
  (display :pointer)
  (fd :int))

(defcfun ("wl_display_run" display-run) :void
  (display :pointer))

(defcfun ("wl_display_get_event_loop" display-get-event-loop) :pointer
  (display :pointer))

(defcfun ("wl_display_flush_clients" display-flush-clients) :void
  (display :pointer))

(defcfun ("wl_event_loop_get_fd" event-loop-get-fd) :int
  (loop :pointer))

(defcfun ("wl_event_loop_dispatch" event-loop-dispatch) :int
  (loop :pointer)
  (timeout :int))

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

(defcfun ("wl_resource_set_dispatcher" resource-set-dispatcher) :void
  (resource :pointer)
  (dispatcher :pointer)
  (implementation :pointer)
  (data :pointer)
  (destroy :pointer))

(defcfun ("wl_resource_post_event_array" resource-post-event-array) :void
  (resource :pointer)
  (opcode :uint32)
  (args :pointer))
