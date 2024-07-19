
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
  (:export
   global-create global-get-name
   event-loop-get-fd event-loop-dispatch
   client-create client-add-destroy-listener client-add-destroy-late-listener %client-destroy

   resource-post-event-array resource-add-destroy-listener
   resource-get-id resource-create resource-set-dispatcher

   display-run display-flush-clients display-destroy-clients display-get-event-loop
   display-create display-destroy display-add-socket-fd

   wl_resource wl_list wl_argument wl_argument_outgoing wl_listener wl_array wl_message

   name version method_count methods event_count events
   signature types

   fixed-from-double fixed-from-int))

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

(defcstruct wl_list
  (prev :pointer)
  (next :pointer))

(defcstruct wl_listener
  (link (:struct wl_list))
  (notify :pointer))

(defcunion wl_argument
  (i :int)                          ;; integer
  (u :uint)                         ;; unsigned integer
  (f :int32)                        ;; fixed numbers are represented with :int32
  (s :string)                       ;; string
  (o :pointer)                      ;; object - usually reference to some wl_resource
  (n :uint)                         ;; new_id
  (a (:pointer (:struct wl_array))) ;; array
  (h :int))                         ;; file descriptor

;; TODO: Libwayland sucks. When the n (new_id) is used for server events
;; Libwayland expects a pointer to an object previously created by us
;; But when libwayland uses our callbacks it actually gives a number rather than a pointer
(defcunion wl_argument_outgoing
  (i :int)                          ;; integer
  (u :uint)                         ;; unsigned integer
  (f :int32)                        ;; fixed numbers are represented with :int32
  (s :string)                       ;; string
  (o :pointer)                      ;; object - usually reference to some wl_resource
  (n :pointer)                      ;; new_id - libwayland is annoying and expects a pointer to the object rather than its id. Then the lib extracts the id itself
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

(defcfun ("wl_display_destroy_clients" display-destroy-clients) :void
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

(defcfun ("wl_client_get_fd" client-get-fd) :int
  (client :pointer))

(defcfun ("wl_client_get_credentials" client-get-credentials) :void
  (client :pointer)
  (pid :pointer)
  (uid :pointer)
  (gid :pointer))

(defcfun ("wl_client_create" client-create) :pointer
  (display :pointer)
  (fd :int32))

(defcfun ("wl_client_add_destroy_listener" client-add-destroy-listener) :void
  (client :pointer)
  (listener (:pointer (:struct wl_listener))))

(defcfun ("wl_client_add_destroy_late_listener" client-add-destroy-late-listener) :void
  (client :pointer)
  (listener (:pointer (:struct wl_listener))))

(defcfun ("wl_client_destroy" %client-destroy) :void
  (client :pointer))

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

(defcfun ("wl_resource_add_destroy_listener" resource-add-destroy-listener) :void
  (resource :pointer)
  (listener (:pointer (:struct wl_listener))))


;; ┬ ┬┌┬┐┬┬
;; │ │ │ ││
;; └─┘ ┴ ┴┴─┘
;; NOTE: Wayland uses :int32 to store its fixed point numbers
(defcfun ("wl_fixed_from_int" fixed-from-int)       :int32 (i :int))
(defcfun ("wl_fixed_from_double" fixed-from-double) :int32 (d :double))
