
;; ██╗    ██╗ █████╗ ██╗   ██╗██╗      █████╗ ███╗   ██╗██████╗
;; ██║    ██║██╔══██╗╚██╗ ██╔╝██║     ██╔══██╗████╗  ██║██╔══██╗
;; ██║ █╗ ██║███████║ ╚████╔╝ ██║     ███████║██╔██╗ ██║██║  ██║
;; ██║███╗██║██╔══██║  ╚██╔╝  ██║     ██╔══██║██║╚██╗██║██║  ██║
;; ╚███╔███╔╝██║  ██║   ██║   ███████╗██║  ██║██║ ╚████║██████╔╝
;;  ╚══╝╚══╝ ╚═╝  ╚═╝   ╚═╝   ╚══════╝╚═╝  ╚═╝╚═╝  ╚═══╝╚═════╝
;; NOTE: Maybe one day try to refer to the darn book: https://github.com/rcalixte/wayland-book
;; NOTE: SWC compositor reference: https://github.com/michaelforney/swc
;; TODO: create another package entry file - which exports the functions that could be used by the end user
;; Currently exporting things that the generated files can use
(defpackage #:bm-cl-wayland
  (:use #:cl #:bm-cl-libwayland #:cffi)
  (:nicknames :wl)
  (:export display-create create-client *global-tracker* resource-get-id object get-client iface
	   get-data pop-data display create-resource reserve-data global-create version data-ptr set-data
	   global-get-name wl_message display-add-socket-fd display-run display-get-event-loop event-loop-get-fd
	   event-loop-dispatch display-flush-clients ptr debug-log! resource-set-dispatcher dispatch-impl
	   wl_resource *resource-tracker* wl_argument id client mk-if))
(in-package :bm-cl-wayland)

(defclass object ()
  ((display :initarg :display :reader display)
   (client :initarg :client :reader client)
   (version :initarg :version :reader version)
   (id :initarg :id :reader id)))

(defmethod mk-if (class (object object) id &rest args)
  "Convenience method to create a new interface using the context of the creating object as reference"
  (apply #'make-instance class :display (display object) :client (client object) :id id args))

;; Uses integer value pointer addresses as keys
(defvar *global-tracker* (make-hash-table :test 'eq))

(defclass global (object)
  ((dispatch-impl :initarg :dispatch-impl :reader dispatch-impl)))
(defgeneric bind (client resource id))

(defvar *resource-tracker* (make-hash-table :test 'eq))
(defun create-resource (client interface version id) (resource-create client interface version id))

;; ┌─┐┬  ┬┌─┐┌┐┌┌┬┐
;; │  │  │├┤ │││ │
;; └─┘┴─┘┴└─┘┘└┘ ┴
(defclass client ()
  ((objects :initform (make-hash-table :test 'eq) :accessor objects)
   (display :initarg :display :reader display)
   (ptr :initarg :ptr :reader ptr)))

(defvar *client-tracker* (make-hash-table :test 'equal))

(defun client-get-credentials (client)
  (with-foreign-objects ((pid :int) (uid :int) (gid :int))
    (wl-ffi::client-get-credentials client pid uid gid)
    (values (mem-aref pid :int) (mem-aref uid :int) (mem-aref gid :int))))

(defun get-client (client)
  (let* ((pid (client-get-credentials client))
	 (client (gethash pid *client-tracker*)))
    (unless client (error (format nil "No client found for pid ~a" pid)))
    client))

(defun create-client (display fd)
  "This function should be called when a new client connects to the socket.
This will in essence forward the client to the libwayland implementation
and set up the client object in the lisp world for further referencing."
  (let* ((client (client-create display fd))
	 (pid (client-get-credentials client)))
    (setf (gethash pid *client-tracker*) (make-instance 'client :display display :ptr client))
    client))

(defmethod iface ((client client) interface)
  (let ((iface (gethash interface (objects client))))
    (unless iface (error (format nil "No interface found for ~a" interface)))
    iface))

(defmethod (setf iface) (iface (client client) interface)
  (setf (gethash interface (objects client)) iface))


;; ┌┬┐┌─┐┌┬┐┌─┐
;;  ││├─┤ │ ├─┤
;; ─┴┘┴ ┴ ┴ ┴ ┴
;; TODO: Might not need to be a hash-table since i intend this to be ephemeral
(defvar *data-tracker* (make-hash-table :test 'equal))
;; TODO: This could technically run out - so reserve data should possibly check for C uint32 limits
(defvar *data-counter* 0)

(defun reserve-data (&optional data)
  (let ((new-data-id (incf *data-counter*)))
    (when data (setf (gethash new-data-id *data-tracker*) data))
    new-data-id))

(defun get-data (data-ptr) (gethash (mem-ref data-ptr :int) *data-tracker*))
(defun pop-data (data-ptr)
  (prog1 (get-data data-ptr)
    (remhash (mem-ref data-ptr :int) *data-tracker*)
    (foreign-free data-ptr)))

(defun data-ptr (data)
  (let ((data-ptr (foreign-alloc :int)))
    (setf (mem-ref data-ptr :int) data)
    data-ptr))

(defun set-data (index data) (setf (gethash index *data-tracker*) data))


;; ┬ ┬┌┬┐┬┬
;; │ │ │ ││
;; └─┘ ┴ ┴┴─┘

(defvar *debug* t)
(defmacro debug-log! (&rest args) (when *debug* `(format t ,@args)))
