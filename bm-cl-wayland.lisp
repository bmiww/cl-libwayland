
;; ██╗    ██╗ █████╗ ██╗   ██╗██╗      █████╗ ███╗   ██╗██████╗
;; ██║    ██║██╔══██╗╚██╗ ██╔╝██║     ██╔══██╗████╗  ██║██╔══██╗
;; ██║ █╗ ██║███████║ ╚████╔╝ ██║     ███████║██╔██╗ ██║██║  ██║
;; ██║███╗██║██╔══██║  ╚██╔╝  ██║     ██╔══██║██║╚██╗██║██║  ██║
;; ╚███╔███╔╝██║  ██║   ██║   ███████╗██║  ██║██║ ╚████║██████╔╝
;;  ╚══╝╚══╝ ╚═╝  ╚═╝   ╚═╝   ╚══════╝╚═╝  ╚═╝╚═╝  ╚═══╝╚═════╝
;; TODO: You might need to keep a list of all the interfaces that are actually globals.......
;; NOTE: Maybe one day try to refer to the darn book: https://github.com/rcalixte/wayland-book
;; NOTE: SWC compositor reference: https://github.com/michaelforney/swc
(defpackage #:bm-cl-wayland
  (:use #:cl #:bm-cl-libwayland #:cffi)
  (:nicknames :wl)
  (:export display-create create-client *global-tracker* resource-get-id))
(in-package :bm-cl-wayland)

(defclass object ()
  ((display :initarg :display :reader display)
   (version :initarg :version :reader version)))

(defvar *global-tracker* (make-hash-table :test 'equal))

(defclass global (object) ())
(defgeneric bind (client resource id))

(defun create-resource (client interface version id)
  ())

;; ┌─┐┬  ┬┌─┐┌┐┌┌┬┐
;; │  │  │├┤ │││ │
;; └─┘┴─┘┴└─┘┘└┘ ┴
(defclass client ()
  ((objects :initform (make-hash-table :test 'eq) :accessor objects)
   (display :initarg :display :reader display)))

(defvar *client-tracker* (make-hash-table :test 'equal))

(defun client-get-credentials (client)
  (with-foreign-objects ((pid :int) (uid :int) (gid :int))
    (wl-ffi::client-get-credentials client pid uid gid)
    (values (mem-aref pid :int) (mem-aref uid :int) (mem-aref gid :int))))

(defun get-client (client-ptr)
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
    (setf (gethash pid *client-tracker*) (make-instance 'client :display display))
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

(defun reserve-data () (incf *data-counter*))
(defun get-data (data-ptr) (gethash (mem-ref data-ptr :int) *data-tracker*))
(defun pop-data (data-ptr) (prog1 (get-data data-ptr) (remhash (mem-ref data-ptr :int) *data-tracker*)))
(defun data-ptr (data) (let ((ptr (make-pointer :int))) (setf (mem-ref ptr :int) data) ptr))
(defun set-data (index data) (setf (gethash index *data-tracker*) data))

;; ┌─┐┌─┐┬  ┬┌─┐┌─┐┌─┐┌─┐
;; ├┤ ├┤ │  │├┤ ├─┤│  ├┤
;; └  └  ┴  ┴└  ┴ ┴└─┘└─┘
(defpackage #:bm-cl-wayland.compositor
  (:use #:cl #:cffi #:bm-cl-wayland)
  (:nicknames :wl-compositor))
(in-package #:bm-cl-wayland.compositor)
(defclass compositor (object) ()
  (:default-initargs :version 4))

(defgeneric create-surface (resource client id))
(defgeneric create-region (resource client id))

(defvar *interface* nil)
(defcstruct interface
  (create-surface :pointer)
  (create-region :pointer))

(cl-async::define-c-callback create-surface-ffi :void ((client :pointer) (resource :pointer) (id :uint))
  (let ((client (get-client client))
	(resource (resource-get-id resource)))
    (funcall 'create-surface (iface client resource) client id)))

(cl-async::define-c-callback create-region-ffi :void ((client :pointer) (resource :pointer) (id :uint))
  (let ((client (get-client client))
	(resource (resource-get-id resource)))
    (funcall 'create-surface (iface client resource) client id)))

(defmethod initialize-instance :after ((compositor compositor) &key)
  (unless *interface*
    (with-foreign-object (interface 'interface)
      (setf (foreign-slot-value interface '(:struct interface) 'create-surface) (callback create-surface-ffi))
      (setf (foreign-slot-value interface '(:struct interface) 'create-region) (callback create-region-ffi))
      (setf *interface* interface))))

;; ┌─┐┌─┐┬  ┌─┐┬  ┌─┐┌┐ ┌─┐┬
;; ├┤ ├┤ │  │ ┬│  │ │├┴┐├─┤│
;; └  └  ┴  └─┘┴─┘└─┘└─┘┴ ┴┴─┘
(defpackage #:bm-cl-wayland.compositor-global
  (:use #:cl #:cffi #:bm-cl-wayland)
  (:nicknames :wl-compositor-global))
(in-package #:bm-cl-wayland.compositor-global)

(defclass compositor (global) ()
  (:default-initargs :version 4))

(defmethod bind ((compositor compositor) client data version id)
  "Default bind implementation for the wl_compositor global object.
This can be overriden by inheritance in case if custom behaviour is required."
  (let ((compositor (make-instance 'bm-cl-wayland.compositor::compositor (display client))))
    (setf (iface client id) compositor)
    (create-resource client bm-cl-wayland.compositor::*interface* version id)))

(cl-async::define-c-callback bind-ffi :void ((client :pointer) (data :pointer) (version :uint) (id :uint))
  (let* ((client (get-client client))
	 (data (pop-data data))
	 (compositor (gethash data *global-tracker*)))
    (funcall 'bind compositor client (null-pointer) (mem-ref version :uint) (mem-ref id :uint))))

(defvar *bind* (callback bind-ffi))

(defmethod initialize-instance :after ((compositor compositor) &key)
  (let* ((next-data-id (reserve-data))
	 (global (global-create (display compositor) *interface* (version compositor) (data-ptr next-data-id) *bind*)))
    (set-data next-data-id (setf (gethash (global-get-name global) *global-tracker*) global))))

;; ┌─┐─┐ ┬┌┬┐┌─┐┌┐┌┌─┐┬┌─┐┌┐┌  ┌─┐┬  ┌─┐┌─┐┌─┐
;; ├┤ ┌┴┬┘ │ ├┤ │││└─┐││ ││││  │  │  ├─┤└─┐└─┐
;; └─┘┴ └─ ┴ └─┘┘└┘└─┘┴└─┘┘└┘  └─┘┴─┘┴ ┴└─┘└─┘
(in-package :bm-cl-wayland)

(defclass compositor (bm-cl-wayland.compositor::compositor)
  ())

(defmethod create-surface ((compositor compositor) client resource id)
  (format t "CREATE-SURFACE: ~a ~a ~a~%" client resource id))

(defmethod create-region ((compositor compositor) client resource id)
  (format t "CREATE-REGION: ~a ~a ~a~%" client resource id))

(defun pretend-func ()
  (let ((display (display-create)))
    (let ((compositor (make-instance 'compositor :display display))))))
