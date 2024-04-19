
;; ██╗    ██╗ █████╗ ██╗   ██╗██╗      █████╗ ███╗   ██╗██████╗
;; ██║    ██║██╔══██╗╚██╗ ██╔╝██║     ██╔══██╗████╗  ██║██╔══██╗
;; ██║ █╗ ██║███████║ ╚████╔╝ ██║     ███████║██╔██╗ ██║██║  ██║
;; ██║███╗██║██╔══██║  ╚██╔╝  ██║     ██╔══██║██║╚██╗██║██║  ██║
;; ╚███╔███╔╝██║  ██║   ██║   ███████╗██║  ██║██║ ╚████║██████╔╝
;;  ╚══╝╚══╝ ╚═╝  ╚═╝   ╚═╝   ╚══════╝╚═╝  ╚═╝╚═╝  ╚═══╝╚═════╝
;; TODO: You might need to keep a list of all the interfaces that are actually globals.......
;; TODO: Maybe one day try to refer to the darn book: https://github.com/rcalixte/wayland-book
;; It is however also highly incomplete
(defpackage #:bm-cl-wayland
  (:use #:cl #:bm-cl-libwayland #:cffi)
  (:nicknames :wl)
  (:export display-create))
(in-package :bm-cl-wayland)

(defclass object ()
  ((display :initarg :display :reader display)
   (version :initarg :version :reader version)))

(defclass global (object) ())

;; ┌─┐┬  ┬┌─┐┌┐┌┌┬┐
;; │  │  │├┤ │││ │
;; └─┘┴─┘┴└─┘┘└┘ ┴
(defclass client ()
  ((objects :initform (make-hash-table :test 'equal) :accessor objects)))

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


;; ┌┬┐┌─┐┌┬┐┌─┐
;;  ││├─┤ │ ├─┤
;; ─┴┘┴ ┴ ┴ ┴ ┴

;; TODO: Need to create a way to insert incremental/random values here
;; TODO: Might not need to be a hash-table since i intend this to be ephemeral
(defvar *data-tracker* (make-hash-table :test 'equal))

(defun get-data (data-ptr) (gethash (mem-ref data-ptr :int) *data-tracker*))

;; ┌─┐┌─┐┬  ┌─┐┬  ┌─┐┌─┐┌─┐
;; ├┤ ├┤ │  │  │  ├─┤└─┐└─┐
;; └  └  ┴  └─┘┴─┘┴ ┴└─┘└─┘
(defpackage #:bm-cl-wayland.compositor
  (:use #:cl #:cffi)
  (:nicknames :wl-compositor))
(in-package #:bm-cl-wayland.compositor)

(defclass compositor (global) ()
  (:default-initargs :version 4))

(defgeneric create-surface (client resource id))
(defgeneric create-region (client resource id))
(defgeneric bind (client resource id))

(defvar *bind* nil)
(defvar *interface* nil)
(defcstruct interface
  (create-surface :pointer)
  (create-region :pointer))

(cl-async::define-c-callback bind-ffi :void ((client :pointer) (data :pointer) (version :uint) (id :uint))
  (let* ((client (get-client client))
	 (data (get-data data)))
    (funcall (bind client data (mem-ref version :uint) (mem-ref id :uint)))))

(cl-async::define-c-callback create-surface-ffi :void ((client :pointer) (resource :pointer) (id :uint))
  (format t "CREATE-SURFACE: ~a ~a ~a~%" client resource id))

(cl-async::define-c-callback create-region-ffi :void ((client :pointer) (resource :pointer) (id :uint))
  (format t "CREATE-REGION: ~a ~a ~a~%" client resource id))

(defmethod initialize-instance :after ((compositor compositor) &key)
  (unless *bind* (setf *bind* (callback bind-ffi)))
  (unless *interface*
    (with-foreign-object (interface 'interface)
      (setf (foreign-slot-value interface '(:struct interface) 'create-surface) (callback create-surface-ffi))
      (setf (foreign-slot-value interface '(:struct interface) 'create-region) (callback create-region-ffi))
      (setf *interface* interface)))
  (global-create (display compositor) *interface* (version compositor) (null-pointer) *bind*))


;; ┌─┐─┐ ┬┌┬┐┌─┐┌┐┌┌─┐┬┌─┐┌┐┌  ┌─┐┬  ┌─┐┌─┐┌─┐
;; ├┤ ┌┴┬┘ │ ├┤ │││└─┐││ ││││  │  │  ├─┤└─┐└─┐
;; └─┘┴ └─ ┴ └─┘┘└┘└─┘┴└─┘┘└┘  └─┘┴─┘┴ ┴└─┘└─┘
(in-package :bm-cl-wayland)

(defclass compositor (bm-cl-wayland.compositor::compositor)
  ())

(defun pretend-func ()
  (let ((display (display-create)))
    (let ((compositor (make-instance 'compositor :display display))))))
