
;; ███████╗██╗  ██╗ █████╗ ███╗   ███╗██████╗ ██╗     ███████╗
;; ██╔════╝╚██╗██╔╝██╔══██╗████╗ ████║██╔══██╗██║     ██╔════╝
;; █████╗   ╚███╔╝ ███████║██╔████╔██║██████╔╝██║     █████╗
;; ██╔══╝   ██╔██╗ ██╔══██║██║╚██╔╝██║██╔═══╝ ██║     ██╔══╝
;; ███████╗██╔╝ ██╗██║  ██║██║ ╚═╝ ██║██║     ███████╗███████╗
;; ╚══════╝╚═╝  ╚═╝╚═╝  ╚═╝╚═╝     ╚═╝╚═╝     ╚══════╝╚══════╝
;; TODO: This example is outdated. The example dispatch and global
;; could be extracted from one of the compiled protocol lisp files
;; TODO: The usage example is also not that great.
;; ┌─┐┌─┐┬  ┬┌─┐┌─┐┌─┐┌─┐
;; ├┤ ├┤ │  │├┤ ├─┤│  ├┤
;; └  └  ┴  ┴└  ┴ ┴└─┘└─┘
(defpackage #:bm-cl-wayland.example.compositor
  (:use #:cl #:cffi #:bm-cl-wayland)
  (:nicknames :wl-compositor))
(in-package #:bm-cl-wayland.example.compositor)
(defclass compositor (object) ()
  (:default-initargs :version 4))

(defgeneric create-surface (resource client id))
(defgeneric create-region (resource client id))

(defcstruct interface
  (create-surface :pointer)
  (create-region :pointer))
(defvar *interface*
  (with-foreign-object (interface 'interface)
    (setf (foreign-slot-value interface '(:struct interface) 'create-surface) (callback create-surface-ffi))
    (setf (foreign-slot-value interface '(:struct interface) 'create-region) (callback create-region-ffi))
    interface))

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
(defpackage #:bm-cl-wayland.example.compositor-global
  (:use #:cl #:cffi #:bm-cl-wayland)
  (:nicknames :wl-compositor-global))
(in-package #:bm-cl-wayland.example.compositor-global)

(defclass compositor (global) ()
  (:default-initargs :version 4))

(defmethod bind ((compositor compositor) client data version id)
  "Default bind implementation for the wl_compositor global object.
This can be overriden by inheritance in case if custom behaviour is required."
  (let ((global (make-instance 'bm-cl-wayland.example.compositor::compositor (display client))))
    (setf (iface client id) global)
    (create-resource client bm-cl-wayland.example.compositor::*interface* version id)))

(cl-async::define-c-callback bind-ffi :void ((client :pointer) (data :pointer) (version :uint) (id :uint))
  (let* ((client (get-client client))
	 (data (pop-data data))
	 (global (gethash data *global-tracker*)))
    (funcall 'bind global client (null-pointer) (mem-ref version :uint) (mem-ref id :uint))))

(defvar *bind* (callback bind-ffi))

(defmethod initialize-instance :after ((compositor compositor) &key)
  (let* ((next-data-id (reserve-data))
	 (global (global-create (display compositor) bm-cl-wayland.example.compositor::*interface* (version compositor) (data-ptr next-data-id) *bind*)))
    (set-data next-data-id (setf (gethash (global-get-name global) *global-tracker*) global))))

;; ┌─┐─┐ ┬┌┬┐┌─┐┌┐┌┌─┐┬┌─┐┌┐┌  ┌─┐┬  ┌─┐┌─┐┌─┐
;; ├┤ ┌┴┬┘ │ ├┤ │││└─┐││ ││││  │  │  ├─┤└─┐└─┐
;; └─┘┴ └─ ┴ └─┘┘└┘└─┘┴└─┘┘└┘  └─┘┴─┘┴ ┴└─┘└─┘
(defpackage :bm-cl-wayland.example
  (:use #:cl #:bm-cl-wayland))
(in-package :bm-cl-wayland.example)

(defclass compositor (bm-cl-wayland.example.compositor::compositor)
  ())

(defmethod bm-cl-wayland.example.compositor::create-surface ((compositor compositor) client resource id)
  (format t "CREATE-SURFACE: ~a ~a ~a~%" client resource id))

(defmethod bm-cl-wayland.example.compositor::create-region ((compositor compositor) client resource id)
  (format t "CREATE-REGION: ~a ~a ~a~%" client resource id))

(defun pretend-func ()
  (let ((display (display-create)))
    (let ((compositor (make-instance 'compositor :display display))))))
