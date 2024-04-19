
;; ██╗    ██╗ █████╗ ██╗   ██╗██╗      █████╗ ███╗   ██╗██████╗
;; ██║    ██║██╔══██╗╚██╗ ██╔╝██║     ██╔══██╗████╗  ██║██╔══██╗
;; ██║ █╗ ██║███████║ ╚████╔╝ ██║     ███████║██╔██╗ ██║██║  ██║
;; ██║███╗██║██╔══██║  ╚██╔╝  ██║     ██╔══██║██║╚██╗██║██║  ██║
;; ╚███╔███╔╝██║  ██║   ██║   ███████╗██║  ██║██║ ╚████║██████╔╝
;;  ╚══╝╚══╝ ╚═╝  ╚═╝   ╚═╝   ╚══════╝╚═╝  ╚═╝╚═╝  ╚═══╝╚═════╝
;; TODO: Maybe one day try to refer to the darn book: https://github.com/rcalixte/wayland-book
;; It is however also highly incomplete
(defpackage #:bm-cl-wayland
  (:use #:cl #:bm-cl-libwayland #:cffi)
  (:nicknames :wl)
  (:export display-create))
(in-package :bm-cl-wayland)

(defvar *client-tracker* (make-hash-table :test 'equal))

(defclass client ()
  ((objects :initform (make-hash-table :test 'equal) :accessor objects)))

(defclass object ()
  ((display :initarg :display :reader display)))

(defclass global (object) ())
(defgeneric create-global (global interface version))


;; ┌─┐┌─┐┬  ┌─┐┬  ┌─┐┌─┐┌─┐
;; ├┤ ├┤ │  │  │  ├─┤└─┐└─┐
;; └  └  ┴  └─┘┴─┘┴ ┴└─┘└─┘
(defpackage #:bm-cl-wayland.compositor
  (:use #:cl #:cffi)
  (:nicknames :wl-compositor))
(in-package #:bm-cl-wayland.compositor)

(defclass compositor (global)
  (create-surface :initarg :create-surface :reader create-surface)
  (create-region :initarg :create-region :reader create-region)
  (version :initform 4))

(defvar *interface* nil)
(defcstruct interface
  (create-surface :pointer)
  (create-region :pointer))

(cl-async::define-c-callback create-surface :void ((client :pointer) (resource :pointer) (id :uint))
  (format t "CREATE-SURFACE: ~a ~a ~a~%" client resource id))

(cl-async::define-c-callback create-region :void ((client :pointer) (resource :pointer) (id :uint))
  (format t "CREATE-REGION: ~a ~a ~a~%" client resource id))

(defmethod initialize-instance :after ((compositor compositor) &key)
  (unless *interface*
    (with-foreign-object (interface 'interface)
      (setf (foreign-slot-value interface '(:struct interface) 'create-surface) (callback create-surface))
      (setf (foreign-slot-value interface '(:struct interface) 'create-region) (callback create-region))
      (setf *interface* interface))))


;; ┌─┐─┐ ┬┌┬┐┌─┐┌┐┌┌─┐┬┌─┐┌┐┌  ┌─┐┬  ┌─┐┌─┐┌─┐
;; ├┤ ┌┴┬┘ │ ├┤ │││└─┐││ ││││  │  │  ├─┤└─┐└─┐
;; └─┘┴ └─ ┴ └─┘┘└┘└─┘┴└─┘┘└┘  └─┘┴─┘┴ ┴└─┘└─┘
(in-package :bm-cl-wayland)

(defclass compositor (bm-cl-wayland.compositor::compositor)
  ())

;; (defmethod initialize-instance ())

(defmethod create-global ((global global) interface version)
  (global-create (display global) ))
