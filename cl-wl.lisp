
;; ██╗    ██╗ █████╗ ██╗   ██╗██╗      █████╗ ███╗   ██╗██████╗
;; ██║    ██║██╔══██╗╚██╗ ██╔╝██║     ██╔══██╗████╗  ██║██╔══██╗
;; ██║ █╗ ██║███████║ ╚████╔╝ ██║     ███████║██╔██╗ ██║██║  ██║
;; ██║███╗██║██╔══██║  ╚██╔╝  ██║     ██╔══██║██║╚██╗██║██║  ██║
;; ╚███╔███╔╝██║  ██║   ██║   ███████╗██║  ██║██║ ╚████║██████╔╝
;;  ╚══╝╚══╝ ╚═╝  ╚═╝   ╚═╝   ╚══════╝╚═╝  ╚═╝╚═╝  ╚═══╝╚═════╝
(defpackage #:cl-wl
  (:use #:cl #:cffi #:wl-ffi)
  (:nicknames :wl)
  (:export create-client mk-if iface init-interface-definitions
	   object get-display client version id ptr destroy
	   global dispatch-impl
	   client objects get-display ptr rem-client
	   display dispatch-event-loop event-loop-fd flush-clients display-ptr all-clients destroy))

(in-package :cl-wl)

;; ┌┬┐┬┌─┐┌─┐┬  ┌─┐┬ ┬
;;  │││└─┐├─┘│  ├─┤└┬┘
;; ─┴┘┴└─┘┴  ┴─┘┴ ┴ ┴
(defvar *display-singleton* nil)
(defclass display ()
  ((ptr :accessor display-ptr)
   (socket-fd :initarg :fd :accessor socket-fd)
   (event-loop :accessor event-loop)
   (event-loop-fd :accessor event-loop-fd)
   (clients :accessor clients :initform (make-hash-table :test 'equal))))

(defmethod initialize-instance :before ((display display) &key)
  (restart-case (when *display-singleton* (error "There can only be one!... display."))
    (make-a-new-one () (setf *display-singleton* nil))))

(defmethod initialize-instance :after ((display display) &key)
  (setf (display-ptr display) (display-create))
  (display-add-socket-fd (display-ptr display) (socket-fd display))
  (setf (event-loop display) (display-get-event-loop (display-ptr display)))
  (setf (event-loop-fd display) (event-loop-get-fd (event-loop display)))
  (setf *display-singleton* display))

(defmethod dispatch-event-loop ((display display)) (event-loop-dispatch (event-loop display) 0))
(defmethod flush-clients ((display display)) (display-flush-clients (display-ptr display)))
(defmethod all-clients ((display display)) (alexandria:hash-table-values (clients display)))
(defmethod rem-client ((display display) client) (remhash (pid client) (clients display)))

;; TODO: This could also clean up some of the resources and close client connections
;; Gracefully. Maybe need to also do a notify for all globals/objects that they are being
;; destroyed.
(defmethod destroy ((display display))
  (display-destroy-clients (display-ptr display))
  (display-destroy (display-ptr display))
  (setf *display-singleton* nil))

;; ┌─┐┌┐  ┬┌─┐┌─┐┌┬┐
;; │ │├┴┐ │├┤ │   │
;; └─┘└─┘└┘└─┘└─┘ ┴
(defclass object ()
  ((display :initarg :display :reader get-display)
   (client :initarg :client :reader client)
   (version :initarg :version :reader version)
   (id :initarg :id :reader id)
   (ptr :initarg :ptr :accessor ptr)
   (destroy :initarg :destroy :accessor destroy)))

(defmethod mk-if (class (object object) id &rest args)
  "Convenience method to create a new interface using the context of the creating object as reference"
  (apply #'make-instance class :display (get-display object) :client (client object) :id id args))

;; Uses integer value pointer addresses as keys
;; TODO: Maybe clear this out once a client is destroyed or a restart is done
;; Might be unnecessary - since it's id based - and usually new restart ids will overwrite what is already here
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
   (display :initarg :display :reader get-display)
   (pid :initarg :pid :reader pid)
   (ptr :initarg :ptr :reader ptr)))

(defun client-get-credentials (client)
  (with-foreign-objects ((pid :int) (uid :int) (gid :int))
    (wl-ffi::client-get-credentials client pid uid gid)
    (values (mem-aref pid :int) (mem-aref uid :int) (mem-aref gid :int))))

(defun get-client (client)
  (let* ((pid (client-get-credentials client))
	 (client (gethash pid (clients *display-singleton*))))
    (unless client (error (format nil "No client found for pid ~a" pid)))
    client))

;; I have no idea what the second argument to this is. Wouldn't touch any more than necessary.
(cl-async-util:define-c-callback client-destroy-cb :void ((listener :pointer) (client :pointer))
  (declare (ignore listener))
  (let* ((client (gethash (pointer-address listener) *destroy-client-tracker*)))
    (rem-client (get-display client) client)
    (remhash (pointer-address listener) *destroy-client-tracker*)
    (foreign-free listener)))

(defvar *destroy-client-tracker* (make-hash-table :test 'eq))

(defun create-client (display fd &key (class 'client))
  "This function should be called when a new client connects to the socket.
This will in essence forward the client to the libwayland implementation
and set up the client object in the lisp world for further referencing."
  (let* ((client (client-create (display-ptr display) fd))
	 (pid (client-get-credentials client))
	 (destructo-struct (foreign-alloc '(:struct wl_listener))))

    (setf (foreign-slot-value destructo-struct '(:struct wl_listener) 'wl-ffi::link)
	  (foreign-alloc '(:struct wl_list)))
    (setf (foreign-slot-value destructo-struct '(:struct wl_listener) 'wl-ffi::notify)
	  (callback client-destroy-cb))

    (client-add-destroy-listener client destructo-struct)
    (let ((client (setf (gethash pid (clients display)) (make-instance class :display display :ptr client :pid pid))))
      (setf (gethash (pointer-address destructo-struct) *destroy-client-tracker*) client))))

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

;; ┌─┐┌┬┐┌─┐┌┬┐┬┌─┐  ┌─┐┌─┐┬  ┬┌┐┌┬┌┬┐
;; └─┐ │ ├─┤ │ ││    ├┤ ├┤ │  │││││ │
;; └─┘ ┴ ┴ ┴ ┴ ┴└─┘  └  └  ┴  ┴┘└┘┴ ┴
;; Keeps track of protocol global dispatch signatures required by libwayland
(defvar *interface-init-list* nil)
(defvar *inited-interfaces* nil)
(defun interface-exists-test (a b) (string= (car a) (car b)))
(defun hierarchical-init (interfaces)
  (let ((needs-processing nil))
    (dolist (interface interfaces)
      (let ((name (car interface)) (deps (cadr interface)) (init (caddr interface)))
	(when (not (find name *inited-interfaces* :test #'string=))
	  (if (every (lambda (dep)
		       (if (string= dep name) t
			   (find dep *inited-interfaces* :test #'string=)))
		     deps)
	      (progn (funcall init) (push name *inited-interfaces*))
	      (push interface needs-processing)))))
    (when (eq (length needs-processing) (length interfaces))
      (error "Circular dependency detected in interface initialization"))
    (when needs-processing (hierarchical-init needs-processing))))

(defun init-interface-definitions ()
  (when *interface-init-list*
    (hierarchical-init *interface-init-list*) (setf *interface-init-list* nil)))

;; ┬ ┬┌┬┐┬┬
;; │ │ │ ││
;; └─┘ ┴ ┴┴─┘
(defvar *debug* nil)
(defmacro debug-log! (&rest args) (when *debug* `(format t "⭐: ~a" (format nil ,@args))))
