
;; ██╗    ██╗ █████╗ ██╗   ██╗██╗      █████╗ ███╗   ██╗██████╗
;; ██║    ██║██╔══██╗╚██╗ ██╔╝██║     ██╔══██╗████╗  ██║██╔══██╗
;; ██║ █╗ ██║███████║ ╚████╔╝ ██║     ███████║██╔██╗ ██║██║  ██║
;; ██║███╗██║██╔══██║  ╚██╔╝  ██║     ██╔══██║██║╚██╗██║██║  ██║
;; ╚███╔███╔╝██║  ██║   ██║   ███████╗██║  ██║██║ ╚████║██████╔╝
;;  ╚══╝╚══╝ ╚═╝  ╚═╝   ╚═╝   ╚══════╝╚═╝  ╚═╝╚═╝  ╚═══╝╚═════╝
(defpackage #:cl-wl
  (:use #:cl #:cffi #:wl-ffi)
  (:nicknames :wl)
  (:export create-client mk-if up-if iface init-interface-definitions
	   object get-display client version id ptr destroy add-destroy-callback
	   global dispatch-impl
	   client objects get-display ptr rem-client
	   display dispatch-event-loop event-loop-fd flush-clients display-ptr all-clients destroy))

(in-package :cl-wl)

;; ┌┬┐┬─┐┌─┐┌─┐┬┌─┌─┐┬─┐┌─┐
;;  │ ├┬┘├─┤│  ├┴┐├┤ ├┬┘└─┐
;;  ┴ ┴└─┴ ┴└─┘┴ ┴└─┘┴└─└─┘
;; Global tables to help deal with the libwayland - pointing back and forth problem
(defvar *display-singleton* nil)
;; Uses integer value pointer addresses as keys
;; TODO: Maybe clear this out once a client is destroyed or a restart is done
;; Might be unnecessary - since it's id based - and usually new restart ids will overwrite what is already here
(defvar *global-tracker* (make-hash-table :test 'eq))
;; TODO: Might not need to be a hash-table since i intend this to be ephemeral
;; TODO: Also - seems to keeping around objects - doesn't have to be this way
(defvar *data-tracker* (make-hash-table :test 'equal))
(defvar *data-counter* 0)
;; Keeps a table of addresses for destruction listener objects connected to particular resources
(defvar *destroy-tracker* (make-hash-table :test 'eq))
;; TODO: Multi inheritance is still keeping objects around here. The destroy :after for different
;; classes doesn't seem to be working out
(defvar *resource-tracker* (make-hash-table :test 'eq))

(defun remove-resource (pointer)
  (if pointer
      (remhash pointer *resource-tracker*)
      (format t "Resource already removed~a~%" pointer)))

;; ┌┬┐┬┌─┐┌─┐┬  ┌─┐┬ ┬
;;  │││└─┐├─┘│  ├─┤└┬┘
;; ─┴┘┴└─┘┴  ┴─┘┴ ┴ ┴
(defclass display ()
  ((ptr :accessor display-ptr)
   (socket-fd :initarg :fd :accessor socket-fd)
   (event-loop :accessor event-loop)
   (event-loop-fd :accessor event-loop-fd)
   (clients :accessor clients :initform (make-hash-table :test 'equal))))

(defmethod initialize-instance :before ((display display) &key)
  (restart-case (when *display-singleton* (error "There can only be one!... display."))
    (make-a-new-one ()
      (setf *display-singleton* nil))))

(defmethod initialize-instance :after ((display display) &key)
  (init-interface-definitions)
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
  (setf *resource-tracker* (make-hash-table :test 'eq))
  (setf *display-singleton* nil)
  (setf *global-tracker* (make-hash-table :test 'eq))
  (setf *data-tracker* (make-hash-table :test 'equal))
  (setf *data-counter* 0))

;; ┌─┐┌┐  ┬┌─┐┌─┐┌┬┐
;; │ │├┴┐ │├┤ │   │
;; └─┘└─┘└┘└─┘└─┘ ┴
(defclass object ()
  ((display :initarg :display :reader get-display)
   (client :initarg :client :reader client)
   (global :initarg :global :reader global)
   (version :initarg :version :reader version)
   ;; TODO: This one is now for the most part moved to ${interface-name}-id
   ;; There are still some stragglers
   ;; Might be possible to remove this
   (id :initarg :id)
   (destroy :initform nil :accessor destroy-callback)))

(defmethod add-destroy-callback ((object object) callback)
  (setf (destroy-callback object) (push callback (destroy-callback object))))

;; NOTE: Empty implementation - since the dispatch object implementations are supposed to connect :after
(defgeneric destroy (object))
(defmethod destroy ((object object)))

(defmethod id ((object object)) (slot-value object 'id))
(defmethod (setf id) (new-id (object object)) (setf (slot-value object 'id) new-id))


(defmethod mk-if (class (object object) id &rest args)
  "Convenience method to create a new interface using the context of the creating object as reference.
Reuses the display and client fields of the reference object.
Created object also gets added to the client object tracking hash-table."
  (setf (gethash id (objects (client object)))
	(apply #'make-instance class :display (get-display object) :client (client object) :id id args)))

;; TODO: The compiled classes also need to handle multiple pointers. I'd assume.
;; Not sure i've used pointers an awful lot yet.
;; Maybe i can instead have a pointer reader that retrieves the pointer based on the id of the object
(defmethod up-if (class (object object) id &rest args)
  "Convenience method to update an existing interface using the context of the creating object as reference.
Reuses the display and client fields of the reference object.
Created object also gets added to the client object tracking hash-table."
  (let ((new-obj (apply #'change-class object class :id id args)))
    (setf (gethash id (objects (client object))) new-obj)))

(defclass global (object)
  ((dispatch-impl :initarg :dispatch-impl :reader dispatch-impl)
   (ptr :initarg :ptr :accessor ptr)))

(defgeneric bind (client resource id))

;; I have no idea what the second argument to this is. Wouldn't touch any more than necessary.
(defcallback resource-destroy-cb :void ((listener :pointer) (data :pointer))
  (declare (ignore data))
  (let* ((resource-ptr (gethash (pointer-address listener) *destroy-tracker*)))
    (remove-resource resource-ptr)
    (foreign-free resource-ptr)
    (remhash (pointer-address listener) *destroy-tracker*)
    (foreign-free listener)))

(defun create-resource (client interface version id)
  (resource-create client interface version id))

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
    ;; TODO: Somehow the pid isn't always there and reliable. Need to figure out why.
    ;; Prime example is firefox, which does something weird
    (unless client (error (format nil "No client found for pid ~a" pid)))
    client))

;; I have no idea what the second argument to this is. Wouldn't touch any more than necessary.
(defcallback client-destroy-cb :void ((listener :pointer) (data :pointer))
  (declare (ignore data))
  (let* ((client (gethash (pointer-address listener) *destroy-tracker*)))
    (clear-client-objects client)
    (rem-client (get-display client) client)
    (remhash (pointer-address listener) *destroy-tracker*)
    (foreign-free listener)))

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
      (setf (gethash (pointer-address destructo-struct) *destroy-tracker*) client))))

(defmethod iface ((client client) id)
  (let ((iface (gethash id (objects client))))
    (unless iface (error (format nil "No interface found for object id: ~a" id)))
    iface))

(defmethod (setf iface) (iface (client client) id)
  (setf (gethash id (objects client)) iface))

(defmethod clear-client-objects ((client client))
  (maphash (lambda (id iface)
	     (declare (ignore id))
	     (destroy iface))
	   (objects client)))

;; ┌┬┐┌─┐┌┬┐┌─┐
;;  ││├─┤ │ ├─┤
;; ─┴┘┴ ┴ ┴ ┴ ┴
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

;; Fixnum conversion
;; NOTE: These are all assuming a Q24.8 fixed point format that wayland protocol uses
;; Signed fixed number - 1 bit denoting the sign with 23 bits for the integer part and
;; 8 bits for the fractional part
;; The wayland conversion code was just this simple
(defun to-fixnum (number) (coerce (floor (* number 256.0)) 'integer))
(defun from-fixnum (number) (/ number 256.0))
