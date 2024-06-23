
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
	   object get-display client version version-want id ptr destroy add-destroy-callback
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
      (progn
	;; (let ((stuff (gethash pointer *resource-tracker*)))
	  ;; (when (typep stuff 'smuks::popup)
	    ;; (format t "Removing ~a~%" pointer)
	    ;; (format t "XDG_POPUP-PTR: ~a~%" (pointer-address  (xdg-popup::xdg_popup-ptr stuff)))
	    ;; (format t "XDG_SURFACE-PTR: ~a~%" (pointer-address (xdg-surface::xdg_surface-ptr stuff)))
	    ;; (format t "WL_SURFACE-PTR: ~a~%" (pointer-address (wl-surface::wl_surface-ptr stuff)))
	    ;; (describe stuff)
	    ;; (when (eq (pointer-address (wl-surface::wl_surface-ptr stuff)) pointer)
	      ;; (break))))
	(remhash pointer *resource-tracker*))
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
(defmethod rem-client ((display display) client)
  (format t "REMOVING CLIENT ~a~%" (pointer-address (ptr client)))
  (remhash (pointer-address (ptr client)) (clients display)))

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
   (upgrade-hierarchy :initform nil :accessor upgrade-hierarchy)
   (version-want :initarg :version-want :reader version-want)
   (version :initarg :version :reader version)
   ;; TODO: This one is now for the most part moved to ${interface-name}-id
   ;; There are still some stragglers
   ;; Might be possible to remove this
   (id :initarg :id)
   ;; NOTE: I'm using this one to be able to grab the latest id attached to a resource
   ;; It should by no means be used in runtime since class upgrades will change it.
   ;; Used in mk-if
   (transient-id :initarg :transient-id :accessor transient-id)
   (destroy :initform nil :accessor destroy-callback)))

(defmethod add-destroy-callback ((object object) callback)
  (setf (destroy-callback object) (push callback (destroy-callback object))))

;; NOTE: Empty implementation - since the dispatch object implementations are supposed to connect :after
;; TODO: The basic implementations could also do regular overrides instead of :after
;; Although - i think the specific smuks implementation is already overriding. A bit awkward.
(defgeneric destroy (object))
(defmethod destroy ((object object)))

(defmethod id ((object object)) (slot-value object 'id))
(defmethod (setf id) (new-id (object object)) (setf (slot-value object 'id) new-id))


(defmethod mk-if (class (object object) id &rest args)
  "Convenience method to create a new interface using the context of the creating object as reference.
Reuses the display and client fields of the reference object.
Created object also gets added to the client object tracking hash-table."
  (let* ((version (getf args :version-want))
	 (object (apply #'make-instance class :display (get-display object)
				  :client (client object) :id id
				  :version-want (or version (version-want object))
				  args)))
    (setf (upgrade-hierarchy object) (list class))
    (setf (gethash (transient-id object) (objects (client object))) object)))

(defmethod up-if (class (object object) id &rest args)
  "Convenience method to update an existing interface using the context of the creating object as reference.
Reuses the display and client fields of the reference object.
Created object also gets added to the client object tracking hash-table."
  (setf (upgrade-hierarchy object) (cons class (upgrade-hierarchy object)))
  (let ((new-obj (apply #'change-class object class :id id args)))
    (setf (gethash id (objects (client object))) new-obj)))

(defmethod dn-if ((object object))
  "Convenience method to downgrade an existing interface to it's previous class type"
  (let* ((downgrade (member (class-name (class-of object)) (upgrade-hierarchy object))))
    (when (cadr downgrade)
      (setf (upgrade-hierarchy object) (cdr downgrade))
      (funcall #'change-class object (cadr downgrade)))))

(defclass global (object)
  ((dispatch-impl :initarg :dispatch-impl :reader dispatch-impl)
   (ptr :initarg :ptr :accessor ptr)))

(defgeneric bind (client resource id))

;; I have no idea what the second argument to this is. Wouldn't touch any more than necessary.
(defcallback resource-destroy-cb :void ((listener :pointer) (data :pointer))
  (declare (ignore data))
  (let* ((resource-ptr (gethash (pointer-address listener) *destroy-tracker*)))
    (remove-resource resource-ptr)
    ;; TODO: For now going to remove this foreign-free
    ;; since i'm worried that i might sometimes be prematurely freeing resources
    ;; (foreign-free resource-ptr)
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
   ;; TODO: Possibly removable - trying to use PTR instead now
   (pid :initarg :pid :reader pid)
   (ptr :initarg :ptr :reader ptr)))

;; TODO: Possibly removable - trying to use PTR instead now
(defun client-get-credentials (client)
  (with-foreign-objects ((pid :int) (uid :int) (gid :int))
    (wl-ffi::client-get-credentials client pid uid gid)
    (values (mem-aref pid :int) (mem-aref uid :int) (mem-aref gid :int))))

(defun get-client (client-ptr)
  (let* ((client (gethash (pointer-address client-ptr) (clients *display-singleton*))))
    (unless client (format t "No client found for ptr ~a~%" (pointer-address client-ptr)))
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
	 (destructo-struct (foreign-alloc '(:struct wl_listener))))

    (setf (foreign-slot-value destructo-struct '(:struct wl_listener) 'wl-ffi::link)
	  (foreign-alloc '(:struct wl_list)))
    (setf (foreign-slot-value destructo-struct '(:struct wl_listener) 'wl-ffi::notify)
	  (callback client-destroy-cb))

    (client-add-destroy-listener client destructo-struct)
    (format t "Creating client with ptr ~a~%" (pointer-address client))
    (let ((client (setf (gethash (pointer-address client) (clients display)) (make-instance class :display display :ptr client))))
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
