
;;  ██████╗ ██████╗ ███╗   ███╗██████╗ ██╗██╗     ███████╗██████╗
;; ██╔════╝██╔═══██╗████╗ ████║██╔══██╗██║██║     ██╔════╝██╔══██╗
;; ██║     ██║   ██║██╔████╔██║██████╔╝██║██║     █████╗  ██████╔╝
;; ██║     ██║   ██║██║╚██╔╝██║██╔═══╝ ██║██║     ██╔══╝  ██╔══██╗
;; ╚██████╗╚██████╔╝██║ ╚═╝ ██║██║     ██║███████╗███████╗██║  ██║
;;  ╚═════╝ ╚═════╝ ╚═╝     ╚═╝╚═╝     ╚═╝╚══════╝╚══════╝╚═╝  ╚═╝
;; NOTE: Example invocations
;; (generate-wayland-classes 'wayland-core "/usr/share/wayland/wayland.xml")
;; (generate-wayland-classes 'xdg-shell "xmls/xdg-shell.xml" :deps '("wayland-core"))
;; TODO: Get rid of the cl-async dependency. defccallback is the only thing we use from it.

(defpackage :bm-cl-wayland.compiler
  (:use :cl :xmls :bm-cl-wayland.parser))
(in-package :bm-cl-wayland.compiler)

(defun gen-request-c-struct (request) `(,(symbolify (name request)) :pointer))
(defun gen-generic-arg (arg) (symbolify (name arg)))
(defun gen-request-generic (request)
  `(defgeneric ,(symbolify (dash-name request)) (resource ,@(mapcar 'gen-generic-arg (args request)))))

(defun gen-dispatch-init ()
  `((defmethod initialize-instance :after ((instance dispatch) &key)
     ;; Bound is instance
     ;; Maybe we can move the hash-table insertion to the constructor here
     (let* ((resource (create-resource (ptr (client instance)) ,(symbolify "*interface*")
				       (version instance) (id instance))))
       (setf (gethash (pointer-address resource) *resource-tracker*) instance)
       (setf (ptr instance) resource)
       (resource-set-dispatcher resource ,(symbolify "*dispatcher*") (null-pointer) (null-pointer) (null-pointer))))))


(defun gen-bind-callback (interface)
  `((defmethod dispatch-bind ((global global) client data version id)
      ,(format nil "Default bind implementation for the ~a global object.
This can be overriden by inheritance in case if custom behaviour is required." (name interface))
      (debug-log! "Binding ~a~%" ,(name interface))
      (let ((bound (make-instance (dispatch-impl global) :display (display client) :client client :id id)))
	(setf (iface client id) bound)))))

(defun gen-bind-c-callback (interface)
  `((eval-when (:compile-toplevel :load-toplevel :execute)
      (cl-async::define-c-callback dispatch-bind-ffi :void ((client :pointer) (data :pointer) (version :uint) (id :uint))
	(debug-log! "C-Binding ~a~%" ,(name interface))
	(let* ((client (get-client client))
	       (global (get-data data)))
	  (funcall 'dispatch-bind global client (null-pointer) version id))))))

(defun gen-interface-var-fill (interface)
  `((eval-when (:load-toplevel :execute)
      (setf (foreign-slot-value *interface* '(:struct interface) 'name) (foreign-string-alloc ,(name interface))
	    (foreign-slot-value *interface* '(:struct interface) 'version) ,(version interface)
	    (foreign-slot-value *interface* '(:struct interface) 'method_count) ,(length (requests interface))
	    (foreign-slot-value *interface* '(:struct interface) 'methods) ,(symbolify "*requests*")
	    (foreign-slot-value *interface* '(:struct interface) 'event_count) ,(length (events interface))
	    (foreign-slot-value *interface* '(:struct interface) 'events) ,(symbolify "*events*")))))

(defun gen-c-struct-filler (var-name methods)
  `((eval-when (:load-toplevel :execute)
      (defvar ,var-name
	(let ((messages (cffi:foreign-alloc '(:struct wl_message)
					    :count ,(length methods))))
	  ,@(mapcar
	     (lambda (method)
	       `(let ((interface-array (cffi:foreign-alloc '(:pointer (:pointer :void))
							   :count ,(length (args method)))))
		  ,@(append
		     ;; Code to fill the interface array with references to interface definitions
		     (loop for index below (length (args method))
			   collect (let ((arg (nth index (args method))))
				     `(setf (mem-aref interface-array :pointer ,index)
					    ,(if (interface arg)
						 (symbolify "~a::*interface*" (interface arg))
						 `(null-pointer)))))
		     `((setf (foreign-slot-value messages '(:struct wl_message) 'wl-ffi::name)
			     (foreign-string-alloc ,(name method))

			     (foreign-slot-value messages '(:struct wl_message) 'wl-ffi::signature)
			     ;; TODO: Do signature - you already started in the parser
			     (foreign-string-alloc ,(signature method))

			     (foreign-slot-value messages '(:struct wl_message) 'wl-ffi::types)
			     ;; TODO: This should instead be a reference to an interface.
			     ;; The interface in question could actually span different packages. So a pain in the neck.
			     interface-array)))))
	     methods)
	  messages)))))

;; TODO: This is a bit annoying - since it loosly refers to the args symbol
(defun gen-c-arg-selector (arg index)
  `(foreign-slot-value
    (mem-aptr args '(:union wl-ffi:wl_argument) ,index)
    ;; args
    '(:union wl-ffi:wl_argument)
    ,(symbolify "'wl-ffi::~a" (arg-type-char arg))))

;; TODO: The values functions are a bit dumb. They act just as rudimentary id function
;; But could be completely omitted if i wasn't too lazy to figure out a different way of doing this
(defun gen-c-arg-mapping (arg index)
  (let ((selector (gen-c-arg-selector arg index)))
    (alexandria:eswitch ((arg-type arg) :test 'string=)
      ("int" `(values ,selector))
      ("uint" `(values ,selector))
      ("new_id" `(values ,selector))
      ("fixed" `(values ,selector))
      ("fd" `(values ,selector))
      ("string" `(values ,selector))
      ("enum" `(error "WL C enum not yet implemented. You wanted to create a lisp list with keywords"))
      ("object" `(gethash (pointer-address ,selector) *resource-tracker*))
      ;; TODO: You don't know yet how to handle the darn arrays - so just error out here :)
      ("array" `(error "WL C ARRAY PARSING NOT IMPLEMENTED")))))

;; TODO: This is a bit annoying - since it loosly refers to the symbol "resource"
(defun gen-matcher (opcode request)
  "A case form to match an opcode to a method to be called upon a resource and it's
argument feed."
  `(,opcode (funcall ',(symbolify (dash-name request)) resource
		     ,@(loop for index below (length (args request))
			     for arg = (nth index (args request))
			     collect (gen-c-arg-mapping arg index)))))


;; NOTE: An implementation of this in the python lib
;; https://github.com/flacjacket/pywayland/blob/4febe9b7c22b61ace7902109904f2a298c510169/pywayland/dispatcher.py#L28
;; NOTE: And on wayland side
;; https://gitlab.freedesktop.org/wayland/wayland/-/blob/main/src/wayland-util.h#L690
;; TODO: The target here could also be wl_proxy - but this seems to be only client side stuff
(defun gen-dispatcher-c-callback (interface)
  (let* ((arg-usage 0)
	 (matchers (loop for index below (length (requests interface))
			 for request = (nth index (requests interface))
			 do (incf arg-usage (length (args request)))
			 collect (gen-matcher index request)))
	 (ignore-list (if (requests interface)
			  (if (= 0 arg-usage) '(args) nil)
			  '(args target opcode))))

    `((eval-when (:compile-toplevel :load-toplevel :execute)
	(cl-async::define-c-callback dispatcher-ffi :int
	    ((data :pointer) (target :pointer) (opcode :uint) (message :pointer) (args :pointer))
	  (declare (ignore data message ,@ignore-list))
	  (debug-log! "Dispatcher invoked: ~a~%" ,(name interface))
	  ,(if (requests interface)
	       `(let ((resource (gethash (pointer-address target) *resource-tracker*)))
		  (ecase opcode ,@matchers))
	       `(error (format nil "A dispatcher wiwthout requests has been called for interface: ~a~%" ,(name interface))))
	  0)))))


(defun gen-interface-c-structs (interface)
  (append
   (gen-c-struct-filler (symbolify "*requests*") (requests interface))
   (gen-c-struct-filler (symbolify "*events*") (events interface))
   (gen-interface-var-fill interface)))

(defun gen-global-init (interface)
  `((defmethod initialize-instance :after ((global global) &key)
      (debug-log! "Initializing global object: ~a~%" ,(name interface))
      (let* ((next-data-id (reserve-data))
	     (global-ptr (global-create (display global) *interface*
					(version global) (data-ptr next-data-id)
					*dispatch-bind*)))
	(setf (wl:ptr global) global-ptr)
	;; TODO: not sure i really need to keep track of the globals.
	;; The *global-tracker* set might be unnecessary
	(set-data next-data-id (setf (gethash (pointer-address global-ptr) *global-tracker*) global))))))

(defun gen-c-arg-setter-inner (arg index value-form)
  `(setf
    (foreign-slot-value
     (mem-aref arg-list '(:union wl-ffi:wl_argument) ,index)
     '(:union wl-ffi:wl_argument)
     ,(symbolify "'wl-ffi::~a" (arg-type-char arg)))
    ,value-form))

(defun gen-c-arg-setter (arg index)
  (gen-c-arg-setter-inner
   arg index
   (alexandria:eswitch ((arg-type arg) :test 'string=)
     ("int" (symbolify (name arg)))
     ("uint" (symbolify (name arg)))
     ("new_id" (symbolify (name arg)))
     ("fixed" (symbolify (name arg)))
     ("fd" (symbolify (name arg)))
     ("string" (symbolify (name arg)))
     ;; TODO: You wanted to create a lisp list with keywords. For now just leaving as is/or as uint
     ;; ("enum" `(error "WL C enum not yet implemented. You wanted to create a lisp list with keywords"))
     ("enum" (symbolify (name arg)))
     ("object" `(wl:ptr ,(symbolify (name arg))))
     ("array" `(error "WL C ARRAY PARSING NOT IMPLEMENTED")))))

(defun gen-event (event opcode)
  `(defmethod ,(symbolify "send-~a" (dash-name event)) ((dispatch dispatch) ,@(mapcar 'gen-generic-arg (args event)))
     (debug-log! "Event: ~a~%" ,(name event))
     (let ((arg-list (foreign-alloc '(:union wl_argument) :count ,(length (args event)))))
       ,@(loop for index below (length (args event))
	       for arg = (nth index (args event))
	       collect (gen-c-arg-setter arg index))
       (resource-post-event-array (ptr dispatch) ,opcode arg-list))))

(defun gen-events (interface)
  (let ((events (events interface)))
    (if events
	(loop for index below (length events)
	      for event = (nth index events)
	      collect (gen-event event index))
	nil)))

(defun pkg-name (interface)
  (symbolify ":~a" (name interface)))

(defun gen-interface (interface)
  (let ((pkg-name (pkg-name interface)))
    (append
     `((in-package ,pkg-name))
     `((defclass dispatch (wl:object) ()
	 (:default-initargs :version ,(version interface))
	 (:documentation ,(description interface))))
     (mapcar 'gen-request-generic (requests interface))
     (gen-interface-c-structs interface)
     (gen-dispatcher-c-callback interface)
     `((eval-when (:load-toplevel :execute)
	 (defvar *dispatcher* (callback ,(symbolify "dispatcher-ffi")))))
     (gen-dispatch-init)
     (gen-events interface)

     `((defclass global (wl::global) ()
	 (:default-initargs :version ,(version interface) :dispatch-impl 'dispatch)
	 (:documentation ,(description interface))))
     (gen-bind-callback interface)
     (gen-bind-c-callback interface)
     `((eval-when (:load-toplevel :execute)
	 (defvar *dispatch-bind* (callback ,(symbolify "dispatch-bind-ffi")))))
     (gen-global-init interface))))

;; NOTE: Thing could be an interface/request/event/arg
(defun dash-name (thing) (str:replace-all "_" "-" (name thing)))

(defun gen-interface-preamble (interface)
  (let ((pkg-name (pkg-name interface)))
    (append
     `((defpackage ,pkg-name
	 (:use :cl :wl :cffi)
	 (:nicknames ,(symbolify ":~a" (dash-name interface)))
	 (:export dispatch global dispatch-bind
		  ,@(mapcar 'symbolify (mapcar 'dash-name (requests interface)))
		  ,@(mapcar (lambda (event) (symbolify "send-~a" (dash-name event))) (events interface)))))
     `((in-package ,pkg-name))
     `((defcstruct interface
	 (name :string)
	 (version :int)
	 (method_count :int)
	 (methods (:pointer (:struct wl_message)))
	 (event_count :int)
	 (events (:pointer (:struct wl_message)))
	 ,@(mapcar 'gen-request-c-struct (requests interface))))
     `((eval-when (:load-toplevel :execute)
	 (defvar *interface* (cffi:foreign-alloc '(:struct interface))))))))

(defun gen-code (protocol)
  (append
   (apply #'append (mapcar (lambda (part) (gen-interface-preamble part)) protocol))
   (apply #'append (mapcar (lambda (part) (gen-interface part)) protocol))))

(defun defpackages-during-compilation (name)
  (let* ((package-name (symbolify "~a" name))
	 (existing-pkg (find-package package-name)))
    (if existing-pkg
	existing-pkg
	(make-package package-name))))

(defun gen-asd (package file deps)
  `((asdf:defsystem ,(symbolify "#:bm-cl-wayland.~a" package)
     :serial t
     :license "GPLv3"
     :version "0.0.1"
     :depends-on (#:cffi #:cl-async #:bm-cl-wayland ,@(mapcar (lambda (dep) (symbolify "#:bm-cl-wayland.~a" dep)) deps))
     :components ((:file ,file)))))

(defun generate-wayland-classes (package xml-file &key (deps nil))
  (dolist (dep deps) (asdf:load-system (symbolify "#:bm-cl-wayland.~a" dep)))
  (let* ((xml (with-open-file (s xml-file :if-does-not-exist :error) (xmls:parse s)))
	 (protocol (read-protocol xml))
	 ;; NOTE: Required so that we can compile cross package dependencies
	 (pkg-names (mapcar 'name protocol))
	 (pkgs (mapcar 'defpackages-during-compilation pkg-names))
	 (code (gen-code protocol))
	 (file (format nil "bm-cl-wayland.~a" (string-downcase package)))
	 (asd (gen-asd package file deps)))
    (with-open-file (stream (format nil "~a.lisp" file) :direction :output :if-exists :supersede)
      (loop :for xep :in code
	    :do (format stream "~s~%~%" xep)))
    (with-open-file (stream (format nil "~a.asd" file) :direction :output :if-exists :supersede)
      (loop :for xep :in asd
	    :do (format stream "~s~%~%" xep)))
    (mapcar 'delete-package pkgs)
    t))

;; ┬ ┬┌┬┐┬┬
;; │ │ │ ││
;; └─┘ ┴ ┴┴─┘

(defun symbolify (&rest args)
  (read-from-string (apply 'format `(nil ,@args))))

(defun nump (s)
  "Return t if `s' contains at least one character and all characters are numbers."
  (ppcre:scan "^[0-9]+$" s))

(defun flatten (list)
  (let ((result nil))
    (dolist (sublist list)
      (dolist (x sublist)
	(push x result)))
    (nreverse result)))

(defun getf-string-equal (plist indicator)
  (loop
     for (i v) on plist by #'cddr
     when (string-equal i indicator)
     return v))
