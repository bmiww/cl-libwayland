
;;  ██████╗ ██████╗ ███╗   ███╗██████╗ ██╗██╗     ███████╗██████╗
;; ██╔════╝██╔═══██╗████╗ ████║██╔══██╗██║██║     ██╔════╝██╔══██╗
;; ██║     ██║   ██║██╔████╔██║██████╔╝██║██║     █████╗  ██████╔╝
;; ██║     ██║   ██║██║╚██╔╝██║██╔═══╝ ██║██║     ██╔══╝  ██╔══██╗
;; ╚██████╗╚██████╔╝██║ ╚═╝ ██║██║     ██║███████╗███████╗██║  ██║
;;  ╚═════╝ ╚═════╝ ╚═╝     ╚═╝╚═╝     ╚═╝╚══════╝╚══════╝╚═╝  ╚═╝
;; NOTE: Example invocations
;; (generate-wayland-classes 'wayland-core "/usr/share/wayland/wayland.xml" :namespace "wl")
;; (generate-wayland-classes 'xdg-shell "protocol/xdg-shell.xml" :namespace "xdg")

(defpackage :bm-cl-wayland.compiler
  (:use :cl :xmls :bm-cl-wayland.parser))
(in-package :bm-cl-wayland.compiler)

;; TODO: Perhaps you can instead just generate a global for everything.
;; Might be a bit confusing - but at least wouldn't have to keep a list of these things
(defvar *global-interfaces*
  '("wl_registry"
    "wl_compositor"
    "wl_subcompositor"
    "wl_shm"
    "wl_data_device"
    "wl_data_device_manager"
    "wl_seat"
    "wl_output"))

(defun ev-name (event) (read-from-string (format nil "evt-~a" (name event))))
(defun req-name (request) (read-from-string (format nil "req-~a" (name request))))
(defun enum-name (enum) (read-from-string (format nil "enum-~a" (name enum))))
(defun enum-val-name (enum) (read-from-string (format nil "enum-~a-value" (name enum))))
(defun symbolize-event (event) (ev-name event))
(defun symbolize-request (request) (req-name request))
(defun do-arg (arg) (read-from-string (name arg)))
(defun arg-type-symbol (arg)
  (if (enum arg)
      (read-from-string (format nil "(~a ~a)" (arg-type arg) (enum arg)))
      (read-from-string (format nil "~a" (arg-type arg)))))

(defun safe-enum-symbol (entry enum)
  "For enum names that are not valid symbols, we prepend them with the enum name"
  (let ((name (name entry)))
    (read-from-string (format nil "~a~a"
			      (if (nump name) (format nil "~a_" (name enum)) "")
			      name))))

(defun do-event (interface event)
  `(defmethod ,(ev-name event) ((obj ,(read-from-string interface))
			 stream
			 ,@(mapcar 'do-arg (args event)))
     ;; (let ((opcode (match-event-opcode obj ',(symbolize-event event))))
     ,(format nil ";; ~a" (description event))
     (error "UNIMPLEMENTED. YOU DECIDED TO IMPLEMENT IT IN THE smuks package.")))

(defun do-request (interface request)
  `(defmethod ,(req-name request) ((obj ,(read-from-string interface))
			    ;; NOTE: Requiring a client object whatever it may be. This is up to the implementation.
			    client
			    ,@(mapcar 'do-arg (args request)))
     ,(format nil ";; ~a" (description request))
     (error "Unimplemented")))


(defun do-regular-enum (interface enum)
  `((defmethod ,(enum-name enum) ((obj ,(read-from-string interface)) value)
     ,(format nil ";; ~a" (description enum))
     (case value
       ,@(mapcar (lambda (entry) `(,(value entry) ',(safe-enum-symbol entry enum))) (entries enum))))))

(defun do-bitfield-enum (interface enum)
  `((defmethod ,(enum-name enum) ((obj ,(read-from-string interface)) value)
     ,(format nil ";; ~a" (description enum))
     (let ((options ',(mapcar (lambda (entry) (cons (value entry) '(safe-enum-symbol entry enum))) (entries enum))))
       (loop for (mask name) in options
	     when (logbitp mask value)
	     collect name)))))

(defun do-enum-to-value (interface enum)
  `((defmethod ,(enum-val-name enum) ((obj ,(read-from-string interface)) enum-symbol)
     ,(format nil ";; ~a" (description enum))
      (case enum-symbol
	,@(mapcar (lambda (entry) `(,(safe-enum-symbol entry enum) ,(value entry))) (entries enum))))))

(defun do-enum (interface enum)
  (append
   `((export ',(mapcar (lambda (entry) (safe-enum-symbol entry enum)) (entries enum))))
   (do-enum-to-value interface enum)
   (if (bitfield-p enum)
       (do-bitfield-enum interface enum)
       (do-regular-enum interface enum))))

(defun do-event-opcode-matchers (interface events)
  `((defmethod match-event-opcode ((obj ,(read-from-string interface)) event)
      (case event
	,@(loop for event in events
		;; TODO: Check if 0 indexed or 1 indexed
		for i from 0
		collect `(,(symbolize-event event) ,i))))))

(defun do-request-opcode-matchers (interface requests)
  `((defmethod match-request-opcode ((obj ,(read-from-string interface)) opcode)
      (nth opcode '(,@(mapcar 'symbolize-request requests))))))

(defun do-request-arg-types (interface requests)
  `((defmethod get-request-arg-types ((obj ,(read-from-string interface)) opcode)
      (nth opcode '(,@(mapcar (lambda (req) (mapcar 'arg-type-symbol (args req))) requests))))))

(defun do-interface (interface namespace)
  (let ((if-name (read-from-string (format nil ":~a/~a" namespace (name interface))))
	(class-name (read-from-string (name interface))))
    (append
     `((defpackage ,if-name
	 (:use :cl :wl)
	 (:export
	  ,class-name
	  ,@(mapcar #'req-name (requests interface))
	  ,@(mapcar #'ev-name (events interface)))))
     `((in-package ,if-name))

    ;; TODO: This could probably move the client to the wl-object thing
     `((defclass ,class-name (wl:object)
	 ((client :initarg :client :accessor client))
	 (:default-initargs :version ,(version interface) :ifname ,(name interface))
	 (:documentation ,(description interface))))
     (mapcar (lambda (event) (do-event (name interface) event)) (events interface))
     (mapcar (lambda (request) (do-request (name interface) request)) (requests interface))
     (flatten (mapcar (lambda (enum) (do-enum (name interface) enum)) (enums interface)))
     (do-event-opcode-matchers (name interface) (events interface))
     (do-request-opcode-matchers (name interface) (requests interface))
     (do-request-arg-types (name interface) (requests interface)))))

(defvar *arg-type-symbols* '(int uint object new_id fixed string array fd enum))

(defun gen-lisp-code (protocol namespace) (apply #'append (mapcar (lambda (part) (do-interface part namespace)) protocol)))

;; ┌┐┌┌─┐┬ ┬
;; │││├┤ │││
;; ┘└┘└─┘└┴┘

(defvar *arg-type-map*
  '("int" :int
    "uint" :uint
    "object" :uint
    "new_id" :uint
    ;; TODO: Check what exactly happens in libwayland with fixed nums. For now made it :float
    "fixed" :float
    "string" (:pointer :char)
    "array" :pointer
    ;; TODO: Recheck the FD thing - this might not be a uint
    "fd" :uint
    ;; TODO: Most likely correct, but needs to be checked
    "enum" :uint))

(defun map-arg-type-to-c (arg)
  (let ((c-type (getf-string-equal *arg-type-map* (arg-type arg))))
    (unless c-type (error "Unknown arg type: ~a" (arg-type arg)))
    c-type))
(defun gen-request-c-arg (arg) `(,(symbolify (name arg)) ,(map-arg-type-to-c arg)))
(defun gen-generic-arg (arg) (symbolify (name arg)))

;; TODO: Need to dynamically fill out arguments - instead of just the id thing
(defun gen-request-callback (request)
  `(cl-async::define-c-callback ,(symbolify "~a-ffi" (name request)) :void
       ((client :pointer) (resource :pointer) ,@(mapcar 'gen-request-c-arg (args request)))
     (let ((client (get-client client))
	   (resource (resource-get-id resource)))
       (funcall ',(symbolify (name request)) (iface client resource) client id))))

(defun gen-request-generic (request)
  `(defgeneric ,(symbolify (name request)) (resource client ,@(mapcar 'gen-generic-arg (args request)))))

(defun gen-request-c-struct (request)
  `(,(symbolify (name request)) :pointer))

(defun gen-bind-callback ()
  `(defmethod dispatch-bind ((global global) client data version id)
     "Default bind implementation for the ,(name interface) global object.
This can be overriden by inheritance in case if custom behaviour is required."
     (let ((bound (make-instance ',(symbolify "dispatch") (display client))))
       (setf (iface client id) bound)
       (create-resource client ,(symbolify "*interface*") version id))))

(defun gen-bind-c-callback ()
  `(cl-async::define-c-callback dispatch-bind-ffi :void ((client :pointer) (data :pointer) (version :uint) (id :uint))
     (let* ((client (get-client client))
	    (data (pop-data data))
	    (global (gethash data *global-tracker*)))
       (funcall 'dispatch-bind global client (null-pointer) (mem-ref version :uint) (mem-ref id :uint)))))

(defun gen-interface (interface namespace)
  (let ((pkg-name  (symbolify ":~a/~a" namespace (name interface)))
	(global? (member (name interface) *global-interfaces* :test #'string=)))
    (append
     `((defpackage ,pkg-name
	 (:use :cl :wl :cffi)
	 (:export dispatch global)))
     `((in-package ,pkg-name))
     `((defclass dispatch (wl:object) ()
	 (:default-initargs :version ,(version interface))
	 (:documentation ,(description interface))))
     `((defvar *interface* nil))
     `((defcstruct interface
	 ,@(mapcar 'gen-request-c-struct (requests interface))))
     (mapcar 'gen-request-generic (requests interface))
     (mapcar 'gen-request-callback (requests interface))

     ;; NOTE: Global class when applicable
     (when global?
       `((defclass global (wl::global) ()
	   (:default-initargs :version ,(version interface))
	   (:documentation ,(description interface)))
	 ,(gen-bind-callback)
	 ,(gen-bind-c-callback)
	 (defvar *dispatch-bind* (callback ,(symbolify "dispatch-bind-ffi"))))))))


(defun gen-code (protocol namespace)
  (apply #'append (mapcar (lambda (part) (gen-interface part namespace)) protocol)))

(defun gen-asd (package namespace)
  `((asdf:defsystem ,(symbolify "#:bm-cl-wayland.~a" package)
     :serial t
     :license "GPLv3"
     :version "0.0.1"
     :depends-on (#:cffi #:cl-async)
     :components ((:file "bm-cl-wayland")
		  (:file package)))))

(defun generate-wayland-classes (package xml-file &key namespace)
  (let* ((xml (with-open-file (s xml-file :if-does-not-exist :error) (xmls:parse s)))
	 (protocol (read-protocol xml))
	 (code (gen-code protocol namespace))
	 (asd (gen-asd package namespace))
	 (filename (format nil "~a.lisp" package)))
    (with-open-file (stream filename :direction :output :if-exists :supersede)
      (loop :for xep :in code
	    :do (format stream "~s~%~%" xep)))
    (with-open-file (stream (format nil "~a.asd" package) :direction :output :if-exists :supersede)
      (loop :for xep :in asd
	    :do (format stream "~s~%~%" xep)))))

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
