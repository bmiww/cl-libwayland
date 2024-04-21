
;;  ██████╗ ██████╗ ███╗   ███╗██████╗ ██╗██╗     ███████╗██████╗
;; ██╔════╝██╔═══██╗████╗ ████║██╔══██╗██║██║     ██╔════╝██╔══██╗
;; ██║     ██║   ██║██╔████╔██║██████╔╝██║██║     █████╗  ██████╔╝
;; ██║     ██║   ██║██║╚██╔╝██║██╔═══╝ ██║██║     ██╔══╝  ██╔══██╗
;; ╚██████╗╚██████╔╝██║ ╚═╝ ██║██║     ██║███████╗███████╗██║  ██║
;;  ╚═════╝ ╚═════╝ ╚═╝     ╚═╝╚═╝     ╚═╝╚══════╝╚══════╝╚═╝  ╚═╝
;; NOTE: Example invocations
;; (generate-wayland-classes 'wayland-core "/usr/share/wayland/wayland.xml")
;; (generate-wayland-classes 'xdg-shell "xmls/xdg-shell.xml" :deps '("wayland-core"))

(defpackage :bm-cl-wayland.compiler
  (:use :cl :xmls :bm-cl-wayland.parser))
(in-package :bm-cl-wayland.compiler)

(defun ev-name (event) (read-from-string (format nil "evt-~a" (name event))))
(defun enum-name (enum) (read-from-string (format nil "enum-~a" (name enum))))
(defun enum-val-name (enum) (read-from-string (format nil "enum-~a-value" (name enum))))
(defun symbolize-event (event) (ev-name event))
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

(defun do-interface (interface)
  (append
   (mapcar (lambda (event) (do-event (name interface) event)) (events interface))
   (flatten (mapcar (lambda (enum) (do-enum (name interface) enum)) (enums interface)))
   (do-event-opcode-matchers (name interface) (events interface))))

(defvar *arg-type-symbols* '(int uint object new_id fixed string array fd enum))

(defun gen-lisp-code (protocol) (apply #'append (mapcar (lambda (part) (do-interface part)) protocol)))

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

(defun gen-request-c-funcall-arg (arg) (symbolify (name arg)))

;; TODO: Need to dynamically fill out arguments - instead of just the id thing
(defun gen-request-callback (request)
  `(cl-async::define-c-callback ,(symbolify "~a-ffi" (name request)) :void
       ((client :pointer) (resource :pointer) ,@(mapcar 'gen-request-c-arg (args request)))
     (let ((client (get-client client))
	   (resource (resource-get-id resource)))
       (funcall
	',(symbolify (name request))
	(iface client resource)
	client
	,@(mapcar 'gen-request-c-funcall-arg (args request))))))

(defun gen-request-generic (request)
  `(defgeneric ,(symbolify (name request)) (resource client ,@(mapcar 'gen-generic-arg (args request)))))

(defun gen-request-c-struct (request)
  `(,(symbolify (name request)) :pointer))

(defun gen-bind-callback (interface)
  `((defmethod dispatch-bind ((global global) client data version id)
      ,(format nil "Default bind implementation for the ~a global object.
This can be overriden by inheritance in case if custom behaviour is required." (name interface))

     (let ((bound (make-instance ',(symbolify "dispatch") :display (display client))))
       (setf (iface client id) bound)
       (create-resource client ,(symbolify "*interface*") version id)))))

(defun gen-bind-c-callback ()
  `((cl-async::define-c-callback dispatch-bind-ffi :void ((client :pointer) (data :pointer) (version :uint) (id :uint))
     (let* ((client (get-client client))
	    (global (pop-data data)))
       (funcall 'dispatch-bind global client (null-pointer) (mem-ref version :uint) (mem-ref id :uint))))))

(defun gen-c-slot-init (request)
  `(setf
    (foreign-slot-value *interface* '(:struct interface) ,(symbolify "'~a" (name request)))
    (callback ,(symbolify "~a-ffi" (name request)))))

(defun gen-interface-var-fill (interface)
  `((setf (foreign-slot-value *interface* '(:struct interface) 'name) (foreign-string-alloc ,(name interface)))
    (setf (foreign-slot-value *interface* '(:struct interface) 'version) ,(version interface))
    (setf (foreign-slot-value *interface* '(:struct interface) 'method_count) ,(length (requests interface)))
    (setf (foreign-slot-value *interface* '(:struct interface) 'methods) ,(symbolify "*requests*"))
    (setf (foreign-slot-value *interface* '(:struct interface) 'event_count) ,(length (events interface)))
    (setf (foreign-slot-value *interface* '(:struct interface) 'events) ,(symbolify "*events*"))
    ,@(mapcar 'gen-c-slot-init (requests interface))))

(defun gen-c-struct-filler (var-name methods)
  `((defvar ,var-name
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
	 messages))))

(defun gen-interface-c-structs (interface)
  (append
   (gen-c-struct-filler (symbolify "*requests*") (requests interface))
   (gen-c-struct-filler (symbolify "*events*") (events interface))
   (gen-interface-var-fill interface)))

(defun gen-global-init ()
  `((defmethod initialize-instance :after ((global global) &key)
     (let* ((next-data-id (reserve-data))
	    (global-ptr (global-create (display global) *interface*
				   (version global) (data-ptr next-data-id)
				   *dispatch-bind*)))
       ;; TODO: not sure i really need to keep track of the globals.
       ;; The *global-tracker* set might be unnecessary
       (set-data next-data-id (setf (gethash (pointer-address global-ptr) *global-tracker*) global))))))

(defun pkg-name (interface )
  (symbolify ":~a" (name interface)))

(defun gen-interface (interface)
  (let ((pkg-name (pkg-name interface)))
    (append
     `((in-package ,pkg-name))
     `((defclass dispatch (wl:object) ()
	 (:default-initargs :version ,(version interface))
	 (:documentation ,(description interface))))
     (mapcar 'gen-request-generic (requests interface))
     (mapcar 'gen-request-callback (requests interface))
     (gen-interface-c-structs interface)

     `((defclass global (wl::global) ()
	 (:default-initargs :version ,(version interface))
	 (:documentation ,(description interface))))
     (gen-bind-callback interface)
     (gen-bind-c-callback)
     `((defvar *dispatch-bind* (callback ,(symbolify "dispatch-bind-ffi"))))
     (gen-global-init))))

(defun gen-interface-preamble (interface)
  (let ((pkg-name (pkg-name interface)))
    (append
     `((defpackage ,pkg-name
	 (:use :cl :wl :cffi)
	 (:nicknames ,(symbolify ":~a" (str:replace-all "_" "-" (name interface))))
	 (:export dispatch global)))
     `((in-package ,pkg-name))
     `((defcstruct interface
	 (name :string)
	 (version :int)
	 (method_count :int)
	 (methods (:pointer (:struct wl_message)))
	 (event_count :int)
	 (events (:pointer (:struct wl_message)))
	 ,@(mapcar 'gen-request-c-struct (requests interface))))
     `((defvar *interface* (cffi:foreign-alloc '(:struct interface)))))))

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
