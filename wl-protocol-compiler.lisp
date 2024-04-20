
;;  ██████╗ ██████╗ ███╗   ███╗██████╗ ██╗██╗     ███████╗██████╗
;; ██╔════╝██╔═══██╗████╗ ████║██╔══██╗██║██║     ██╔════╝██╔══██╗
;; ██║     ██║   ██║██╔████╔██║██████╔╝██║██║     █████╗  ██████╔╝
;; ██║     ██║   ██║██║╚██╔╝██║██╔═══╝ ██║██║     ██╔══╝  ██╔══██╗
;; ╚██████╗╚██████╔╝██║ ╚═╝ ██║██║     ██║███████╗███████╗██║  ██║
;;  ╚═════╝ ╚═════╝ ╚═╝     ╚═╝╚═╝     ╚═╝╚══════╝╚══════╝╚═╝  ╚═╝
;; NOTE: Example invocations
;; (generate-wayland-classes 'wayland-core "/usr/share/wayland/wayland.xml" :namespace "wl")
;; (generate-wayland-classes 'xdg-shell "xmls/xdg-shell.xml" :namespace "xdg")

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

(defun gen-bind-callback ()
  `(defmethod dispatch-bind ((global global) client data version id)
     "Default bind implementation for the ,(name interface) global object.
This can be overriden by inheritance in case if custom behaviour is required."
     (let ((bound (make-instance ',(symbolify "dispatch") :display (display client))))
       (setf (iface client id) bound)
       (create-resource client ,(symbolify "*interface*") version id))))

(defun gen-bind-c-callback ()
  `(cl-async::define-c-callback dispatch-bind-ffi :void ((client :pointer) (data :pointer) (version :uint) (id :uint))
     (let* ((client (get-client client))
	    (data (pop-data data))
	    (global (gethash data *global-tracker*)))
       (funcall 'dispatch-bind global client (null-pointer) (mem-ref version :uint) (mem-ref id :uint)))))

(defun gen-interface (interface namespace)
  (let ((pkg-name  (symbolify ":~a/~a" namespace (name interface))))
    (append
     `((defpackage ,pkg-name
	 (:use :cl :wl :cffi)
	 (:nicknames ,(symbolify ":~a" (str:replace-all "_" "-" (name interface))))
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

     `((defclass global (wl::global) ()
	 (:default-initargs :version ,(version interface))
	 (:documentation ,(description interface)))
       ,(gen-bind-callback)
       ,(gen-bind-c-callback)
       (defvar *dispatch-bind* (callback ,(symbolify "dispatch-bind-ffi")))))))


(defun gen-code (protocol namespace)
  (apply #'append (mapcar (lambda (part) (gen-interface part namespace)) protocol)))

(defun gen-asd (package file)
  `((asdf:defsystem ,(symbolify "#:bm-cl-wayland.~a" package)
     :serial t
     :license "GPLv3"
     :version "0.0.1"
     :depends-on (#:cffi #:cl-async #:bm-cl-wayland)
     :components ((:file ,file)))))

(defun generate-wayland-classes (package xml-file &key namespace)
  (let* ((xml (with-open-file (s xml-file :if-does-not-exist :error) (xmls:parse s)))
	 (protocol (read-protocol xml))
	 (code (gen-code protocol namespace))
	 (file (format nil "bm-cl-wayland.~a" (string-downcase package)))
	 (asd (gen-asd package file)))
    (with-open-file (stream (format nil "~a.lisp" file) :direction :output :if-exists :supersede)
      (loop :for xep :in code
	    :do (format stream "~s~%~%" xep)))
    (with-open-file (stream (format nil "~a.asd" file) :direction :output :if-exists :supersede)
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
