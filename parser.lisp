
;; ██████╗  █████╗ ██████╗ ███████╗███████╗██████╗
;; ██╔══██╗██╔══██╗██╔══██╗██╔════╝██╔════╝██╔══██╗
;; ██████╔╝███████║██████╔╝███████╗█████╗  ██████╔╝
;; ██╔═══╝ ██╔══██║██╔══██╗╚════██║██╔══╝  ██╔══██╗
;; ██║     ██║  ██║██║  ██║███████║███████╗██║  ██║
;; ╚═╝     ╚═╝  ╚═╝╚═╝  ╚═╝╚══════╝╚══════╝╚═╝  ╚═╝
(defpackage :cl-wl.parser
  (:use :cl :xmls :split-sequence)
  (:export name enum description arg-type args value entries enum-name bitfield-p requests events
	   version enums read-protocol event interface signature arg-type-char
	   parent-interface parent-message nullable))
(in-package :cl-wl.parser)

;; ┌─┐┌┐┌┌┬┐┬─┐┬ ┬
;; ├┤ │││ │ ├┬┘└┬┘
;; └─┘┘└┘ ┴ ┴└─ ┴
(defun read-protocol (xml) (mapcar #'read-interface (interfaces-of (xmls:node-children xml))))

(defun read-interface (interface)
  (let* ((name (name-of interface))
	 (version (version-of interface))
	 (entries (xmls:node-children interface))
	 (interface (make-instance 'interface :name name :version version :description (get-description interface))))
    (setf (requests interface) (mapcar (lambda (request) (make-request request name)) (read-entries "request" entries)))
    (setf (events interface) (mapcar (lambda (event) (make-event event name)) (read-entries "event" entries)))
    (setf (enums interface) (mapcar (lambda (entry) (make-enum name entry)) (read-entries "enum" entries)))
    interface))

(defun read-entries (type entries) (remove-if (lambda (entry) (not (of-type entry type))) entries))

;; ┌─┐┬  ┌─┐┌─┐┌─┐┌─┐┌─┐
;; │  │  ├─┤└─┐└─┐├┤ └─┐
;; └─┘┴─┘┴ ┴└─┘└─┘└─┘└─┘

(defclass interface ()
  ((name :initarg :name :accessor name)
   (description :initarg :description :accessor description)
   (version :initarg :version :accessor version)
   (requests :initarg :requests :accessor requests)
   (events :initarg :events :accessor events)
   (enums :initarg :enums :accessor enums)))

(defclass arg ()
  ((name :initarg :name :accessor name)
   (summary :initarg :summary :accessor summary)
   (arg-type :initarg :arg-type :accessor arg-type)
   (interface :initarg :interface :accessor interface)
   (nullable :initarg :nullable :accessor nullable)
   (enum :initarg :enum :accessor enum)
   (parent-interface :initarg :parent-interface :accessor parent-interface)
   (parent-message :initarg :parent-message :accessor parent-message)))

(defclass event ()
  ((name :initarg :name :accessor name)
   (description :initarg :description :accessor description)
   (args :initarg :args :accessor args)
   (signature :initarg :signature :accessor signature)
   (since :initarg :since :accessor since)))

(defclass request ()
  ((name :initarg :name :accessor name)
   (description :initarg :description :accessor description)
   (args :initarg :args :accessor args)
   (signature :initarg :signature :accessor signature)
   (since :initarg :since :accessor since)))

(defclass enum ()
  ((name :initarg :name :accessor name)
   (description :initarg :description :accessor description)
   (interface-name :initarg :interface-name :accessor interface-name)
   (is-bitfield :initarg :is-bitfield :accessor bitfield-p)
   (entries :initarg :entries :accessor entries)))

(defclass enum-entry ()
  ((name :initarg :name :accessor name)
   (value :initarg :value :accessor value)
   (summary :initarg :summary :accessor summary)))

;; ┌─┐┌─┐┌┐┌┌─┐┌┬┐┬─┐┬ ┬┌─┐┌┬┐┌─┐┬─┐┌─┐
;; │  │ ││││└─┐ │ ├┬┘│ ││   │ │ │├┬┘└─┐
;; └─┘└─┘┘└┘└─┘ ┴ ┴└─└─┘└─┘ ┴ └─┘┴└─└─┘

(defun arg-type-char (arg)
  (alexandria:eswitch ((arg-type arg) :test 'string=)
		("int" "i")
		("uint" "u")
		("fixed" "f")
		("string" "s")
		("object" "o")
		("new_id" "n")
		("array" "a")
		("fd" "h")
		("enum" "u")))

(defun make-signature (message)
  (with-output-to-string (string)
    (when (since message) (format string (since message)))
    (dolist (arg (args message))
      (format string "~a~a"
	      (if (nullable arg) "?" "")
	      (arg-type-char arg)))))

(defmethod initialize-instance :after ((event event) &key) (setf (signature event) (make-signature event)))
(defmethod initialize-instance :after ((request request) &key) (setf (signature request) (make-signature request)))

(defun make-request (xml interface)
  (let* ((name (name-of xml))
	(args (read-args xml name interface)))
    (make-instance 'request :name name :args args
		:description (get-description xml args) :since (since-of xml))))

(defun make-event (xml interface)
  (let* ((name (name-of xml))
	(args (read-args xml name interface)))
    (make-instance 'event :name name :args args
	      :description (get-description xml args) :since (since-of xml))))

(defun make-enum (interface-name xml)
  (make-instance 'enum :name (name-of xml) :interface-name interface-name
	   :is-bitfield (bitfield-of xml) :entries (enum-entries xml)
	   :description (get-description xml)))

(defun make-enum-ref (enum-name) (if enum-name (str:split "." enum-name) nil))

(defun make-arg (arg-sxml message-name interface-name)
  (let ((enum (make-enum-ref (enum-of-type arg-sxml))))
    (make-instance 'arg
       :name (name-of arg-sxml)
       :parent-interface interface-name
       :parent-message message-name
       :arg-type (if enum "enum" (type-of-arg arg-sxml))
       :interface (interface-of arg-sxml)
       :nullable (allow-null arg-sxml)
       :summary (summary-of arg-sxml)
       :enum enum)))

;; ┌─┐┌─┐┬─┐┌─┐┌─┐┬─┐┌─┐
;; ├─┘├─┤├┬┘└─┐├┤ ├┬┘└─┐
;; ┴  ┴ ┴┴└─└─┘└─┘┴└─└─┘

(defun parse-possible-number (string)
  (if (str:starts-with? "0x" string)
      (parse-integer (str:substring 2 t string) :radix 16)
      (parse-integer string)))


(defun enum-entries (xml)
  (mapcar (lambda (entry)
	    (make-instance 'enum-entry
	       :name (name-of entry)
	       :value (parse-possible-number (value-of entry))
	       :summary (summary-of entry)))
	  (entries-of xml)))

;; TODO: Most of these *-of could be turned into a macro or a function
(defun name-of (object)
  (second (find-if (lambda (x) (and (listp x) (stringp (first x)) (string= (first x) "name")))
		   (xmls:node-attrs object))))

(defun enum-of-type (object)
  (second (find-if (lambda (x) (and (listp x) (stringp (first x)) (string= (first x) "enum")))
		   (xmls:node-attrs object))))

(defun type-of-arg (object)
  (second (find-if (lambda (x) (and (listp x) (stringp (first x)) (string= (first x) "type")))
		   (xmls:node-attrs object))))

(defun since-of (object)
  (second (find-if (lambda (x) (and (listp x) (stringp (first x)) (string= (first x) "since")))
		   (xmls:node-attrs object))))

(defun value-of (object)
  (second (find-if (lambda (x) (and (listp x) (stringp (first x)) (string= (first x) "value")))
		   (xmls:node-attrs object))))

(defun summary-of (object)
  (second (find-if (lambda (x) (and (listp x) (stringp (first x)) (string= (first x) "summary")))
		   (xmls:node-attrs object))))

(defun version-of (object)
  (parse-integer (second (find-if (lambda (x) (and (listp x) (stringp (first x)) (string= (first x) "version")))
				  (xmls:node-attrs object)))))

(defun bitfield-of (object)
  (let ((bitfield (second (find-if (lambda (x) (and (listp x) (stringp (first x)) (string= (first x) "bitfield")))
				   (xmls:node-attrs object)))))
    (if (equal bitfield "true") t nil)))

(defun interface-of (object)
  (second (find-if (lambda (x) (and (listp x) (stringp (first x)) (string= (first x) "interface")))
		   (xmls:node-attrs object))))

(defun allow-null (arg-sxml)
  (string=
   (second (find-if (lambda (x) (and (listp x) (stringp (first x)) (string= (first x) "allow-null")))
		    (xmls:node-attrs arg-sxml)))
   "true"))

(defun format-arg-list (arg)
  (format nil "~A::~A: ~A"
	  (name arg)
	  (arg-type arg)
	  (summary arg)))


(defun get-description (xml &optional args)
  (let ((description-node (find-if (lambda (entry) (of-type entry "description")) (xmls:node-children xml))))
    (if description-node
	(let* ((description (first (xmls:node-children description-node)))
	       (summary (summary-of description-node)))
	  (format nil "~A~%~%~A~%~A"
		  summary
		  description
		  (if args (format nil "~%Arguments:~%~{~A~%~}" (mapcar 'format-arg-list args)) "")))
	"")))

(defun read-args (roe-sxml message-name interface-name)
  (remove nil (mapcar (lambda (entry) (when (of-type entry "arg") (make-arg entry message-name interface-name)))
		      (xmls:node-children roe-sxml))))

(defun of-type (x type) (equal (xmls:node-name x) type))
(defun entries-of (enum) (remove-if (lambda (x) (not (of-type x "entry"))) (xmls:node-children enum)))
(defun enums-of (interface) (remove-if (lambda (x) (not (of-type x "enum"))) interface))
(defun events-of (interface) (remove-if (lambda (x) (not (of-type x "event"))) interface))
(defun requests-of (interface) (remove-if (lambda (x) (not (of-type x "request"))) interface))
(defun interfaces-of (protocol) (remove-if (lambda (x) (not (of-type x "interface"))) protocol))
