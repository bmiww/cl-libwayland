
;;  ██████╗ ██████╗ ███╗   ███╗██████╗ ██╗██╗     ███████╗██████╗
;; ██╔════╝██╔═══██╗████╗ ████║██╔══██╗██║██║     ██╔════╝██╔══██╗
;; ██║     ██║   ██║██╔████╔██║██████╔╝██║██║     █████╗  ██████╔╝
;; ██║     ██║   ██║██║╚██╔╝██║██╔═══╝ ██║██║     ██╔══╝  ██╔══██╗
;; ╚██████╗╚██████╔╝██║ ╚═╝ ██║██║     ██║███████╗███████╗██║  ██║
;;  ╚═════╝ ╚═════╝ ╚═╝     ╚═╝╚═╝     ╚═╝╚══════╝╚══════╝╚═╝  ╚═╝
;; TODO: For the wl_display send-error function - it is expected to send an object_id rather than a pointer (it seems)
;; Although the type there is listed as object. Since i don't currently use the send-error event - ignoring for now
;; NOTE: Regenerate all the xmls that are predefined in this project
;; (gen-classes-i-use)
;; NOTE: Example invocations
;; (generate-wayland-classes 'wayland-core "/usr/share/wayland/wayland.xml")
;; (generate-wayland-classes 'xdg-shell "xmls/xdg-shell.xml" :deps '("wayland-core"))

(defpackage :cl-wl.compiler
  (:use :cl :xmls :cl-wl.parser))
(in-package :cl-wl.compiler)

(defvar *message-array-args*
  (list
   '("xdg_toplevel" "configure" 2 :uint32)
   '("wl_keyboard" "enter" 2 :uint32)
   '("xdg_toplevel" "wm_capabilities" 0 :uint32)
   ;; TODO: dev_t value. This could be 32 bits in older c versions. Can't be arsed
   '("zwp_linux_dmabuf_feedback_v1" "main_device" 0 :uint64)
   ;; TODO: dev_t value. This could be 32 bits in older c versions. Can't be arsed
   '("zwp_linux_dmabuf_feedback_v1" "tranche_target_device" 0 :uint64)
   '("zwp_linux_dmabuf_feedback_v1" "tranche_formats" 0 :uint16)))

(defun find-array-type (interface-name message-name index)
  (let ((c-type (loop for (interface message arg-index argtype) in *message-array-args*
	when (and (string= interface interface-name)
		  (string= message message-name)
		  (= index arg-index))
	  return argtype)))
    (if c-type c-type (error (format nil "No array type found for ~a ~a~%" interface-name message-name)))))

(defun gen-request-c-struct (request) `(,(symbolify (name request)) :pointer))
(defun gen-generic-arg (arg) (symbolify (name arg)))
(defun gen-request-generic (request)
  `(defgeneric ,(symbolify (dash-name request)) (resource ,@(mapcar 'gen-generic-arg (args request)))))

(defun pkg-name (interface) (symbolify ":~a" (name interface)))
(defun dispatch-id! (name) (symbolify "~a-id" name))
(defun dispatch-named-id! (name) (symbolify "~a::~a-id" name name))
(defun dispatch-id (interface) (dispatch-id! (name interface)))
(defun dispatch-ptr (interface) (symbolify "~a-ptr" (name interface)))
(defun dispatch-named-ptr (name) (symbolify "~a::~a-ptr" name name))

(defun gen-dispatch-init (interface)
  `((defmethod shared-initialize :after ((instance dispatch) slot-names &key id)
      (declare (ignore slot-names))
      (unless (,(dispatch-id interface) instance)
	(let* ((resource (wl::create-resource
			  (wl::ptr (wl::client instance))
			  ,(symbolify "*interface*")
			  (version instance)
			  (or id 0))))
	  (setf (wl::transient-id instance)
		(setf (,(dispatch-id interface) instance) (or id (wl-ffi:resource-get-id resource))))
	  (setf (,(dispatch-ptr interface) instance) resource)
	  (setf (gethash (pointer-address resource) wl::*resource-tracker*) instance)
	  (resource-set-dispatcher resource ,(symbolify "*dispatcher*") (null-pointer) (null-pointer) (null-pointer)))))))

(defun gen-bind-callback (interface)
  `((defmethod dispatch-bind ((global global) client data version id)
      ,(format nil "Default bind implementation for the ~a global object.
This can be overriden by inheritance in case if custom behaviour is required." (name interface))
      (wl::debug-log! "Binding ~a~%" ,(name interface))
      (let ((bound (make-instance (wl::dispatch-impl global) :display (wl::get-display client) :client client :id id :global global :version-want version)))
	(setf (wl::iface client id) bound)))))

(defun gen-bind-c-callback ()
  `((defcallback dispatch-bind-ffi :void ((client :pointer) (data :pointer) (version :uint) (id :uint))
	(let* ((client (wl::get-client client))
	       (global (wl::get-data data)))
	  (when client (funcall 'dispatch-bind global client (null-pointer) version id))))))

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
      ("enum"
       (let ((enum (enum arg)) (func-name ""))
	 (setf func-name
	       (if (> (length enum) 1)
		   (format nil "~a::~a-from-value" (dash-name! (car enum)) (dash-name! (cadr enum)))
		   (format nil "~a-from-value" (dash-name! (car enum)))))
	 `(,(symbolify func-name) ,selector)))
      ("object" `(gethash (pointer-address ,selector) wl::*resource-tracker*))
      ;; TODO: You don't know yet how to handle the darn arrays - so just error out here :)
      ("array" `(error "WL C ARRAY PARSING NOT IMPLEMENTED")))))


;; TODO: This is a bit annoying - since it loosly refers to the symbol "resource"
(defun gen-matcher (ifname opcode request)
  "A case form to match an opcode to a method to be called upon a resource and it's
argument feed."
  `(,opcode
    (wl::debug-log! "Dispatching ~a:~a~%" ,ifname ,(dash-name request))
    (funcall ',(symbolify (dash-name request)) resource
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
			 collect (gen-matcher (name interface) index request)))
	 (ignore-list (if (requests interface)
			  (if (= 0 arg-usage) '(args) nil)
			  '(args target opcode))))

    `((defcallback dispatcher-ffi :int
	  ((data :pointer) (target :pointer) (opcode :uint) (message :pointer) (args :pointer))
	(declare (ignore data message ,@ignore-list))
	,(if (requests interface)
	     `(let ((resource (gethash (pointer-address target) wl::*resource-tracker*)))
		(restart-case (ecase opcode ,@matchers)
		  (kill-client ()
		    :report "Kill the client causing errors"
		    (wl:destroy-client (wl:client resource)) 0)))
	     `(error (format nil "A dispatcher without requests has been called for interface: ~a~%" ,(name interface))))
	  0))))


(defun gen-c-struct-filler (methods)
  (let ((interface-deps nil))
    (values `(let ((messages (cffi:foreign-alloc '(:struct wl-ffi:wl_message)
						 :count ,(length methods))))
	       ,@(loop for opcode below (length methods)
		       for method = (nth opcode methods)
		       collect
		       `(let ((interface-array (cffi:foreign-alloc '(:pointer (:pointer :void))
								   :count ,(length (args method))))
			      (msg-ptr (mem-aptr messages '(:struct wl-ffi:wl_message) ,opcode)))
			  ,@(append
			     ;; Code to fill the interface array with references to interface definitions
			     (loop for index below (length (args method))
				   collect (let ((arg (nth index (args method))))
					     `(setf (mem-aref interface-array :pointer ,index)
						    ,(if (interface arg)
							 (progn
							   (pushnew (interface arg) interface-deps :test #'string=)
							   (symbolify "~a::*interface*" (interface arg)))
							 `(null-pointer)))))
			     `((with-foreign-slots ((name signature types) msg-ptr (:struct wl-ffi:wl_message))
				 (setf name (foreign-string-alloc ,(name method))
				       signature ,(signature method)
				       types interface-array))))))
	       messages)
	    interface-deps)))

(defun gen-interface-var-fill (interface interface-deps request-sexps event-sexps)
  `((pushnew
     (list ,(name interface)
	   (list ,@interface-deps)
	   (lambda ()
	     (setf *interface* (foreign-alloc '(:struct interface)))
	     (let ((requests-ptr ,request-sexps)
		   (events-ptr ,event-sexps))
	       (with-foreign-slots ((name version method_count methods event_count events) *interface* (:struct interface))
		 (setf name (foreign-string-alloc ,(name interface))
		       version ,(version interface)
		       method_count ,(length (requests interface))
		       methods requests-ptr
		       event_count ,(length (events interface))
		       events events-ptr)))))
     wl::*interface-init-list*
     :test #'wl::interface-exists-test)))

(defun gen-interface-c-structs (interface)
  (let ((interface-deps nil)
	(request-sexps nil)
	(event-sexps nil))
    (multiple-value-bind (sexps deps) (gen-c-struct-filler (requests interface))
      (loop for dep in deps do (pushnew dep interface-deps :test #'string=))
      (setf request-sexps sexps))
    (multiple-value-bind (sexps deps) (gen-c-struct-filler (events interface))
      (loop for dep in deps do (pushnew dep interface-deps :test #'string=))
      (setf event-sexps sexps))
    (gen-interface-var-fill interface interface-deps request-sexps event-sexps)))

(defun gen-global-init (interface)
  `((defmethod initialize-instance :after ((global global) &key)
      (wl::debug-log! "Initializing global object: ~a~%" ,(name interface))
      (let* ((next-data-id (wl::reserve-data))
	     (global-ptr (global-create (wl::display-ptr (wl::get-display global)) *interface*
					(version global) (wl::data-ptr next-data-id)
					*dispatch-bind*)))
	(setf (wl::ptr global) global-ptr)
	;; TODO: not sure i really need to keep track of the globals.
	;; The *global-tracker* set might be unnecessary
	(wl::set-data next-data-id (setf (gethash (pointer-address global-ptr) wl::*global-tracker*) global))))))

(defun gen-c-arg-setter-inner (arg index value-form)
  `(setf
    (foreign-slot-value
     (mem-aref arg-list '(:union wl-ffi:wl_argument_outgoing) ,index)
     '(:union wl-ffi:wl_argument_outgoing)
     ,(symbolify "'wl-ffi::~a" (arg-type-char arg)))
    ,value-form))

(defun gen-c-arg-setter (arg index)
  (let ((name (symbolify (name arg))))
    (gen-c-arg-setter-inner
     arg index
     (alexandria:eswitch ((arg-type arg) :test 'string=)
       ("int" name) ("uint" name)
       ("fd" name) ("string" name)
       ("new_id" `(,(dispatch-named-ptr (interface arg)) ,name))
       ("enum" (let ((enum (enum arg)) (func-name ""))
		 (setf func-name
		       (if (> (length enum) 1)
			   (format nil "~a::~a-to-value" (dash-name! (car enum)) (dash-name! (cadr enum)))
			   (format nil "~a-to-value" (dash-name! (car enum)))))
		 `(,(symbolify func-name) ,name)))
       ("object" (if (interface arg)
		     (if (nullable arg)
			 `(if ,name (,(dispatch-named-ptr (interface arg)) ,name) (null-pointer))
			 `(,(dispatch-named-ptr (interface arg)) ,name))
		     `(error "Protocol did not specify object type. For example see wl_display error event. This is unimplemented")))
       ("fixed" `(wl::to-fixnum ,name))
       ("array"
	;; NOTE: For some reason - the pywayland implementation sets alloc to equal the size of the array
	(let ((c-type (find-array-type (parent-interface arg) (parent-message arg) index)))
	  `(let* ((length (length ,name))
		  (struct (foreign-alloc '(:struct wl_array)))
		  (data (foreign-alloc ,c-type :count length)))

	     (loop for index below length do
	       (setf (mem-aref data ,c-type index) (nth index ,name)))

	     (setf (foreign-slot-value struct '(:struct wl_array) 'wl-ffi::size) (* length (foreign-type-size ,c-type))
		   (foreign-slot-value struct '(:struct wl_array) 'wl-ffi::alloc) length
		   (foreign-slot-value struct '(:struct wl_array) 'wl-ffi::data) data)
	     struct)))))))

(defun gen-event (interface event opcode)
  `(defmethod ,(symbolify "send-~a" (dash-name event)) ((dispatch dispatch) ,@(mapcar 'gen-generic-arg (args event)))
     (wl::debug-log! "Event: ~a~%" ,(name event))
     (let ((arg-list (foreign-alloc '(:union wl_argument) :count ,(length (args event)))))
       ,@(loop for index below (length (args event))
	       for arg = (nth index (args event))
	       collect (gen-c-arg-setter arg index))
       (resource-post-event-array
	(,(dispatch-ptr interface) dispatch) ,opcode arg-list))))

(defun gen-events (interface)
  (let ((events (events interface)))
    (if events
	(loop for index below (length events)
	      for event = (nth index events)
	      collect (gen-event interface event index))
	nil)))

(defun has-destroy-request (interface)
  (some (lambda (request) (string= (name request) "destroy")) (requests interface)))

(defun gen-enum-keyword (entry) (symbolify ":~a" (dash-name entry)))

(defun gen-bitfield-enum (enum entries)
  `((defun ,(symbolify "~a-from-value" (dash-name enum)) (bits)
      (loop for entry in '(,@(mapcar (lambda (entry) `(,(value entry) ,(gen-enum-keyword entry))) entries))
	    for value = (car entry)
	    for keyword = (cadr entry)
	    for bit = (logand value bits)
	    for flag = (if (and (zerop bits) (zerop value)) keyword
			   (if (> bit 0) keyword nil))
	    when flag collect flag))
    (defun ,(symbolify "~a-to-value" (dash-name enum)) (keywords)
      (reduce #'+ keywords
	      :key (lambda (keyword)
		     (or (cadr (assoc keyword
				      ',(mapcar
					 (lambda (entry)
					   `(,(gen-enum-keyword entry) ,(value entry)))
					 entries))) 0))))))

(defun gen-simple-enum (enum entries)
  `((defun ,(symbolify "~a-from-value" (dash-name enum)) (number)
      (loop for entry in '(,@(mapcar (lambda (entry) `(,(value entry) ,(gen-enum-keyword entry))) entries))
	    for value = (car entry)
	    for keyword = (cadr entry)
	    when (eq number value)
	      return keyword
	    finally (error (format nil "Unknown enum value: ~a" number))))
    (defun ,(symbolify "~a-to-value" (dash-name enum)) (key)
      (loop for entry in '(,@(mapcar (lambda (entry) `(,(value entry) ,(gen-enum-keyword entry))) entries))
	    for value = (car entry)
	    for keyword = (cadr entry)
	    when (eq key keyword)
	      return value
	    finally (error (format nil "Unknown enum keyword: ~a" key))))))

(defun gen-enum (enum)
  (let ((entries (entries enum))
	(bitfield (bitfield-p enum)))
    (if bitfield
	(gen-bitfield-enum enum entries)
	(gen-simple-enum enum entries))))



(defun gen-enums (interface)
  (let ((enums (enums interface)))
    (apply #'append
	   (loop for enum in enums
		 collect (gen-enum enum)))))


(defun gen-interface (interface)
  (let ((pkg-name (pkg-name interface)))
    (append
     `((in-package ,pkg-name))
     `((defclass dispatch (wl::object)
	 ((,(dispatch-id interface) :initform nil :accessor ,(dispatch-id interface))
	  (,(dispatch-ptr interface) :initform nil :accessor ,(dispatch-ptr interface)))
	 (:default-initargs :version ,(version interface))
	 (:documentation ,(description interface))))
     (mapcar 'gen-request-generic (requests interface))
     `((defmethod wl::destroy ((dispatch dispatch))
	 (wl::debug-log! "Destroying dispatch object: ~a~%" ,(name interface))
	 ;; TODO: This might need to be a hook or something instead
	 ;; Right now - it is easily overwriteable by different levels of inheritance
	 (when (wl::destroy-callback dispatch) (loop for callback in (wl::destroy-callback dispatch)
						     do (funcall callback dispatch)))
	 (let ((resource-ptr (,(dispatch-ptr interface) dispatch)))
	   (wl::remove-resource (pointer-address resource-ptr)))
	 (wl::dn-if dispatch)))
     (if (has-destroy-request interface)
	 `((defmethod destroy ((dispatch dispatch)) (wl::destroy dispatch))
	   (defmethod destroy (empty) ()))
	 nil)
     (gen-interface-c-structs interface)
     (gen-dispatcher-c-callback interface)
     `((defvar *dispatcher* (callback ,(symbolify "dispatcher-ffi"))))
     (gen-dispatch-init interface)
     (gen-events interface)

     (gen-enums interface)

     `((defclass global (wl::global) ()
	 (:default-initargs :version ,(version interface) :dispatch-impl 'dispatch)
	 (:documentation ,(description interface))))
     (gen-bind-callback interface)
     (gen-bind-c-callback)
     `((defvar *dispatch-bind* (callback ,(symbolify "dispatch-bind-ffi"))))
     (gen-global-init interface))))

(defun dash-name! (name) (str:replace-all "_" "-" name))
;; NOTE: Thing could be an interface/request/event/arg
(defun dash-name (thing) (dash-name! (name thing)))

(defun gen-interface-preamble (interface)
  (let ((pkg-name (pkg-name interface)))
    (append
     `((defpackage ,pkg-name
	 (:use :cl :cffi :cl-wl.ffi)
	 (:nicknames ,(symbolify ":~a" (dash-name interface)))
	 (:export dispatch global dispatch-bind
		  ,@(mapcar 'symbolify (mapcar 'dash-name (requests interface)))
		  ,@(mapcar (lambda (event) (symbolify "send-~a" (dash-name event))) (events interface)))))
     `((in-package ,pkg-name))
     `((defcstruct interface
	 (name :string)
	 (version :int)
	 (method_count :int)
	 (methods (:pointer (:struct wl-ffi:wl_message)))
	 (event_count :int)
	 (events (:pointer (:struct wl-ffi:wl_message)))))
     `((defvar *interface* nil)))))


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
  `((asdf:defsystem ,(symbolify "#:cl-wl.~a" package)
     :serial t
     :license "GPLv3"
     :version "0.0.1"
     :depends-on (#:cffi #:cl-wl #:cl-wl.ffi ,@(mapcar (lambda (dep) (symbolify "#:cl-wl.~a" dep)) deps))
     :components ((:file ,file)))))

(defun symbolify-extract (target-string start end match-start match-end reg-starts reg-ends)
  (declare (ignore match-start match-end start end))
  (format nil "~a" (subseq target-string (aref reg-starts 0) (aref reg-ends 0))))

(defun write-sexps (sexps stream)
  (loop :for xep :in sexps
	:do (let* ((sexp-string (format nil "~s" xep))
		   (sexp-string (cl-ppcre:regex-replace-all "\"SYMBOLIFY ([^\"]+)\"" sexp-string 'symbolify-extract)))
	      (format stream "~a~%~%" sexp-string))))

(defun generate-wayland-classes (package xml-file &key (deps nil))
  (let* ((xml (with-open-file (s xml-file :if-does-not-exist :error) (xmls:parse s)))
	 (protocol (read-protocol xml))
	 (code (gen-code protocol))
	 (file (format nil "cl-wl.~a" (string-downcase package)))
	 (asd (gen-asd package file deps)))
    (with-open-file (stream (format nil "~a.lisp" file) :direction :output :if-exists :supersede)
      (write-sexps code stream))
    (with-open-file (stream (format nil "~a.asd" file) :direction :output :if-exists :supersede)
      (write-sexps asd stream))
    t))

;; NOTE: The deps here - are based on whether the protocol references any objects from other protocols
(defun gen-classes-i-use ()
  (flet ((fname (name) (merge-pathnames name (asdf:system-source-directory :cl-wl))))
    (generate-wayland-classes 'wayland-core (fname "xmls/wayland.xml"))
    (generate-wayland-classes 'xdg-shell (fname "xmls/xdg-shell.xml")
			      :deps '("wayland-core"))
    (generate-wayland-classes 'zwp-linux (fname "xmls/linux-dmabuf-v1.xml")
			      :deps '("wayland-core"))
    (generate-wayland-classes 'zwlr-layer-shell (fname "xmls/wlr-layer-shell-unstable-v1.xml")
			      :deps '("wayland-core" "xdg-shell"))
    (generate-wayland-classes 'virtual-keyboard  (fname "xmls/virtual-keyboard-unstable-v1.xml")
			      :deps '("wayland-core"))
    (generate-wayland-classes 'xdg-decoration  (fname "xmls/xdg-decoration-unstable-v1.xml")
			      :deps '("xdg-shell"))
    (generate-wayland-classes 'text-input (fname "xmls/text-input-unstable-v3.xml")
			      :deps '("wayland-core"))
    (generate-wayland-classes 'input-method (fname "xmls/input-method-unstable-v2.xml")
			      :deps '("wayland-core" "text-input"))
    (generate-wayland-classes 'xwayland (fname "xmls/xwayland-shell-v1.xml")
			      :deps '("wayland-core"))))

;; ┬ ┬┌┬┐┬┬
;; │ │ │ ││
;; └─┘ ┴ ┴┴─┘

(defun symbolify (&rest args) (format nil "SYMBOLIFY ~a" (string-upcase (apply 'format nil args))))

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
