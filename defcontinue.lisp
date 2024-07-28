
;;  ██████╗ ██████╗ ███╗   ██╗████████╗██╗███╗   ██╗██╗   ██╗ █████╗ ████████╗██╗ ██████╗ ███╗   ██╗███████╗
;; ██╔════╝██╔═══██╗████╗  ██║╚══██╔══╝██║████╗  ██║██║   ██║██╔══██╗╚══██╔══╝██║██╔═══██╗████╗  ██║██╔════╝
;; ██║     ██║   ██║██╔██╗ ██║   ██║   ██║██╔██╗ ██║██║   ██║███████║   ██║   ██║██║   ██║██╔██╗ ██║███████╗
;; ██║     ██║   ██║██║╚██╗██║   ██║   ██║██║╚██╗██║██║   ██║██╔══██║   ██║   ██║██║   ██║██║╚██╗██║╚════██║
;; ╚██████╗╚██████╔╝██║ ╚████║   ██║   ██║██║ ╚████║╚██████╔╝██║  ██║   ██║   ██║╚██████╔╝██║ ╚████║███████║
;;  ╚═════╝ ╚═════╝ ╚═╝  ╚═══╝   ╚═╝   ╚═╝╚═╝  ╚═══╝ ╚═════╝ ╚═╝  ╚═╝   ╚═╝   ╚═╝ ╚═════╝ ╚═╝  ╚═══╝╚══════╝
(defpackage #:defcontinue
  (:use #:cl)
  (:export defcontinue after before)
  (:local-nicknames (:clos :closer-mop)))
(in-package :defcontinue)

;; ┌┬┐┌─┐┌┬┐┬ ┬┌─┐┌┬┐  ┌─┐┌─┐┌┐┌┌┬┐┬┌┐┌┬ ┬┌─┐┌┬┐┬┌─┐┌┐┌┌─┐
;; │││├┤  │ ├─┤│ │ ││  │  │ ││││ │ │││││ │├─┤ │ ││ ││││└─┐
;; ┴ ┴└─┘ ┴ ┴ ┴└─┘─┴┘  └─┘└─┘┘└┘ ┴ ┴┘└┘└─┘┴ ┴ ┴ ┴└─┘┘└┘└─┘
;; TODO: This assumes that the first arg is the class, and will die otherwise
;; TODO: Throw if a method with :after or :before is given
;; TODO: Maybe can somehow directly reference the freshly created method for ensure-class-slot?
;; TODO: The :keep list might need to be reversed
(defmacro defcontinue (name &rest args)
  (let* ((method-declaration `(defmethod ,name ,@args))
	 (after-slot (intern (format nil "after~a" name) (method-package name)))
	 (before-slot (intern (format nil "before~a" name) (method-package name)))
	 (arg-list (car args))
	 (class-arg (method-class-arg arg-list))
	 (after-method `(defmethod ,name :after ,(car args)
			  (let ((keep nil))
			    (loop for cb in (slot-value ,(car class-arg) ',after-slot)
				  do (let ((result (funcall cb ,@(args-from-arglist arg-list))))
				       (when (eq result :keep) (push cb keep))))
			    (setf (slot-value ,(car class-arg) ',after-slot) keep))))
	 (before-method `(defmethod ,name :before ,(car args)
			   (let ((keep nil))
			     (loop for cb in (slot-value ,(car class-arg) ',before-slot)
				   do (let ((result (funcall cb ,@(args-from-arglist arg-list))))
					(when (eq result :keep) (push cb keep))))
			     (setf (slot-value ,(car class-arg) ',before-slot) keep)))))

    `(progn
       ,method-declaration
       ,after-method
       ,before-method
       (let* ((class-name (cadr ',class-arg))
	      (class (find-class class-name)))
	 (ensure-class-slot class ',after-slot)
	 (ensure-class-slot class ',before-slot)))))

;; TODO: You had to remove the compile time checks cause they didn't make sense
;; Maybe you can still add back in runtime checks if safety or debug compile values are high enough
(defmacro after (method instance callback)
  `(push ,callback (slot-value ,instance ',(intern (format nil "after~a" method) (method-package method)))))

(defmacro before (method instance callback)
  `(push ,callback (slot-value ,instance ',(intern (format nil "before~a" method) (method-package method)))))


;; ┌─┐┬  ┌─┐┌─┐┌─┐  ┌┬┐┌─┐┌┐┌┬┌─┐┬ ┬┬  ┌─┐┌┬┐┬┌─┐┌┐┌
;; │  │  ├─┤└─┐└─┐  │││├─┤││││├─┘│ ││  ├─┤ │ ││ ││││
;; └─┘┴─┘┴ ┴└─┘└─┘  ┴ ┴┴ ┴┘└┘┴┴  └─┘┴─┘┴ ┴ ┴ ┴└─┘┘└┘
;; NOTE: Thread where i found the way to add a class slot:
;; https://groups.google.com/g/comp.lang.lisp/c/3JhUiNth7Lk
(defun canonicalize-slot-definition (slotdef)
  (list :name (CLOS:SLOT-DEFINITION-NAME slotdef)
	:readers (CLOS:SLOT-DEFINITION-READERS slotdef)
	:writers (CLOS:SLOT-DEFINITION-WRITERS slotdef)
	:type (CLOS:SLOT-DEFINITION-TYPE slotdef)
	:allocation (CLOS:SLOT-DEFINITION-ALLOCATION slotdef)
	:initargs (CLOS:SLOT-DEFINITION-INITARGS slotdef)
	:initform (CLOS:SLOT-DEFINITION-INITFORM slotdef)
	:initfunction (CLOS:SLOT-DEFINITION-INITFUNCTION slotdef)))


(defun ensure-class-slot (class slot-name)
  (let ((class-name (class-name class)))
    ;; finalize it before calling CLOS:CLASS-SLOTS
    (clos:ensure-finalized class)
    (unless (find slot-name (clos:class-slots class) :key (function clos:slot-definition-name))
      (clos:ensure-class
       class-name
       :direct-slots
       (append (mapcar (function canonicalize-slot-definition) (CLOS:CLASS-DIRECT-SLOTS class))
	       (list (list :name slot-name
			   :initform 'nil
			   :initfunction (constantly nil)
			   :readers (list slot-name)
			   :writers (list `(setf ,slot-name))
			   :documentation "Generated by define-association")))))
    class))


;; ┬ ┬┌┬┐┬┬
;; │ │ │ ││
;; └─┘ ┴ ┴┴─┘
(defun args-from-arglist (arglist)
  (loop for arg in arglist
	collect (if (listp arg) (car arg) arg)))

(defun method-package (method)
  (symbol-package
   (etypecase method
     (list (cadr method))
     (symbol method))))

;; TODO: Would still fail if the first typed arg is not the class
(defun method-class-arg (arg-list) (find-if (lambda (arg) (listp arg)) arg-list))
