(DEFPACKAGE :ZWLR_LAYER_SHELL_V1
  (:USE :CL :CFFI :CL-WL.FFI)
  (:NICKNAMES :ZWLR-LAYER-SHELL-V1)
  (:EXPORT DISPATCH
           GLOBAL
           DISPATCH-BIND
           GET-LAYER-SURFACE
           DESTROY))

(IN-PACKAGE :ZWLR_LAYER_SHELL_V1)

(DEFCSTRUCT INTERFACE (NAME :STRING) (VERSION :INT) (METHOD_COUNT :INT)
 (METHODS (:POINTER (:STRUCT CL-WL.FFI:WL_MESSAGE))) (EVENT_COUNT :INT)
 (EVENTS (:POINTER (:STRUCT CL-WL.FFI:WL_MESSAGE))))

(DEFVAR *INTERFACE* NIL)

(DEFPACKAGE :ZWLR_LAYER_SURFACE_V1
  (:USE :CL :CFFI :CL-WL.FFI)
  (:NICKNAMES :ZWLR-LAYER-SURFACE-V1)
  (:EXPORT DISPATCH
           GLOBAL
           DISPATCH-BIND
           SET-SIZE
           SET-ANCHOR
           SET-EXCLUSIVE-ZONE
           SET-MARGIN
           SET-KEYBOARD-INTERACTIVITY
           GET-POPUP
           ACK-CONFIGURE
           DESTROY
           SET-LAYER
           SET-EXCLUSIVE-EDGE
           SEND-CONFIGURE
           SEND-CLOSED))

(IN-PACKAGE :ZWLR_LAYER_SURFACE_V1)

(DEFCSTRUCT INTERFACE (NAME :STRING) (VERSION :INT) (METHOD_COUNT :INT)
 (METHODS (:POINTER (:STRUCT CL-WL.FFI:WL_MESSAGE))) (EVENT_COUNT :INT)
 (EVENTS (:POINTER (:STRUCT CL-WL.FFI:WL_MESSAGE))))

(DEFVAR *INTERFACE* NIL)

(IN-PACKAGE :ZWLR_LAYER_SHELL_V1)

(DEFCLASS DISPATCH (CL-WL:OBJECT)
          ((ZWLR_LAYER_SHELL_V1-ID :INITFORM NIL :ACCESSOR
            ZWLR_LAYER_SHELL_V1-ID)
           (ZWLR_LAYER_SHELL_V1-PTR :INITFORM NIL :ACCESSOR
            ZWLR_LAYER_SHELL_V1-PTR))
          (:DEFAULT-INITARGS :VERSION 5)
          (:DOCUMENTATION "create surfaces that are layers of the desktop

Clients can use this interface to assign the surface_layer role to
      wl_surfaces. Such surfaces are assigned to a \"layer\" of the output and
      rendered with a defined z-depth respective to each other. They may also be
      anchored to the edges and corners of a screen and specify input handling
      semantics. This interface should be suitable for the implementation of
      many desktop shell components, and a broad number of other applications
      that interact with the desktop.
"))

(DEFGENERIC GET-LAYER-SURFACE
    (RESOURCE ID SURFACE OUTPUT
     LAYER NAMESPACE))

(DEFGENERIC DESTROY
    (RESOURCE))

(DEFMETHOD CL-WL:DESTROY :AFTER ((DISPATCH DISPATCH))
  (CL-WL::DEBUG-LOG! "Destroying dispatch object: ~a~%" "zwlr_layer_shell_v1")
  (WHEN (CL-WL::DESTROY-CALLBACK DISPATCH)
    (LOOP FOR CALLBACK IN (CL-WL::DESTROY-CALLBACK DISPATCH)
          DO (FUNCALL CALLBACK DISPATCH)))
  (LET ((RESOURCE-PTR (ZWLR_LAYER_SHELL_V1-PTR DISPATCH)))
    (CL-WL::REMOVE-RESOURCE (POINTER-ADDRESS RESOURCE-PTR))))

(DEFMETHOD DESTROY ((DISPATCH DISPATCH)) (CL-WL:DESTROY DISPATCH))

(DEFMETHOD DESTROY (EMPTY) NIL)

(PUSHNEW
 (LIST "zwlr_layer_shell_v1"
       (LIST "zwlr_layer_surface_v1" "wl_surface" "wl_output")
       (LAMBDA ()
         (SETF *INTERFACE* (FOREIGN-ALLOC '(:STRUCT INTERFACE)))
         (LET ((REQUESTS-PTR
                (LET ((MESSAGES
                       (CFFI:FOREIGN-ALLOC '(:STRUCT CL-WL.FFI:WL_MESSAGE)
                                           :COUNT 2)))
                  (LET ((INTERFACE-ARRAY
                         (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID))
                                             :COUNT 5))
                        (MSG-PTR
                         (MEM-APTR MESSAGES '(:STRUCT CL-WL.FFI:WL_MESSAGE) 0)))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 0)
                            ZWLR_LAYER_SURFACE_V1::*INTERFACE*)
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 1)
                            WL_SURFACE::*INTERFACE*)
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 2)
                            WL_OUTPUT::*INTERFACE*)
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 3) (NULL-POINTER))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 4) (NULL-POINTER))
                    (WITH-FOREIGN-SLOTS
                     ((NAME SIGNATURE TYPES) MSG-PTR
                      (:STRUCT CL-WL.FFI:WL_MESSAGE))
                     (SETF NAME (FOREIGN-STRING-ALLOC "get_layer_surface")
                           SIGNATURE "no?ous"
                           TYPES INTERFACE-ARRAY)))
                  (LET ((INTERFACE-ARRAY
                         (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID))
                                             :COUNT 0))
                        (MSG-PTR
                         (MEM-APTR MESSAGES '(:STRUCT CL-WL.FFI:WL_MESSAGE) 1)))
                    (WITH-FOREIGN-SLOTS
                     ((NAME SIGNATURE TYPES) MSG-PTR
                      (:STRUCT CL-WL.FFI:WL_MESSAGE))
                     (SETF NAME (FOREIGN-STRING-ALLOC "destroy")
                           SIGNATURE "3"
                           TYPES INTERFACE-ARRAY)))
                  MESSAGES))
               (EVENTS-PTR
                (LET ((MESSAGES
                       (CFFI:FOREIGN-ALLOC '(:STRUCT CL-WL.FFI:WL_MESSAGE)
                                           :COUNT 0)))
                  MESSAGES)))
           (WITH-FOREIGN-SLOTS
            ((NAME VERSION METHOD_COUNT METHODS EVENT_COUNT EVENTS) *INTERFACE*
             (:STRUCT INTERFACE))
            (SETF NAME (FOREIGN-STRING-ALLOC "zwlr_layer_shell_v1")
                  VERSION 5
                  METHOD_COUNT 2
                  METHODS REQUESTS-PTR
                  EVENT_COUNT 0
                  EVENTS EVENTS-PTR)))))
 CL-WL::*INTERFACE-INIT-LIST* :TEST #'CL-WL::INTERFACE-EXISTS-TEST)

(DEFCALLBACK DISPATCHER-FFI :INT
 ((DATA :POINTER) (TARGET :POINTER) (OPCODE :UINT) (MESSAGE :POINTER)
  (ARGS :POINTER))
 (DECLARE (IGNORE DATA MESSAGE))
 (LET ((RESOURCE (GETHASH (POINTER-ADDRESS TARGET) CL-WL::*RESOURCE-TRACKER*)))
   (ECASE OPCODE
     (0
      (CL-WL::DEBUG-LOG! "Dispatching ~a:~a~%" "zwlr_layer_shell_v1"
                         "get-layer-surface")
      (FUNCALL 'GET-LAYER-SURFACE RESOURCE
               (VALUES
                (FOREIGN-SLOT-VALUE
                 (MEM-APTR ARGS '(:UNION CL-WL.FFI:WL_ARGUMENT) 0)
                 '(:UNION CL-WL.FFI:WL_ARGUMENT) 'WL-FFI::N))
               (GETHASH
                (POINTER-ADDRESS
                 (FOREIGN-SLOT-VALUE
                  (MEM-APTR ARGS '(:UNION CL-WL.FFI:WL_ARGUMENT) 1)
                  '(:UNION CL-WL.FFI:WL_ARGUMENT) 'WL-FFI::O))
                CL-WL::*RESOURCE-TRACKER*)
               (GETHASH
                (POINTER-ADDRESS
                 (FOREIGN-SLOT-VALUE
                  (MEM-APTR ARGS '(:UNION CL-WL.FFI:WL_ARGUMENT) 2)
                  '(:UNION CL-WL.FFI:WL_ARGUMENT) 'WL-FFI::O))
                CL-WL::*RESOURCE-TRACKER*)
               (LAYER-FROM-VALUE
                (FOREIGN-SLOT-VALUE
                 (MEM-APTR ARGS '(:UNION CL-WL.FFI:WL_ARGUMENT) 3)
                 '(:UNION CL-WL.FFI:WL_ARGUMENT) 'WL-FFI::U))
               (VALUES
                (FOREIGN-SLOT-VALUE
                 (MEM-APTR ARGS '(:UNION CL-WL.FFI:WL_ARGUMENT) 4)
                 '(:UNION CL-WL.FFI:WL_ARGUMENT) 'WL-FFI::S))))
     (1
      (CL-WL::DEBUG-LOG! "Dispatching ~a:~a~%" "zwlr_layer_shell_v1" "destroy")
      (FUNCALL 'DESTROY RESOURCE))))
 0)

(DEFVAR *DISPATCHER* (CALLBACK DISPATCHER-FFI))

(DEFMETHOD SHARED-INITIALIZE :AFTER ((INSTANCE DISPATCH) SLOT-NAMES &KEY ID)
  (DECLARE (IGNORE SLOT-NAMES))
  (UNLESS (ZWLR_LAYER_SHELL_V1-ID INSTANCE)
    (LET* ((RESOURCE
            (CL-WL::CREATE-RESOURCE (CL-WL:PTR (CL-WL:CLIENT INSTANCE))
                                    *INTERFACE* (VERSION INSTANCE)
                                    (OR ID 0))))
      (SETF (CL-WL::TRANSIENT-ID INSTANCE)
              (SETF (ZWLR_LAYER_SHELL_V1-ID INSTANCE)
                      (OR ID (CL-WL.FFI:RESOURCE-GET-ID RESOURCE))))
      (SETF (ZWLR_LAYER_SHELL_V1-PTR INSTANCE) RESOURCE)
      (SETF (GETHASH (POINTER-ADDRESS RESOURCE) CL-WL::*RESOURCE-TRACKER*)
              INSTANCE)
      (RESOURCE-SET-DISPATCHER RESOURCE *DISPATCHER* (NULL-POINTER)
       (NULL-POINTER) (NULL-POINTER)))))

(DEFUN ERROR-FROM-VALUE (NUMBER)
  (LOOP FOR ENTRY IN '((0 :ROLE) (1 :INVALID-LAYER)
                       (2 :ALREADY-CONSTRUCTED))
        FOR VALUE = (CAR ENTRY)
        FOR KEYWORD = (CADR ENTRY)
        WHEN (EQ NUMBER VALUE) RETURN KEYWORD
        FINALLY (ERROR (FORMAT NIL "Unknown enum value: ~a" NUMBER))))

(DEFUN ERROR-TO-VALUE (KEY)
  (LOOP FOR ENTRY IN '((0 :ROLE) (1 :INVALID-LAYER)
                       (2 :ALREADY-CONSTRUCTED))
        FOR VALUE = (CAR ENTRY)
        FOR KEYWORD = (CADR ENTRY)
        WHEN (EQ KEY KEYWORD) RETURN VALUE
        FINALLY (ERROR (FORMAT NIL "Unknown enum keyword: ~a" KEY))))

(DEFUN LAYER-FROM-VALUE (NUMBER)
  (LOOP FOR ENTRY IN '((0 :BACKGROUND) (1 :BOTTOM)
                       (2 :TOP) (3 :OVERLAY))
        FOR VALUE = (CAR ENTRY)
        FOR KEYWORD = (CADR ENTRY)
        WHEN (EQ NUMBER VALUE) RETURN KEYWORD
        FINALLY (ERROR (FORMAT NIL "Unknown enum value: ~a" NUMBER))))

(DEFUN LAYER-TO-VALUE (KEY)
  (LOOP FOR ENTRY IN '((0 :BACKGROUND) (1 :BOTTOM)
                       (2 :TOP) (3 :OVERLAY))
        FOR VALUE = (CAR ENTRY)
        FOR KEYWORD = (CADR ENTRY)
        WHEN (EQ KEY KEYWORD) RETURN VALUE
        FINALLY (ERROR (FORMAT NIL "Unknown enum keyword: ~a" KEY))))

(DEFCLASS GLOBAL (CL-WL:GLOBAL) NIL
          (:DEFAULT-INITARGS :VERSION 5 :DISPATCH-IMPL 'DISPATCH)
          (:DOCUMENTATION "create surfaces that are layers of the desktop

Clients can use this interface to assign the surface_layer role to
      wl_surfaces. Such surfaces are assigned to a \"layer\" of the output and
      rendered with a defined z-depth respective to each other. They may also be
      anchored to the edges and corners of a screen and specify input handling
      semantics. This interface should be suitable for the implementation of
      many desktop shell components, and a broad number of other applications
      that interact with the desktop.
"))

(DEFMETHOD DISPATCH-BIND ((GLOBAL GLOBAL) CLIENT DATA VERSION ID)
  "Default bind implementation for the zwlr_layer_shell_v1 global object.
This can be overriden by inheritance in case if custom behaviour is required."
  (CL-WL::DEBUG-LOG! "Binding ~a~%" "zwlr_layer_shell_v1")
  (LET ((BOUND
         (MAKE-INSTANCE (CL-WL:DISPATCH-IMPL GLOBAL) :DISPLAY
                        (CL-WL:GET-DISPLAY CLIENT) :CLIENT CLIENT :ID ID
                        :GLOBAL GLOBAL :VERSION-WANT VERSION)))
    (SETF (CL-WL:IFACE CLIENT ID) BOUND)))

(DEFCALLBACK DISPATCH-BIND-FFI :VOID
 ((CLIENT :POINTER) (DATA :POINTER) (VERSION :UINT) (ID :UINT))
 (LET* ((CLIENT (CL-WL::GET-CLIENT CLIENT)) (GLOBAL (CL-WL::GET-DATA DATA)))
   (WHEN CLIENT
     (FUNCALL 'DISPATCH-BIND GLOBAL CLIENT (NULL-POINTER) VERSION ID))))

(DEFVAR *DISPATCH-BIND* (CALLBACK DISPATCH-BIND-FFI))

(DEFMETHOD INITIALIZE-INSTANCE :AFTER ((GLOBAL GLOBAL) &KEY)
  (CL-WL::DEBUG-LOG! "Initializing global object: ~a~%" "zwlr_layer_shell_v1")
  (LET* ((NEXT-DATA-ID (CL-WL::RESERVE-DATA))
         (GLOBAL-PTR
          (GLOBAL-CREATE (CL-WL:DISPLAY-PTR (CL-WL:GET-DISPLAY GLOBAL))
           *INTERFACE* (VERSION GLOBAL) (CL-WL::DATA-PTR NEXT-DATA-ID)
           *DISPATCH-BIND*)))
    (SETF (CL-WL:PTR GLOBAL) GLOBAL-PTR)
    (CL-WL::SET-DATA NEXT-DATA-ID
                     (SETF (GETHASH (POINTER-ADDRESS GLOBAL-PTR)
                                    CL-WL::*GLOBAL-TRACKER*)
                             GLOBAL))))

(IN-PACKAGE :ZWLR_LAYER_SURFACE_V1)

(DEFCLASS DISPATCH (CL-WL:OBJECT)
          ((ZWLR_LAYER_SURFACE_V1-ID :INITFORM NIL :ACCESSOR
            ZWLR_LAYER_SURFACE_V1-ID)
           (ZWLR_LAYER_SURFACE_V1-PTR :INITFORM NIL :ACCESSOR
            ZWLR_LAYER_SURFACE_V1-PTR))
          (:DEFAULT-INITARGS :VERSION 5)
          (:DOCUMENTATION "layer metadata interface

An interface that may be implemented by a wl_surface, for surfaces that
      are designed to be rendered as a layer of a stacked desktop-like
      environment.

      Layer surface state (layer, size, anchor, exclusive zone,
      margin, interactivity) is double-buffered, and will be applied at the
      time wl_surface.commit of the corresponding wl_surface is called.

      Attaching a null buffer to a layer surface unmaps it.

      Unmapping a layer_surface means that the surface cannot be shown by the
      compositor until it is explicitly mapped again. The layer_surface
      returns to the state it had right after layer_shell.get_layer_surface.
      The client can re-map the surface by performing a commit without any
      buffer attached, waiting for a configure event and handling it as usual.
"))

(DEFGENERIC SET-SIZE
    (RESOURCE WIDTH HEIGHT))

(DEFGENERIC SET-ANCHOR
    (RESOURCE ANCHOR))

(DEFGENERIC SET-EXCLUSIVE-ZONE
    (RESOURCE ZONE))

(DEFGENERIC SET-MARGIN
    (RESOURCE TOP RIGHT BOTTOM
     LEFT))

(DEFGENERIC SET-KEYBOARD-INTERACTIVITY
    (RESOURCE KEYBOARD_INTERACTIVITY))

(DEFGENERIC GET-POPUP
    (RESOURCE POPUP))

(DEFGENERIC ACK-CONFIGURE
    (RESOURCE SERIAL))

(DEFGENERIC DESTROY
    (RESOURCE))

(DEFGENERIC SET-LAYER
    (RESOURCE LAYER))

(DEFGENERIC SET-EXCLUSIVE-EDGE
    (RESOURCE EDGE))

(DEFMETHOD CL-WL:DESTROY :AFTER ((DISPATCH DISPATCH))
  (CL-WL::DEBUG-LOG! "Destroying dispatch object: ~a~%"
                     "zwlr_layer_surface_v1")
  (WHEN (CL-WL::DESTROY-CALLBACK DISPATCH)
    (LOOP FOR CALLBACK IN (CL-WL::DESTROY-CALLBACK DISPATCH)
          DO (FUNCALL CALLBACK DISPATCH)))
  (LET ((RESOURCE-PTR (ZWLR_LAYER_SURFACE_V1-PTR DISPATCH)))
    (CL-WL::REMOVE-RESOURCE (POINTER-ADDRESS RESOURCE-PTR))))

(DEFMETHOD DESTROY ((DISPATCH DISPATCH)) (CL-WL:DESTROY DISPATCH))

(DEFMETHOD DESTROY (EMPTY) NIL)

(PUSHNEW
 (LIST "zwlr_layer_surface_v1" (LIST "xdg_popup")
       (LAMBDA ()
         (SETF *INTERFACE* (FOREIGN-ALLOC '(:STRUCT INTERFACE)))
         (LET ((REQUESTS-PTR
                (LET ((MESSAGES
                       (CFFI:FOREIGN-ALLOC '(:STRUCT CL-WL.FFI:WL_MESSAGE)
                                           :COUNT 10)))
                  (LET ((INTERFACE-ARRAY
                         (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID))
                                             :COUNT 2))
                        (MSG-PTR
                         (MEM-APTR MESSAGES '(:STRUCT CL-WL.FFI:WL_MESSAGE) 0)))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 0) (NULL-POINTER))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 1) (NULL-POINTER))
                    (WITH-FOREIGN-SLOTS
                     ((NAME SIGNATURE TYPES) MSG-PTR
                      (:STRUCT CL-WL.FFI:WL_MESSAGE))
                     (SETF NAME (FOREIGN-STRING-ALLOC "set_size")
                           SIGNATURE "uu"
                           TYPES INTERFACE-ARRAY)))
                  (LET ((INTERFACE-ARRAY
                         (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID))
                                             :COUNT 1))
                        (MSG-PTR
                         (MEM-APTR MESSAGES '(:STRUCT CL-WL.FFI:WL_MESSAGE) 1)))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 0) (NULL-POINTER))
                    (WITH-FOREIGN-SLOTS
                     ((NAME SIGNATURE TYPES) MSG-PTR
                      (:STRUCT CL-WL.FFI:WL_MESSAGE))
                     (SETF NAME (FOREIGN-STRING-ALLOC "set_anchor")
                           SIGNATURE "u"
                           TYPES INTERFACE-ARRAY)))
                  (LET ((INTERFACE-ARRAY
                         (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID))
                                             :COUNT 1))
                        (MSG-PTR
                         (MEM-APTR MESSAGES '(:STRUCT CL-WL.FFI:WL_MESSAGE) 2)))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 0) (NULL-POINTER))
                    (WITH-FOREIGN-SLOTS
                     ((NAME SIGNATURE TYPES) MSG-PTR
                      (:STRUCT CL-WL.FFI:WL_MESSAGE))
                     (SETF NAME (FOREIGN-STRING-ALLOC "set_exclusive_zone")
                           SIGNATURE "i"
                           TYPES INTERFACE-ARRAY)))
                  (LET ((INTERFACE-ARRAY
                         (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID))
                                             :COUNT 4))
                        (MSG-PTR
                         (MEM-APTR MESSAGES '(:STRUCT CL-WL.FFI:WL_MESSAGE) 3)))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 0) (NULL-POINTER))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 1) (NULL-POINTER))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 2) (NULL-POINTER))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 3) (NULL-POINTER))
                    (WITH-FOREIGN-SLOTS
                     ((NAME SIGNATURE TYPES) MSG-PTR
                      (:STRUCT CL-WL.FFI:WL_MESSAGE))
                     (SETF NAME (FOREIGN-STRING-ALLOC "set_margin")
                           SIGNATURE "iiii"
                           TYPES INTERFACE-ARRAY)))
                  (LET ((INTERFACE-ARRAY
                         (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID))
                                             :COUNT 1))
                        (MSG-PTR
                         (MEM-APTR MESSAGES '(:STRUCT CL-WL.FFI:WL_MESSAGE) 4)))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 0) (NULL-POINTER))
                    (WITH-FOREIGN-SLOTS
                     ((NAME SIGNATURE TYPES) MSG-PTR
                      (:STRUCT CL-WL.FFI:WL_MESSAGE))
                     (SETF NAME
                             (FOREIGN-STRING-ALLOC
                              "set_keyboard_interactivity")
                           SIGNATURE "u"
                           TYPES INTERFACE-ARRAY)))
                  (LET ((INTERFACE-ARRAY
                         (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID))
                                             :COUNT 1))
                        (MSG-PTR
                         (MEM-APTR MESSAGES '(:STRUCT CL-WL.FFI:WL_MESSAGE) 5)))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 0)
                            XDG_POPUP::*INTERFACE*)
                    (WITH-FOREIGN-SLOTS
                     ((NAME SIGNATURE TYPES) MSG-PTR
                      (:STRUCT CL-WL.FFI:WL_MESSAGE))
                     (SETF NAME (FOREIGN-STRING-ALLOC "get_popup")
                           SIGNATURE "o"
                           TYPES INTERFACE-ARRAY)))
                  (LET ((INTERFACE-ARRAY
                         (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID))
                                             :COUNT 1))
                        (MSG-PTR
                         (MEM-APTR MESSAGES '(:STRUCT CL-WL.FFI:WL_MESSAGE) 6)))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 0) (NULL-POINTER))
                    (WITH-FOREIGN-SLOTS
                     ((NAME SIGNATURE TYPES) MSG-PTR
                      (:STRUCT CL-WL.FFI:WL_MESSAGE))
                     (SETF NAME (FOREIGN-STRING-ALLOC "ack_configure")
                           SIGNATURE "u"
                           TYPES INTERFACE-ARRAY)))
                  (LET ((INTERFACE-ARRAY
                         (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID))
                                             :COUNT 0))
                        (MSG-PTR
                         (MEM-APTR MESSAGES '(:STRUCT CL-WL.FFI:WL_MESSAGE) 7)))
                    (WITH-FOREIGN-SLOTS
                     ((NAME SIGNATURE TYPES) MSG-PTR
                      (:STRUCT CL-WL.FFI:WL_MESSAGE))
                     (SETF NAME (FOREIGN-STRING-ALLOC "destroy")
                           SIGNATURE ""
                           TYPES INTERFACE-ARRAY)))
                  (LET ((INTERFACE-ARRAY
                         (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID))
                                             :COUNT 1))
                        (MSG-PTR
                         (MEM-APTR MESSAGES '(:STRUCT CL-WL.FFI:WL_MESSAGE) 8)))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 0) (NULL-POINTER))
                    (WITH-FOREIGN-SLOTS
                     ((NAME SIGNATURE TYPES) MSG-PTR
                      (:STRUCT CL-WL.FFI:WL_MESSAGE))
                     (SETF NAME (FOREIGN-STRING-ALLOC "set_layer")
                           SIGNATURE "2u"
                           TYPES INTERFACE-ARRAY)))
                  (LET ((INTERFACE-ARRAY
                         (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID))
                                             :COUNT 1))
                        (MSG-PTR
                         (MEM-APTR MESSAGES '(:STRUCT CL-WL.FFI:WL_MESSAGE) 9)))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 0) (NULL-POINTER))
                    (WITH-FOREIGN-SLOTS
                     ((NAME SIGNATURE TYPES) MSG-PTR
                      (:STRUCT CL-WL.FFI:WL_MESSAGE))
                     (SETF NAME (FOREIGN-STRING-ALLOC "set_exclusive_edge")
                           SIGNATURE "5u"
                           TYPES INTERFACE-ARRAY)))
                  MESSAGES))
               (EVENTS-PTR
                (LET ((MESSAGES
                       (CFFI:FOREIGN-ALLOC '(:STRUCT CL-WL.FFI:WL_MESSAGE)
                                           :COUNT 2)))
                  (LET ((INTERFACE-ARRAY
                         (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID))
                                             :COUNT 3))
                        (MSG-PTR
                         (MEM-APTR MESSAGES '(:STRUCT CL-WL.FFI:WL_MESSAGE) 0)))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 0) (NULL-POINTER))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 1) (NULL-POINTER))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 2) (NULL-POINTER))
                    (WITH-FOREIGN-SLOTS
                     ((NAME SIGNATURE TYPES) MSG-PTR
                      (:STRUCT CL-WL.FFI:WL_MESSAGE))
                     (SETF NAME (FOREIGN-STRING-ALLOC "configure")
                           SIGNATURE "uuu"
                           TYPES INTERFACE-ARRAY)))
                  (LET ((INTERFACE-ARRAY
                         (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID))
                                             :COUNT 0))
                        (MSG-PTR
                         (MEM-APTR MESSAGES '(:STRUCT CL-WL.FFI:WL_MESSAGE) 1)))
                    (WITH-FOREIGN-SLOTS
                     ((NAME SIGNATURE TYPES) MSG-PTR
                      (:STRUCT CL-WL.FFI:WL_MESSAGE))
                     (SETF NAME (FOREIGN-STRING-ALLOC "closed")
                           SIGNATURE ""
                           TYPES INTERFACE-ARRAY)))
                  MESSAGES)))
           (WITH-FOREIGN-SLOTS
            ((NAME VERSION METHOD_COUNT METHODS EVENT_COUNT EVENTS) *INTERFACE*
             (:STRUCT INTERFACE))
            (SETF NAME (FOREIGN-STRING-ALLOC "zwlr_layer_surface_v1")
                  VERSION 5
                  METHOD_COUNT 10
                  METHODS REQUESTS-PTR
                  EVENT_COUNT 2
                  EVENTS EVENTS-PTR)))))
 CL-WL::*INTERFACE-INIT-LIST* :TEST #'CL-WL::INTERFACE-EXISTS-TEST)

(DEFCALLBACK DISPATCHER-FFI :INT
 ((DATA :POINTER) (TARGET :POINTER) (OPCODE :UINT) (MESSAGE :POINTER)
  (ARGS :POINTER))
 (DECLARE (IGNORE DATA MESSAGE))
 (LET ((RESOURCE (GETHASH (POINTER-ADDRESS TARGET) CL-WL::*RESOURCE-TRACKER*)))
   (ECASE OPCODE
     (0
      (CL-WL::DEBUG-LOG! "Dispatching ~a:~a~%" "zwlr_layer_surface_v1"
                         "set-size")
      (FUNCALL 'SET-SIZE RESOURCE
               (VALUES
                (FOREIGN-SLOT-VALUE
                 (MEM-APTR ARGS '(:UNION CL-WL.FFI:WL_ARGUMENT) 0)
                 '(:UNION CL-WL.FFI:WL_ARGUMENT) 'WL-FFI::U))
               (VALUES
                (FOREIGN-SLOT-VALUE
                 (MEM-APTR ARGS '(:UNION CL-WL.FFI:WL_ARGUMENT) 1)
                 '(:UNION CL-WL.FFI:WL_ARGUMENT) 'WL-FFI::U))))
     (1
      (CL-WL::DEBUG-LOG! "Dispatching ~a:~a~%" "zwlr_layer_surface_v1"
                         "set-anchor")
      (FUNCALL 'SET-ANCHOR RESOURCE
               (ANCHOR-FROM-VALUE
                (FOREIGN-SLOT-VALUE
                 (MEM-APTR ARGS '(:UNION CL-WL.FFI:WL_ARGUMENT) 0)
                 '(:UNION CL-WL.FFI:WL_ARGUMENT) 'WL-FFI::U))))
     (2
      (CL-WL::DEBUG-LOG! "Dispatching ~a:~a~%" "zwlr_layer_surface_v1"
                         "set-exclusive-zone")
      (FUNCALL 'SET-EXCLUSIVE-ZONE RESOURCE
               (VALUES
                (FOREIGN-SLOT-VALUE
                 (MEM-APTR ARGS '(:UNION CL-WL.FFI:WL_ARGUMENT) 0)
                 '(:UNION CL-WL.FFI:WL_ARGUMENT) 'WL-FFI::I))))
     (3
      (CL-WL::DEBUG-LOG! "Dispatching ~a:~a~%" "zwlr_layer_surface_v1"
                         "set-margin")
      (FUNCALL 'SET-MARGIN RESOURCE
               (VALUES
                (FOREIGN-SLOT-VALUE
                 (MEM-APTR ARGS '(:UNION CL-WL.FFI:WL_ARGUMENT) 0)
                 '(:UNION CL-WL.FFI:WL_ARGUMENT) 'WL-FFI::I))
               (VALUES
                (FOREIGN-SLOT-VALUE
                 (MEM-APTR ARGS '(:UNION CL-WL.FFI:WL_ARGUMENT) 1)
                 '(:UNION CL-WL.FFI:WL_ARGUMENT) 'WL-FFI::I))
               (VALUES
                (FOREIGN-SLOT-VALUE
                 (MEM-APTR ARGS '(:UNION CL-WL.FFI:WL_ARGUMENT) 2)
                 '(:UNION CL-WL.FFI:WL_ARGUMENT) 'WL-FFI::I))
               (VALUES
                (FOREIGN-SLOT-VALUE
                 (MEM-APTR ARGS '(:UNION CL-WL.FFI:WL_ARGUMENT) 3)
                 '(:UNION CL-WL.FFI:WL_ARGUMENT) 'WL-FFI::I))))
     (4
      (CL-WL::DEBUG-LOG! "Dispatching ~a:~a~%" "zwlr_layer_surface_v1"
                         "set-keyboard-interactivity")
      (FUNCALL 'SET-KEYBOARD-INTERACTIVITY RESOURCE
               (KEYBOARD-INTERACTIVITY-FROM-VALUE
                (FOREIGN-SLOT-VALUE
                 (MEM-APTR ARGS '(:UNION CL-WL.FFI:WL_ARGUMENT) 0)
                 '(:UNION CL-WL.FFI:WL_ARGUMENT) 'WL-FFI::U))))
     (5
      (CL-WL::DEBUG-LOG! "Dispatching ~a:~a~%" "zwlr_layer_surface_v1"
                         "get-popup")
      (FUNCALL 'GET-POPUP RESOURCE
               (GETHASH
                (POINTER-ADDRESS
                 (FOREIGN-SLOT-VALUE
                  (MEM-APTR ARGS '(:UNION CL-WL.FFI:WL_ARGUMENT) 0)
                  '(:UNION CL-WL.FFI:WL_ARGUMENT) 'WL-FFI::O))
                CL-WL::*RESOURCE-TRACKER*)))
     (6
      (CL-WL::DEBUG-LOG! "Dispatching ~a:~a~%" "zwlr_layer_surface_v1"
                         "ack-configure")
      (FUNCALL 'ACK-CONFIGURE RESOURCE
               (VALUES
                (FOREIGN-SLOT-VALUE
                 (MEM-APTR ARGS '(:UNION CL-WL.FFI:WL_ARGUMENT) 0)
                 '(:UNION CL-WL.FFI:WL_ARGUMENT) 'WL-FFI::U))))
     (7
      (CL-WL::DEBUG-LOG! "Dispatching ~a:~a~%" "zwlr_layer_surface_v1"
                         "destroy")
      (FUNCALL 'DESTROY RESOURCE))
     (8
      (CL-WL::DEBUG-LOG! "Dispatching ~a:~a~%" "zwlr_layer_surface_v1"
                         "set-layer")
      (FUNCALL 'SET-LAYER RESOURCE
               (ZWLR-LAYER-SHELL-V1::LAYER-FROM-VALUE
                (FOREIGN-SLOT-VALUE
                 (MEM-APTR ARGS '(:UNION CL-WL.FFI:WL_ARGUMENT) 0)
                 '(:UNION CL-WL.FFI:WL_ARGUMENT) 'WL-FFI::U))))
     (9
      (CL-WL::DEBUG-LOG! "Dispatching ~a:~a~%" "zwlr_layer_surface_v1"
                         "set-exclusive-edge")
      (FUNCALL 'SET-EXCLUSIVE-EDGE RESOURCE
               (ANCHOR-FROM-VALUE
                (FOREIGN-SLOT-VALUE
                 (MEM-APTR ARGS '(:UNION CL-WL.FFI:WL_ARGUMENT) 0)
                 '(:UNION CL-WL.FFI:WL_ARGUMENT) 'WL-FFI::U))))))
 0)

(DEFVAR *DISPATCHER* (CALLBACK DISPATCHER-FFI))

(DEFMETHOD SHARED-INITIALIZE :AFTER ((INSTANCE DISPATCH) SLOT-NAMES &KEY ID)
  (DECLARE (IGNORE SLOT-NAMES))
  (UNLESS (ZWLR_LAYER_SURFACE_V1-ID INSTANCE)
    (LET* ((RESOURCE
            (CL-WL::CREATE-RESOURCE (CL-WL:PTR (CL-WL:CLIENT INSTANCE))
                                    *INTERFACE* (VERSION INSTANCE)
                                    (OR ID 0))))
      (SETF (CL-WL::TRANSIENT-ID INSTANCE)
              (SETF (ZWLR_LAYER_SURFACE_V1-ID INSTANCE)
                      (OR ID (CL-WL.FFI:RESOURCE-GET-ID RESOURCE))))
      (SETF (ZWLR_LAYER_SURFACE_V1-PTR INSTANCE) RESOURCE)
      (SETF (GETHASH (POINTER-ADDRESS RESOURCE) CL-WL::*RESOURCE-TRACKER*)
              INSTANCE)
      (RESOURCE-SET-DISPATCHER RESOURCE *DISPATCHER* (NULL-POINTER)
       (NULL-POINTER) (NULL-POINTER)))))

(DEFMETHOD SEND-CONFIGURE
           ((DISPATCH DISPATCH) SERIAL WIDTH
            HEIGHT)
  (CL-WL::DEBUG-LOG! "Event: ~a~%" "configure")
  (LET ((ARG-LIST (FOREIGN-ALLOC '(:UNION WL_ARGUMENT) :COUNT 3)))
    (SETF (FOREIGN-SLOT-VALUE
           (MEM-AREF ARG-LIST '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 0)
           '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 'WL-FFI::U)
            SERIAL)
    (SETF (FOREIGN-SLOT-VALUE
           (MEM-AREF ARG-LIST '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 1)
           '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 'WL-FFI::U)
            WIDTH)
    (SETF (FOREIGN-SLOT-VALUE
           (MEM-AREF ARG-LIST '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 2)
           '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 'WL-FFI::U)
            HEIGHT)
    (RESOURCE-POST-EVENT-ARRAY (ZWLR_LAYER_SURFACE_V1-PTR DISPATCH)
     0 ARG-LIST)))

(DEFMETHOD SEND-CLOSED ((DISPATCH DISPATCH))
  (CL-WL::DEBUG-LOG! "Event: ~a~%" "closed")
  (LET ((ARG-LIST (FOREIGN-ALLOC '(:UNION WL_ARGUMENT) :COUNT 0)))
    (RESOURCE-POST-EVENT-ARRAY (ZWLR_LAYER_SURFACE_V1-PTR DISPATCH)
     1 ARG-LIST)))

(DEFUN KEYBOARD-INTERACTIVITY-FROM-VALUE (NUMBER)
  (LOOP FOR ENTRY IN '((0 :NONE) (1 :EXCLUSIVE)
                       (2 :ON-DEMAND))
        FOR VALUE = (CAR ENTRY)
        FOR KEYWORD = (CADR ENTRY)
        WHEN (EQ NUMBER VALUE) RETURN KEYWORD
        FINALLY (ERROR (FORMAT NIL "Unknown enum value: ~a" NUMBER))))

(DEFUN KEYBOARD-INTERACTIVITY-TO-VALUE (KEY)
  (LOOP FOR ENTRY IN '((0 :NONE) (1 :EXCLUSIVE)
                       (2 :ON-DEMAND))
        FOR VALUE = (CAR ENTRY)
        FOR KEYWORD = (CADR ENTRY)
        WHEN (EQ KEY KEYWORD) RETURN VALUE
        FINALLY (ERROR (FORMAT NIL "Unknown enum keyword: ~a" KEY))))

(DEFUN ERROR-FROM-VALUE (NUMBER)
  (LOOP FOR ENTRY IN '((0 :INVALID-SURFACE-STATE)
                       (1 :INVALID-SIZE)
                       (2 :INVALID-ANCHOR)
                       (3 :INVALID-KEYBOARD-INTERACTIVITY)
                       (4 :INVALID-EXCLUSIVE-EDGE))
        FOR VALUE = (CAR ENTRY)
        FOR KEYWORD = (CADR ENTRY)
        WHEN (EQ NUMBER VALUE) RETURN KEYWORD
        FINALLY (ERROR (FORMAT NIL "Unknown enum value: ~a" NUMBER))))

(DEFUN ERROR-TO-VALUE (KEY)
  (LOOP FOR ENTRY IN '((0 :INVALID-SURFACE-STATE)
                       (1 :INVALID-SIZE)
                       (2 :INVALID-ANCHOR)
                       (3 :INVALID-KEYBOARD-INTERACTIVITY)
                       (4 :INVALID-EXCLUSIVE-EDGE))
        FOR VALUE = (CAR ENTRY)
        FOR KEYWORD = (CADR ENTRY)
        WHEN (EQ KEY KEYWORD) RETURN VALUE
        FINALLY (ERROR (FORMAT NIL "Unknown enum keyword: ~a" KEY))))

(DEFUN ANCHOR-FROM-VALUE (BITS)
  (LOOP FOR ENTRY IN '((1 :TOP) (2 :BOTTOM)
                       (4 :LEFT) (8 :RIGHT))
        FOR VALUE = (CAR ENTRY)
        FOR KEYWORD = (CADR ENTRY)
        FOR BIT = (LOGAND VALUE BITS)
        FOR FLAG = (IF (AND (ZEROP BITS) (ZEROP VALUE))
                       KEYWORD
                       (IF (> BIT 0)
                           KEYWORD
                           NIL))
        WHEN FLAG
        COLLECT FLAG))

(DEFUN ANCHOR-TO-VALUE (KEYWORDS)
  (REDUCE #'+ KEYWORDS :KEY
          (LAMBDA (KEYWORD)
            (OR
             (CADR
              (ASSOC KEYWORD
                     '((:TOP 1) (:BOTTOM 2)
                       (:LEFT 4) (:RIGHT 8))))
             0))))

(DEFCLASS GLOBAL (CL-WL:GLOBAL) NIL
          (:DEFAULT-INITARGS :VERSION 5 :DISPATCH-IMPL 'DISPATCH)
          (:DOCUMENTATION "layer metadata interface

An interface that may be implemented by a wl_surface, for surfaces that
      are designed to be rendered as a layer of a stacked desktop-like
      environment.

      Layer surface state (layer, size, anchor, exclusive zone,
      margin, interactivity) is double-buffered, and will be applied at the
      time wl_surface.commit of the corresponding wl_surface is called.

      Attaching a null buffer to a layer surface unmaps it.

      Unmapping a layer_surface means that the surface cannot be shown by the
      compositor until it is explicitly mapped again. The layer_surface
      returns to the state it had right after layer_shell.get_layer_surface.
      The client can re-map the surface by performing a commit without any
      buffer attached, waiting for a configure event and handling it as usual.
"))

(DEFMETHOD DISPATCH-BIND ((GLOBAL GLOBAL) CLIENT DATA VERSION ID)
  "Default bind implementation for the zwlr_layer_surface_v1 global object.
This can be overriden by inheritance in case if custom behaviour is required."
  (CL-WL::DEBUG-LOG! "Binding ~a~%" "zwlr_layer_surface_v1")
  (LET ((BOUND
         (MAKE-INSTANCE (CL-WL:DISPATCH-IMPL GLOBAL) :DISPLAY
                        (CL-WL:GET-DISPLAY CLIENT) :CLIENT CLIENT :ID ID
                        :GLOBAL GLOBAL :VERSION-WANT VERSION)))
    (SETF (CL-WL:IFACE CLIENT ID) BOUND)))

(DEFCALLBACK DISPATCH-BIND-FFI :VOID
 ((CLIENT :POINTER) (DATA :POINTER) (VERSION :UINT) (ID :UINT))
 (LET* ((CLIENT (CL-WL::GET-CLIENT CLIENT)) (GLOBAL (CL-WL::GET-DATA DATA)))
   (WHEN CLIENT
     (FUNCALL 'DISPATCH-BIND GLOBAL CLIENT (NULL-POINTER) VERSION ID))))

(DEFVAR *DISPATCH-BIND* (CALLBACK DISPATCH-BIND-FFI))

(DEFMETHOD INITIALIZE-INSTANCE :AFTER ((GLOBAL GLOBAL) &KEY)
  (CL-WL::DEBUG-LOG! "Initializing global object: ~a~%"
                     "zwlr_layer_surface_v1")
  (LET* ((NEXT-DATA-ID (CL-WL::RESERVE-DATA))
         (GLOBAL-PTR
          (GLOBAL-CREATE (CL-WL:DISPLAY-PTR (CL-WL:GET-DISPLAY GLOBAL))
           *INTERFACE* (VERSION GLOBAL) (CL-WL::DATA-PTR NEXT-DATA-ID)
           *DISPATCH-BIND*)))
    (SETF (CL-WL:PTR GLOBAL) GLOBAL-PTR)
    (CL-WL::SET-DATA NEXT-DATA-ID
                     (SETF (GETHASH (POINTER-ADDRESS GLOBAL-PTR)
                                    CL-WL::*GLOBAL-TRACKER*)
                             GLOBAL))))

