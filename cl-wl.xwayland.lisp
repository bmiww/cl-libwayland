(DEFPACKAGE :XWAYLAND_SHELL_V1
  (:USE :CL :CFFI :CL-WL.FFI)
  (:NICKNAMES :XWAYLAND-SHELL-V1)
  (:EXPORT DISPATCH
           GLOBAL
           DISPATCH-BIND
           DESTROY
           GET-XWAYLAND-SURFACE))

(IN-PACKAGE :XWAYLAND_SHELL_V1)

(DEFCSTRUCT INTERFACE (NAME :STRING) (VERSION :INT) (METHOD_COUNT :INT)
 (METHODS (:POINTER (:STRUCT CL-WL.FFI:WL_MESSAGE))) (EVENT_COUNT :INT)
 (EVENTS (:POINTER (:STRUCT CL-WL.FFI:WL_MESSAGE))))

(DEFVAR *INTERFACE* NIL)

(DEFPACKAGE :XWAYLAND_SURFACE_V1
  (:USE :CL :CFFI :CL-WL.FFI)
  (:NICKNAMES :XWAYLAND-SURFACE-V1)
  (:EXPORT DISPATCH
           GLOBAL
           DISPATCH-BIND
           SET-SERIAL
           DESTROY))

(IN-PACKAGE :XWAYLAND_SURFACE_V1)

(DEFCSTRUCT INTERFACE (NAME :STRING) (VERSION :INT) (METHOD_COUNT :INT)
 (METHODS (:POINTER (:STRUCT CL-WL.FFI:WL_MESSAGE))) (EVENT_COUNT :INT)
 (EVENTS (:POINTER (:STRUCT CL-WL.FFI:WL_MESSAGE))))

(DEFVAR *INTERFACE* NIL)

(IN-PACKAGE :XWAYLAND_SHELL_V1)

(DEFCLASS DISPATCH (CL-WL:OBJECT)
          ((XWAYLAND_SHELL_V1-ID :INITFORM NIL :ACCESSOR
            XWAYLAND_SHELL_V1-ID)
           (XWAYLAND_SHELL_V1-PTR :INITFORM NIL :ACCESSOR
            XWAYLAND_SHELL_V1-PTR))
          (:DEFAULT-INITARGS :VERSION 1)
          (:DOCUMENTATION "context object for Xwayland shell

xwayland_shell_v1 is a singleton global object that
      provides the ability to create a xwayland_surface_v1 object
      for a given wl_surface.

      This interface is intended to be bound by the Xwayland server.

      A compositor must not allow clients other than Xwayland to
      bind to this interface. A compositor should hide this global
      from other clients' wl_registry.
      A client the compositor does not consider to be an Xwayland
      server attempting to bind this interface will result in
      an implementation-defined error.

      An Xwayland server that has bound this interface must not
      set the `WL_SURFACE_ID` atom on a window.
"))

(DEFGENERIC DESTROY
    (RESOURCE))

(DEFGENERIC GET-XWAYLAND-SURFACE
    (RESOURCE ID SURFACE))

(DEFMETHOD CL-WL:DESTROY ((DISPATCH DISPATCH))
  (CL-WL::DEBUG-LOG! "Destroying dispatch object: ~a~%" "xwayland_shell_v1")
  (WHEN (CL-WL::DESTROY-CALLBACK DISPATCH)
    (LOOP FOR CALLBACK IN (CL-WL::DESTROY-CALLBACK DISPATCH)
          DO (FUNCALL CALLBACK DISPATCH)))
  (LET ((CLIENT (CL-WL:CLIENT DISPATCH))
        (RESOURCE-PTR (XWAYLAND_SHELL_V1-PTR DISPATCH)))
    (CL-WL:REMOVE-CLIENT-OBJECT CLIENT
                                (XWAYLAND_SHELL_V1-ID DISPATCH))
    (CL-WL::REMOVE-RESOURCE (POINTER-ADDRESS RESOURCE-PTR)))
  (CL-WL::DN-IF DISPATCH))

(DEFMETHOD DESTROY ((DISPATCH DISPATCH)) (CL-WL:DESTROY DISPATCH))

(DEFMETHOD DESTROY (EMPTY) NIL)

(PUSHNEW
 (LIST "xwayland_shell_v1" (LIST "xwayland_surface_v1" "wl_surface")
       (LAMBDA ()
         (SETF *INTERFACE* (FOREIGN-ALLOC '(:STRUCT INTERFACE)))
         (LET ((REQUESTS-PTR
                (LET ((MESSAGES
                       (CFFI:FOREIGN-ALLOC '(:STRUCT CL-WL.FFI:WL_MESSAGE)
                                           :COUNT 2)))
                  (LET ((INTERFACE-ARRAY
                         (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID))
                                             :COUNT 0))
                        (MSG-PTR
                         (MEM-APTR MESSAGES '(:STRUCT CL-WL.FFI:WL_MESSAGE) 0)))
                    (WITH-FOREIGN-SLOTS
                     ((NAME SIGNATURE TYPES) MSG-PTR
                      (:STRUCT CL-WL.FFI:WL_MESSAGE))
                     (SETF NAME (FOREIGN-STRING-ALLOC "destroy")
                           SIGNATURE ""
                           TYPES INTERFACE-ARRAY)))
                  (LET ((INTERFACE-ARRAY
                         (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID))
                                             :COUNT 2))
                        (MSG-PTR
                         (MEM-APTR MESSAGES '(:STRUCT CL-WL.FFI:WL_MESSAGE) 1)))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 0)
                            XWAYLAND_SURFACE_V1::*INTERFACE*)
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 1)
                            WL_SURFACE::*INTERFACE*)
                    (WITH-FOREIGN-SLOTS
                     ((NAME SIGNATURE TYPES) MSG-PTR
                      (:STRUCT CL-WL.FFI:WL_MESSAGE))
                     (SETF NAME (FOREIGN-STRING-ALLOC "get_xwayland_surface")
                           SIGNATURE "no"
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
            (SETF NAME (FOREIGN-STRING-ALLOC "xwayland_shell_v1")
                  VERSION 1
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
   (RESTART-CASE (ECASE OPCODE
                   (0
                    (CL-WL::DEBUG-LOG! "Dispatching ~a:~a~%"
                                       "xwayland_shell_v1" "destroy")
                    (FUNCALL 'DESTROY RESOURCE))
                   (1
                    (CL-WL::DEBUG-LOG! "Dispatching ~a:~a~%"
                                       "xwayland_shell_v1"
                                       "get-xwayland-surface")
                    (FUNCALL 'GET-XWAYLAND-SURFACE RESOURCE
                             (VALUES
                              (FOREIGN-SLOT-VALUE
                               (MEM-APTR ARGS '(:UNION CL-WL.FFI:WL_ARGUMENT)
                                0)
                               '(:UNION CL-WL.FFI:WL_ARGUMENT)
                               'WL-FFI::N))
                             (GETHASH
                              (POINTER-ADDRESS
                               (FOREIGN-SLOT-VALUE
                                (MEM-APTR ARGS '(:UNION CL-WL.FFI:WL_ARGUMENT)
                                 1)
                                '(:UNION CL-WL.FFI:WL_ARGUMENT)
                                'WL-FFI::O))
                              CL-WL::*RESOURCE-TRACKER*))))
     (KILL-CLIENT NIL :REPORT "Kill the client causing errors"
      (CL-WL:DESTROY-CLIENT (CL-WL:CLIENT RESOURCE)) 0)))
 0)

(DEFVAR *DISPATCHER* (CALLBACK DISPATCHER-FFI))

(DEFMETHOD SHARED-INITIALIZE :AFTER ((INSTANCE DISPATCH) SLOT-NAMES &KEY ID)
  (DECLARE (IGNORE SLOT-NAMES))
  (UNLESS (XWAYLAND_SHELL_V1-ID INSTANCE)
    (LET* ((RESOURCE
            (CL-WL::CREATE-RESOURCE (CL-WL:PTR (CL-WL:CLIENT INSTANCE))
                                    *INTERFACE* (VERSION INSTANCE)
                                    (OR ID 0))))
      (SETF (CL-WL::TRANSIENT-ID INSTANCE)
              (SETF (XWAYLAND_SHELL_V1-ID INSTANCE)
                      (OR ID (CL-WL.FFI:RESOURCE-GET-ID RESOURCE))))
      (SETF (XWAYLAND_SHELL_V1-PTR INSTANCE) RESOURCE)
      (SETF (GETHASH (POINTER-ADDRESS RESOURCE) CL-WL::*RESOURCE-TRACKER*)
              INSTANCE)
      (RESOURCE-SET-DISPATCHER RESOURCE *DISPATCHER* (NULL-POINTER)
       (NULL-POINTER) (NULL-POINTER)))))

(DEFUN ERROR-FROM-VALUE (NUMBER)
  (LOOP FOR ENTRY IN '((0 :ROLE))
        FOR VALUE = (CAR ENTRY)
        FOR KEYWORD = (CADR ENTRY)
        WHEN (EQ NUMBER VALUE) RETURN KEYWORD
        FINALLY (ERROR (FORMAT NIL "Unknown enum value: ~a" NUMBER))))

(DEFUN ERROR-TO-VALUE (KEY)
  (LOOP FOR ENTRY IN '((0 :ROLE))
        FOR VALUE = (CAR ENTRY)
        FOR KEYWORD = (CADR ENTRY)
        WHEN (EQ KEY KEYWORD) RETURN VALUE
        FINALLY (ERROR (FORMAT NIL "Unknown enum keyword: ~a" KEY))))

(DEFCLASS GLOBAL (CL-WL:GLOBAL) NIL
          (:DEFAULT-INITARGS :VERSION 1 :DISPATCH-IMPL 'DISPATCH)
          (:DOCUMENTATION "context object for Xwayland shell

xwayland_shell_v1 is a singleton global object that
      provides the ability to create a xwayland_surface_v1 object
      for a given wl_surface.

      This interface is intended to be bound by the Xwayland server.

      A compositor must not allow clients other than Xwayland to
      bind to this interface. A compositor should hide this global
      from other clients' wl_registry.
      A client the compositor does not consider to be an Xwayland
      server attempting to bind this interface will result in
      an implementation-defined error.

      An Xwayland server that has bound this interface must not
      set the `WL_SURFACE_ID` atom on a window.
"))

(DEFMETHOD DISPATCH-BIND ((GLOBAL GLOBAL) CLIENT DATA VERSION ID)
  "Default bind implementation for the xwayland_shell_v1 global object.
This can be overriden by inheritance in case if custom behaviour is required."
  (CL-WL::DEBUG-LOG! "Binding ~a~%" "xwayland_shell_v1")
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
  (CL-WL::DEBUG-LOG! "Initializing global object: ~a~%" "xwayland_shell_v1")
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

(IN-PACKAGE :XWAYLAND_SURFACE_V1)

(DEFCLASS DISPATCH (CL-WL:OBJECT)
          ((XWAYLAND_SURFACE_V1-ID :INITFORM NIL :ACCESSOR
            XWAYLAND_SURFACE_V1-ID)
           (XWAYLAND_SURFACE_V1-PTR :INITFORM NIL :ACCESSOR
            XWAYLAND_SURFACE_V1-PTR))
          (:DEFAULT-INITARGS :VERSION 1)
          (:DOCUMENTATION
           "interface for associating Xwayland windows to wl_surfaces

An Xwayland surface is a surface managed by an Xwayland server.
      It is used for associating surfaces to Xwayland windows.

      The Xwayland server associated with actions in this interface is
      determined by the Wayland client making the request.

      The client must call wl_surface.commit on the corresponding wl_surface
      for the xwayland_surface_v1 state to take effect.
"))

(DEFGENERIC SET-SERIAL
    (RESOURCE SERIAL_LO SERIAL_HI))

(DEFGENERIC DESTROY
    (RESOURCE))

(DEFMETHOD CL-WL:DESTROY ((DISPATCH DISPATCH))
  (CL-WL::DEBUG-LOG! "Destroying dispatch object: ~a~%" "xwayland_surface_v1")
  (WHEN (CL-WL::DESTROY-CALLBACK DISPATCH)
    (LOOP FOR CALLBACK IN (CL-WL::DESTROY-CALLBACK DISPATCH)
          DO (FUNCALL CALLBACK DISPATCH)))
  (LET ((CLIENT (CL-WL:CLIENT DISPATCH))
        (RESOURCE-PTR (XWAYLAND_SURFACE_V1-PTR DISPATCH)))
    (CL-WL:REMOVE-CLIENT-OBJECT CLIENT
                                (XWAYLAND_SURFACE_V1-ID DISPATCH))
    (CL-WL::REMOVE-RESOURCE (POINTER-ADDRESS RESOURCE-PTR)))
  (CL-WL::DN-IF DISPATCH))

(DEFMETHOD DESTROY ((DISPATCH DISPATCH)) (CL-WL:DESTROY DISPATCH))

(DEFMETHOD DESTROY (EMPTY) NIL)

(PUSHNEW
 (LIST "xwayland_surface_v1" (LIST)
       (LAMBDA ()
         (SETF *INTERFACE* (FOREIGN-ALLOC '(:STRUCT INTERFACE)))
         (LET ((REQUESTS-PTR
                (LET ((MESSAGES
                       (CFFI:FOREIGN-ALLOC '(:STRUCT CL-WL.FFI:WL_MESSAGE)
                                           :COUNT 2)))
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
                     (SETF NAME (FOREIGN-STRING-ALLOC "set_serial")
                           SIGNATURE "uu"
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
                           SIGNATURE ""
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
            (SETF NAME (FOREIGN-STRING-ALLOC "xwayland_surface_v1")
                  VERSION 1
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
   (RESTART-CASE (ECASE OPCODE
                   (0
                    (CL-WL::DEBUG-LOG! "Dispatching ~a:~a~%"
                                       "xwayland_surface_v1" "set-serial")
                    (FUNCALL 'SET-SERIAL RESOURCE
                             (VALUES
                              (FOREIGN-SLOT-VALUE
                               (MEM-APTR ARGS '(:UNION CL-WL.FFI:WL_ARGUMENT)
                                0)
                               '(:UNION CL-WL.FFI:WL_ARGUMENT)
                               'WL-FFI::U))
                             (VALUES
                              (FOREIGN-SLOT-VALUE
                               (MEM-APTR ARGS '(:UNION CL-WL.FFI:WL_ARGUMENT)
                                1)
                               '(:UNION CL-WL.FFI:WL_ARGUMENT)
                               'WL-FFI::U))))
                   (1
                    (CL-WL::DEBUG-LOG! "Dispatching ~a:~a~%"
                                       "xwayland_surface_v1" "destroy")
                    (FUNCALL 'DESTROY RESOURCE)))
     (KILL-CLIENT NIL :REPORT "Kill the client causing errors"
      (CL-WL:DESTROY-CLIENT (CL-WL:CLIENT RESOURCE)) 0)))
 0)

(DEFVAR *DISPATCHER* (CALLBACK DISPATCHER-FFI))

(DEFMETHOD SHARED-INITIALIZE :AFTER ((INSTANCE DISPATCH) SLOT-NAMES &KEY ID)
  (DECLARE (IGNORE SLOT-NAMES))
  (UNLESS (XWAYLAND_SURFACE_V1-ID INSTANCE)
    (LET* ((RESOURCE
            (CL-WL::CREATE-RESOURCE (CL-WL:PTR (CL-WL:CLIENT INSTANCE))
                                    *INTERFACE* (VERSION INSTANCE)
                                    (OR ID 0))))
      (SETF (CL-WL::TRANSIENT-ID INSTANCE)
              (SETF (XWAYLAND_SURFACE_V1-ID INSTANCE)
                      (OR ID (CL-WL.FFI:RESOURCE-GET-ID RESOURCE))))
      (SETF (XWAYLAND_SURFACE_V1-PTR INSTANCE) RESOURCE)
      (SETF (GETHASH (POINTER-ADDRESS RESOURCE) CL-WL::*RESOURCE-TRACKER*)
              INSTANCE)
      (RESOURCE-SET-DISPATCHER RESOURCE *DISPATCHER* (NULL-POINTER)
       (NULL-POINTER) (NULL-POINTER)))))

(DEFUN ERROR-FROM-VALUE (NUMBER)
  (LOOP FOR ENTRY IN '((0 :ALREADY-ASSOCIATED)
                       (1 :INVALID-SERIAL))
        FOR VALUE = (CAR ENTRY)
        FOR KEYWORD = (CADR ENTRY)
        WHEN (EQ NUMBER VALUE) RETURN KEYWORD
        FINALLY (ERROR (FORMAT NIL "Unknown enum value: ~a" NUMBER))))

(DEFUN ERROR-TO-VALUE (KEY)
  (LOOP FOR ENTRY IN '((0 :ALREADY-ASSOCIATED)
                       (1 :INVALID-SERIAL))
        FOR VALUE = (CAR ENTRY)
        FOR KEYWORD = (CADR ENTRY)
        WHEN (EQ KEY KEYWORD) RETURN VALUE
        FINALLY (ERROR (FORMAT NIL "Unknown enum keyword: ~a" KEY))))

(DEFCLASS GLOBAL (CL-WL:GLOBAL) NIL
          (:DEFAULT-INITARGS :VERSION 1 :DISPATCH-IMPL 'DISPATCH)
          (:DOCUMENTATION
           "interface for associating Xwayland windows to wl_surfaces

An Xwayland surface is a surface managed by an Xwayland server.
      It is used for associating surfaces to Xwayland windows.

      The Xwayland server associated with actions in this interface is
      determined by the Wayland client making the request.

      The client must call wl_surface.commit on the corresponding wl_surface
      for the xwayland_surface_v1 state to take effect.
"))

(DEFMETHOD DISPATCH-BIND ((GLOBAL GLOBAL) CLIENT DATA VERSION ID)
  "Default bind implementation for the xwayland_surface_v1 global object.
This can be overriden by inheritance in case if custom behaviour is required."
  (CL-WL::DEBUG-LOG! "Binding ~a~%" "xwayland_surface_v1")
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
  (CL-WL::DEBUG-LOG! "Initializing global object: ~a~%" "xwayland_surface_v1")
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

