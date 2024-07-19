(DEFPACKAGE :ZXDG_DECORATION_MANAGER_V1
  (:USE :CL :CFFI :CL-WL.FFI)
  (:NICKNAMES :ZXDG-DECORATION-MANAGER-V1)
  (:EXPORT DISPATCH
           GLOBAL
           DISPATCH-BIND
           DESTROY
           GET-TOPLEVEL-DECORATION))

(IN-PACKAGE :ZXDG_DECORATION_MANAGER_V1)

(DEFCSTRUCT INTERFACE (NAME :STRING) (VERSION :INT) (METHOD_COUNT :INT)
 (METHODS (:POINTER (:STRUCT CL-WL.FFI:WL_MESSAGE))) (EVENT_COUNT :INT)
 (EVENTS (:POINTER (:STRUCT CL-WL.FFI:WL_MESSAGE))))

(DEFVAR *INTERFACE* NIL)

(DEFPACKAGE :ZXDG_TOPLEVEL_DECORATION_V1
  (:USE :CL :CFFI :CL-WL.FFI)
  (:NICKNAMES :ZXDG-TOPLEVEL-DECORATION-V1)
  (:EXPORT DISPATCH
           GLOBAL
           DISPATCH-BIND
           DESTROY
           SET-MODE
           UNSET-MODE
           SEND-CONFIGURE))

(IN-PACKAGE :ZXDG_TOPLEVEL_DECORATION_V1)

(DEFCSTRUCT INTERFACE (NAME :STRING) (VERSION :INT) (METHOD_COUNT :INT)
 (METHODS (:POINTER (:STRUCT CL-WL.FFI:WL_MESSAGE))) (EVENT_COUNT :INT)
 (EVENTS (:POINTER (:STRUCT CL-WL.FFI:WL_MESSAGE))))

(DEFVAR *INTERFACE* NIL)

(IN-PACKAGE :ZXDG_DECORATION_MANAGER_V1)

(DEFCLASS DISPATCH (CL-WL:OBJECT)
          ((ZXDG_DECORATION_MANAGER_V1-ID :INITFORM NIL :ACCESSOR
            ZXDG_DECORATION_MANAGER_V1-ID)
           (ZXDG_DECORATION_MANAGER_V1-PTR :INITFORM NIL :ACCESSOR
            ZXDG_DECORATION_MANAGER_V1-PTR))
          (:DEFAULT-INITARGS :VERSION 1)
          (:DOCUMENTATION "window decoration manager

This interface allows a compositor to announce support for server-side
      decorations.

      A window decoration is a set of window controls as deemed appropriate by
      the party managing them, such as user interface components used to move,
      resize and change a window's state.

      A client can use this protocol to request being decorated by a supporting
      compositor.

      If compositor and client do not negotiate the use of a server-side
      decoration using this protocol, clients continue to self-decorate as they
      see fit.

      Warning! The protocol described in this file is experimental and
      backward incompatible changes may be made. Backward compatible changes
      may be added together with the corresponding interface version bump.
      Backward incompatible changes are done by bumping the version number in
      the protocol and interface names and resetting the interface version.
      Once the protocol is to be declared stable, the 'z' prefix and the
      version number in the protocol and interface names are removed and the
      interface version number is reset.
"))

(DEFGENERIC DESTROY
    (RESOURCE))

(DEFGENERIC GET-TOPLEVEL-DECORATION
    (RESOURCE ID TOPLEVEL))

(DEFMETHOD CL-WL:DESTROY ((DISPATCH DISPATCH))
  (CL-WL::DEBUG-LOG! "Destroying dispatch object: ~a~%"
                     "zxdg_decoration_manager_v1")
  (WHEN (CL-WL::DESTROY-CALLBACK DISPATCH)
    (LOOP FOR CALLBACK IN (CL-WL::DESTROY-CALLBACK DISPATCH)
          DO (FUNCALL CALLBACK DISPATCH)))
  (LET ((CLIENT (CL-WL:CLIENT DISPATCH))
        (RESOURCE-PTR (ZXDG_DECORATION_MANAGER_V1-PTR DISPATCH)))
    (CL-WL:REMOVE-CLIENT-OBJECT CLIENT
                                (ZXDG_DECORATION_MANAGER_V1-ID
                                 DISPATCH))
    (CL-WL::REMOVE-RESOURCE (POINTER-ADDRESS RESOURCE-PTR)))
  (CL-WL::DN-IF DISPATCH))

(DEFMETHOD DESTROY ((DISPATCH DISPATCH)) (CL-WL:DESTROY DISPATCH))

(DEFMETHOD DESTROY (EMPTY) NIL)

(PUSHNEW
 (LIST "zxdg_decoration_manager_v1"
       (LIST "zxdg_toplevel_decoration_v1" "xdg_toplevel")
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
                            ZXDG_TOPLEVEL_DECORATION_V1::*INTERFACE*)
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 1)
                            XDG_TOPLEVEL::*INTERFACE*)
                    (WITH-FOREIGN-SLOTS
                     ((NAME SIGNATURE TYPES) MSG-PTR
                      (:STRUCT CL-WL.FFI:WL_MESSAGE))
                     (SETF NAME
                             (FOREIGN-STRING-ALLOC "get_toplevel_decoration")
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
            (SETF NAME (FOREIGN-STRING-ALLOC "zxdg_decoration_manager_v1")
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
                                       "zxdg_decoration_manager_v1" "destroy")
                    (FUNCALL 'DESTROY RESOURCE))
                   (1
                    (CL-WL::DEBUG-LOG! "Dispatching ~a:~a~%"
                                       "zxdg_decoration_manager_v1"
                                       "get-toplevel-decoration")
                    (FUNCALL 'GET-TOPLEVEL-DECORATION RESOURCE
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
  (UNLESS (ZXDG_DECORATION_MANAGER_V1-ID INSTANCE)
    (LET* ((RESOURCE
            (CL-WL::CREATE-RESOURCE (CL-WL:PTR (CL-WL:CLIENT INSTANCE))
                                    *INTERFACE* (VERSION INSTANCE)
                                    (OR ID 0))))
      (SETF (CL-WL::TRANSIENT-ID INSTANCE)
              (SETF (ZXDG_DECORATION_MANAGER_V1-ID INSTANCE)
                      (OR ID (CL-WL.FFI:RESOURCE-GET-ID RESOURCE))))
      (SETF (ZXDG_DECORATION_MANAGER_V1-PTR INSTANCE) RESOURCE)
      (SETF (GETHASH (POINTER-ADDRESS RESOURCE) CL-WL::*RESOURCE-TRACKER*)
              INSTANCE)
      (RESOURCE-SET-DISPATCHER RESOURCE *DISPATCHER* (NULL-POINTER)
       (NULL-POINTER) (NULL-POINTER)))))

(DEFCLASS GLOBAL (CL-WL:GLOBAL) NIL
          (:DEFAULT-INITARGS :VERSION 1 :DISPATCH-IMPL 'DISPATCH)
          (:DOCUMENTATION "window decoration manager

This interface allows a compositor to announce support for server-side
      decorations.

      A window decoration is a set of window controls as deemed appropriate by
      the party managing them, such as user interface components used to move,
      resize and change a window's state.

      A client can use this protocol to request being decorated by a supporting
      compositor.

      If compositor and client do not negotiate the use of a server-side
      decoration using this protocol, clients continue to self-decorate as they
      see fit.

      Warning! The protocol described in this file is experimental and
      backward incompatible changes may be made. Backward compatible changes
      may be added together with the corresponding interface version bump.
      Backward incompatible changes are done by bumping the version number in
      the protocol and interface names and resetting the interface version.
      Once the protocol is to be declared stable, the 'z' prefix and the
      version number in the protocol and interface names are removed and the
      interface version number is reset.
"))

(DEFMETHOD DISPATCH-BIND ((GLOBAL GLOBAL) CLIENT DATA VERSION ID)
  "Default bind implementation for the zxdg_decoration_manager_v1 global object.
This can be overriden by inheritance in case if custom behaviour is required."
  (CL-WL::DEBUG-LOG! "Binding ~a~%" "zxdg_decoration_manager_v1")
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
                     "zxdg_decoration_manager_v1")
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

(IN-PACKAGE :ZXDG_TOPLEVEL_DECORATION_V1)

(DEFCLASS DISPATCH (CL-WL:OBJECT)
          ((ZXDG_TOPLEVEL_DECORATION_V1-ID :INITFORM NIL :ACCESSOR
            ZXDG_TOPLEVEL_DECORATION_V1-ID)
           (ZXDG_TOPLEVEL_DECORATION_V1-PTR :INITFORM NIL :ACCESSOR
            ZXDG_TOPLEVEL_DECORATION_V1-PTR))
          (:DEFAULT-INITARGS :VERSION 1)
          (:DOCUMENTATION "decoration object for a toplevel surface

The decoration object allows the compositor to toggle server-side window
      decorations for a toplevel surface. The client can request to switch to
      another mode.

      The xdg_toplevel_decoration object must be destroyed before its
      xdg_toplevel.
"))

(DEFGENERIC DESTROY
    (RESOURCE))

(DEFGENERIC SET-MODE
    (RESOURCE MODE))

(DEFGENERIC UNSET-MODE
    (RESOURCE))

(DEFMETHOD CL-WL:DESTROY ((DISPATCH DISPATCH))
  (CL-WL::DEBUG-LOG! "Destroying dispatch object: ~a~%"
                     "zxdg_toplevel_decoration_v1")
  (WHEN (CL-WL::DESTROY-CALLBACK DISPATCH)
    (LOOP FOR CALLBACK IN (CL-WL::DESTROY-CALLBACK DISPATCH)
          DO (FUNCALL CALLBACK DISPATCH)))
  (LET ((CLIENT (CL-WL:CLIENT DISPATCH))
        (RESOURCE-PTR (ZXDG_TOPLEVEL_DECORATION_V1-PTR DISPATCH)))
    (CL-WL:REMOVE-CLIENT-OBJECT CLIENT
                                (ZXDG_TOPLEVEL_DECORATION_V1-ID
                                 DISPATCH))
    (CL-WL::REMOVE-RESOURCE (POINTER-ADDRESS RESOURCE-PTR)))
  (CL-WL::DN-IF DISPATCH))

(DEFMETHOD DESTROY ((DISPATCH DISPATCH)) (CL-WL:DESTROY DISPATCH))

(DEFMETHOD DESTROY (EMPTY) NIL)

(PUSHNEW
 (LIST "zxdg_toplevel_decoration_v1" (LIST)
       (LAMBDA ()
         (SETF *INTERFACE* (FOREIGN-ALLOC '(:STRUCT INTERFACE)))
         (LET ((REQUESTS-PTR
                (LET ((MESSAGES
                       (CFFI:FOREIGN-ALLOC '(:STRUCT CL-WL.FFI:WL_MESSAGE)
                                           :COUNT 3)))
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
                                             :COUNT 1))
                        (MSG-PTR
                         (MEM-APTR MESSAGES '(:STRUCT CL-WL.FFI:WL_MESSAGE) 1)))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 0) (NULL-POINTER))
                    (WITH-FOREIGN-SLOTS
                     ((NAME SIGNATURE TYPES) MSG-PTR
                      (:STRUCT CL-WL.FFI:WL_MESSAGE))
                     (SETF NAME (FOREIGN-STRING-ALLOC "set_mode")
                           SIGNATURE "u"
                           TYPES INTERFACE-ARRAY)))
                  (LET ((INTERFACE-ARRAY
                         (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID))
                                             :COUNT 0))
                        (MSG-PTR
                         (MEM-APTR MESSAGES '(:STRUCT CL-WL.FFI:WL_MESSAGE) 2)))
                    (WITH-FOREIGN-SLOTS
                     ((NAME SIGNATURE TYPES) MSG-PTR
                      (:STRUCT CL-WL.FFI:WL_MESSAGE))
                     (SETF NAME (FOREIGN-STRING-ALLOC "unset_mode")
                           SIGNATURE ""
                           TYPES INTERFACE-ARRAY)))
                  MESSAGES))
               (EVENTS-PTR
                (LET ((MESSAGES
                       (CFFI:FOREIGN-ALLOC '(:STRUCT CL-WL.FFI:WL_MESSAGE)
                                           :COUNT 1)))
                  (LET ((INTERFACE-ARRAY
                         (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID))
                                             :COUNT 1))
                        (MSG-PTR
                         (MEM-APTR MESSAGES '(:STRUCT CL-WL.FFI:WL_MESSAGE) 0)))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 0) (NULL-POINTER))
                    (WITH-FOREIGN-SLOTS
                     ((NAME SIGNATURE TYPES) MSG-PTR
                      (:STRUCT CL-WL.FFI:WL_MESSAGE))
                     (SETF NAME (FOREIGN-STRING-ALLOC "configure")
                           SIGNATURE "u"
                           TYPES INTERFACE-ARRAY)))
                  MESSAGES)))
           (WITH-FOREIGN-SLOTS
            ((NAME VERSION METHOD_COUNT METHODS EVENT_COUNT EVENTS) *INTERFACE*
             (:STRUCT INTERFACE))
            (SETF NAME (FOREIGN-STRING-ALLOC "zxdg_toplevel_decoration_v1")
                  VERSION 1
                  METHOD_COUNT 3
                  METHODS REQUESTS-PTR
                  EVENT_COUNT 1
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
                                       "zxdg_toplevel_decoration_v1" "destroy")
                    (FUNCALL 'DESTROY RESOURCE))
                   (1
                    (CL-WL::DEBUG-LOG! "Dispatching ~a:~a~%"
                                       "zxdg_toplevel_decoration_v1"
                                       "set-mode")
                    (FUNCALL 'SET-MODE RESOURCE
                             (MODE-FROM-VALUE
                              (FOREIGN-SLOT-VALUE
                               (MEM-APTR ARGS '(:UNION CL-WL.FFI:WL_ARGUMENT)
                                0)
                               '(:UNION CL-WL.FFI:WL_ARGUMENT)
                               'WL-FFI::U))))
                   (2
                    (CL-WL::DEBUG-LOG! "Dispatching ~a:~a~%"
                                       "zxdg_toplevel_decoration_v1"
                                       "unset-mode")
                    (FUNCALL 'UNSET-MODE RESOURCE)))
     (KILL-CLIENT NIL :REPORT "Kill the client causing errors"
      (CL-WL:DESTROY-CLIENT (CL-WL:CLIENT RESOURCE)) 0)))
 0)

(DEFVAR *DISPATCHER* (CALLBACK DISPATCHER-FFI))

(DEFMETHOD SHARED-INITIALIZE :AFTER ((INSTANCE DISPATCH) SLOT-NAMES &KEY ID)
  (DECLARE (IGNORE SLOT-NAMES))
  (UNLESS (ZXDG_TOPLEVEL_DECORATION_V1-ID INSTANCE)
    (LET* ((RESOURCE
            (CL-WL::CREATE-RESOURCE (CL-WL:PTR (CL-WL:CLIENT INSTANCE))
                                    *INTERFACE* (VERSION INSTANCE)
                                    (OR ID 0))))
      (SETF (CL-WL::TRANSIENT-ID INSTANCE)
              (SETF (ZXDG_TOPLEVEL_DECORATION_V1-ID INSTANCE)
                      (OR ID (CL-WL.FFI:RESOURCE-GET-ID RESOURCE))))
      (SETF (ZXDG_TOPLEVEL_DECORATION_V1-PTR INSTANCE) RESOURCE)
      (SETF (GETHASH (POINTER-ADDRESS RESOURCE) CL-WL::*RESOURCE-TRACKER*)
              INSTANCE)
      (RESOURCE-SET-DISPATCHER RESOURCE *DISPATCHER* (NULL-POINTER)
       (NULL-POINTER) (NULL-POINTER)))))

(DEFMETHOD SEND-CONFIGURE ((DISPATCH DISPATCH) MODE)
  (CL-WL::DEBUG-LOG! "Event: ~a~%" "configure")
  (LET ((ARG-LIST (FOREIGN-ALLOC '(:UNION WL_ARGUMENT) :COUNT 1)))
    (SETF (FOREIGN-SLOT-VALUE
           (MEM-AREF ARG-LIST '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 0)
           '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 'WL-FFI::U)
            (MODE-TO-VALUE MODE))
    (RESOURCE-POST-EVENT-ARRAY
     (ZXDG_TOPLEVEL_DECORATION_V1-PTR DISPATCH) 0 ARG-LIST)))

(DEFUN ERROR-FROM-VALUE (NUMBER)
  (LOOP FOR ENTRY IN '((0 :UNCONFIGURED-BUFFER)
                       (1 :ALREADY-CONSTRUCTED)
                       (2 :ORPHANED))
        FOR VALUE = (CAR ENTRY)
        FOR KEYWORD = (CADR ENTRY)
        WHEN (EQ NUMBER VALUE) RETURN KEYWORD
        FINALLY (ERROR (FORMAT NIL "Unknown enum value: ~a" NUMBER))))

(DEFUN ERROR-TO-VALUE (KEY)
  (LOOP FOR ENTRY IN '((0 :UNCONFIGURED-BUFFER)
                       (1 :ALREADY-CONSTRUCTED)
                       (2 :ORPHANED))
        FOR VALUE = (CAR ENTRY)
        FOR KEYWORD = (CADR ENTRY)
        WHEN (EQ KEY KEYWORD) RETURN VALUE
        FINALLY (ERROR (FORMAT NIL "Unknown enum keyword: ~a" KEY))))

(DEFUN MODE-FROM-VALUE (NUMBER)
  (LOOP FOR ENTRY IN '((1 :CLIENT-SIDE)
                       (2 :SERVER-SIDE))
        FOR VALUE = (CAR ENTRY)
        FOR KEYWORD = (CADR ENTRY)
        WHEN (EQ NUMBER VALUE) RETURN KEYWORD
        FINALLY (ERROR (FORMAT NIL "Unknown enum value: ~a" NUMBER))))

(DEFUN MODE-TO-VALUE (KEY)
  (LOOP FOR ENTRY IN '((1 :CLIENT-SIDE)
                       (2 :SERVER-SIDE))
        FOR VALUE = (CAR ENTRY)
        FOR KEYWORD = (CADR ENTRY)
        WHEN (EQ KEY KEYWORD) RETURN VALUE
        FINALLY (ERROR (FORMAT NIL "Unknown enum keyword: ~a" KEY))))

(DEFCLASS GLOBAL (CL-WL:GLOBAL) NIL
          (:DEFAULT-INITARGS :VERSION 1 :DISPATCH-IMPL 'DISPATCH)
          (:DOCUMENTATION "decoration object for a toplevel surface

The decoration object allows the compositor to toggle server-side window
      decorations for a toplevel surface. The client can request to switch to
      another mode.

      The xdg_toplevel_decoration object must be destroyed before its
      xdg_toplevel.
"))

(DEFMETHOD DISPATCH-BIND ((GLOBAL GLOBAL) CLIENT DATA VERSION ID)
  "Default bind implementation for the zxdg_toplevel_decoration_v1 global object.
This can be overriden by inheritance in case if custom behaviour is required."
  (CL-WL::DEBUG-LOG! "Binding ~a~%" "zxdg_toplevel_decoration_v1")
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
                     "zxdg_toplevel_decoration_v1")
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

