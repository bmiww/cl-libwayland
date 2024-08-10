(DEFPACKAGE :EXT_SESSION_LOCK_MANAGER_V1
  (:USE :CL :CFFI :CL-WL.FFI)
  (:NICKNAMES :EXT-SESSION-LOCK-MANAGER-V1)
  (:EXPORT DISPATCH GLOBAL DISPATCH-BIND DESTROY LOCK))

(IN-PACKAGE :EXT_SESSION_LOCK_MANAGER_V1)

(DEFCSTRUCT INTERFACE (NAME :STRING) (VERSION :INT) (METHOD_COUNT :INT)
 (METHODS (:POINTER (:STRUCT CL-WL.FFI:WL_MESSAGE))) (EVENT_COUNT :INT)
 (EVENTS (:POINTER (:STRUCT CL-WL.FFI:WL_MESSAGE))))

(DEFVAR *INTERFACE* NIL)

(DEFPACKAGE :EXT_SESSION_LOCK_V1
  (:USE :CL :CFFI :CL-WL.FFI)
  (:NICKNAMES :EXT-SESSION-LOCK-V1)
  (:EXPORT DISPATCH
           GLOBAL
           DISPATCH-BIND
           DESTROY
           GET-LOCK-SURFACE
           UNLOCK-AND-DESTROY
           SEND-LOCKED
           SEND-FINISHED))

(IN-PACKAGE :EXT_SESSION_LOCK_V1)

(DEFCSTRUCT INTERFACE (NAME :STRING) (VERSION :INT) (METHOD_COUNT :INT)
 (METHODS (:POINTER (:STRUCT CL-WL.FFI:WL_MESSAGE))) (EVENT_COUNT :INT)
 (EVENTS (:POINTER (:STRUCT CL-WL.FFI:WL_MESSAGE))))

(DEFVAR *INTERFACE* NIL)

(DEFPACKAGE :EXT_SESSION_LOCK_SURFACE_V1
  (:USE :CL :CFFI :CL-WL.FFI)
  (:NICKNAMES :EXT-SESSION-LOCK-SURFACE-V1)
  (:EXPORT DISPATCH
           GLOBAL
           DISPATCH-BIND
           DESTROY
           ACK-CONFIGURE
           SEND-CONFIGURE))

(IN-PACKAGE :EXT_SESSION_LOCK_SURFACE_V1)

(DEFCSTRUCT INTERFACE (NAME :STRING) (VERSION :INT) (METHOD_COUNT :INT)
 (METHODS (:POINTER (:STRUCT CL-WL.FFI:WL_MESSAGE))) (EVENT_COUNT :INT)
 (EVENTS (:POINTER (:STRUCT CL-WL.FFI:WL_MESSAGE))))

(DEFVAR *INTERFACE* NIL)

(IN-PACKAGE :EXT_SESSION_LOCK_MANAGER_V1)

(DEFCLASS DISPATCH (CL-WL:OBJECT)
          ((EXT_SESSION_LOCK_MANAGER_V1-ID :INITFORM NIL :ACCESSOR
            EXT_SESSION_LOCK_MANAGER_V1-ID)
           (EXT_SESSION_LOCK_MANAGER_V1-PTR :INITFORM NIL :ACCESSOR
            EXT_SESSION_LOCK_MANAGER_V1-PTR))
          (:DEFAULT-INITARGS :VERSION 1)
          (:DOCUMENTATION "used to lock the session

This interface is used to request that the session be locked.
"))

(DEFGENERIC DESTROY
    (RESOURCE))

(DEFGENERIC LOCK
    (RESOURCE ID))

(DEFCONTINUE:DEFCONTINUE CL-WL:DESTROY ((DISPATCH DISPATCH))
                         (CL-WL::DEBUG-LOG! "Destroying dispatch object: ~a~%"
                                            "ext_session_lock_manager_v1")
                         (LET ((CLIENT (CL-WL:CLIENT DISPATCH))
                               (RESOURCE-PTR
                                (EXT_SESSION_LOCK_MANAGER_V1-PTR
                                 DISPATCH)))
                           (CL-WL:REMOVE-CLIENT-OBJECT CLIENT
                                                       (EXT_SESSION_LOCK_MANAGER_V1-ID
                                                        DISPATCH))
                           (CL-WL::REMOVE-RESOURCE
                            (POINTER-ADDRESS RESOURCE-PTR)))
                         (CL-WL::DN-IF DISPATCH))

(DEFMETHOD DESTROY ((DISPATCH DISPATCH)) (CL-WL:DESTROY DISPATCH))

(DEFMETHOD DESTROY (EMPTY) NIL)

(PUSHNEW
 (LIST "ext_session_lock_manager_v1" (LIST "ext_session_lock_v1")
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
                                             :COUNT 1))
                        (MSG-PTR
                         (MEM-APTR MESSAGES '(:STRUCT CL-WL.FFI:WL_MESSAGE) 1)))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 0)
                            EXT_SESSION_LOCK_V1::*INTERFACE*)
                    (WITH-FOREIGN-SLOTS
                     ((NAME SIGNATURE TYPES) MSG-PTR
                      (:STRUCT CL-WL.FFI:WL_MESSAGE))
                     (SETF NAME (FOREIGN-STRING-ALLOC "lock")
                           SIGNATURE "n"
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
            (SETF NAME (FOREIGN-STRING-ALLOC "ext_session_lock_manager_v1")
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
                                       "ext_session_lock_manager_v1" "destroy")
                    (FUNCALL 'DESTROY RESOURCE))
                   (1
                    (CL-WL::DEBUG-LOG! "Dispatching ~a:~a~%"
                                       "ext_session_lock_manager_v1" "lock")
                    (FUNCALL 'LOCK RESOURCE
                             (VALUES
                              (FOREIGN-SLOT-VALUE
                               (MEM-APTR ARGS '(:UNION CL-WL.FFI:WL_ARGUMENT)
                                0)
                               '(:UNION CL-WL.FFI:WL_ARGUMENT)
                               'WL-FFI::N)))))
     (KILL-CLIENT NIL :REPORT "Kill the client causing errors"
      (CL-WL:DESTROY-CLIENT (CL-WL:CLIENT RESOURCE)) 0)))
 0)

(DEFVAR *DISPATCHER* (CALLBACK DISPATCHER-FFI))

(DEFMETHOD SHARED-INITIALIZE :AFTER ((INSTANCE DISPATCH) SLOT-NAMES &KEY ID)
  (DECLARE (IGNORE SLOT-NAMES))
  (UNLESS (EXT_SESSION_LOCK_MANAGER_V1-ID INSTANCE)
    (LET* ((RESOURCE
            (CL-WL::CREATE-RESOURCE (CL-WL:PTR (CL-WL:CLIENT INSTANCE))
                                    *INTERFACE* (VERSION INSTANCE)
                                    (OR ID 0))))
      (SETF (CL-WL::TRANSIENT-ID INSTANCE)
              (SETF (EXT_SESSION_LOCK_MANAGER_V1-ID INSTANCE)
                      (OR ID (CL-WL.FFI:RESOURCE-GET-ID RESOURCE))))
      (SETF (EXT_SESSION_LOCK_MANAGER_V1-PTR INSTANCE) RESOURCE)
      (SETF (GETHASH (POINTER-ADDRESS RESOURCE) CL-WL::*RESOURCE-TRACKER*)
              INSTANCE)
      (RESOURCE-SET-DISPATCHER RESOURCE *DISPATCHER* (NULL-POINTER)
       (NULL-POINTER) (NULL-POINTER)))))

(DEFCLASS GLOBAL (CL-WL:GLOBAL) NIL
          (:DEFAULT-INITARGS :VERSION 1 :DISPATCH-IMPL 'DISPATCH)
          (:DOCUMENTATION "used to lock the session

This interface is used to request that the session be locked.
"))

(DEFMETHOD DISPATCH-BIND ((GLOBAL GLOBAL) CLIENT DATA VERSION ID)
  "Default bind implementation for the ext_session_lock_manager_v1 global object.
This can be overriden by inheritance in case if custom behaviour is required."
  (CL-WL::DEBUG-LOG! "Binding ~a~%" "ext_session_lock_manager_v1")
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
                     "ext_session_lock_manager_v1")
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

(IN-PACKAGE :EXT_SESSION_LOCK_V1)

(DEFCLASS DISPATCH (CL-WL:OBJECT)
          ((EXT_SESSION_LOCK_V1-ID :INITFORM NIL :ACCESSOR
            EXT_SESSION_LOCK_V1-ID)
           (EXT_SESSION_LOCK_V1-PTR :INITFORM NIL :ACCESSOR
            EXT_SESSION_LOCK_V1-PTR))
          (:DEFAULT-INITARGS :VERSION 1)
          (:DOCUMENTATION "manage lock state and create lock surfaces

In response to the creation of this object the compositor must send
      either the locked or finished event.

      The locked event indicates that the session is locked. This means
      that the compositor must stop rendering and providing input to normal
      clients. Instead the compositor must blank all outputs with an opaque
      color such that their normal content is fully hidden.

      The only surfaces that should be rendered while the session is locked
      are the lock surfaces created through this interface and optionally,
      at the compositor's discretion, special privileged surfaces such as
      input methods or portions of desktop shell UIs.

      The locked event must not be sent until a new \"locked\" frame (either
      from a session lock surface or the compositor blanking the output) has
      been presented on all outputs and no security sensitive normal/unlocked
      content is possibly visible.

      The finished event should be sent immediately on creation of this
      object if the compositor decides that the locked event will not be sent.

      The compositor may wait for the client to create and render session lock
      surfaces before sending the locked event to avoid displaying intermediate
      blank frames. However, it must impose a reasonable time limit if
      waiting and send the locked event as soon as the hard requirements
      described above can be met if the time limit expires. Clients should
      immediately create lock surfaces for all outputs on creation of this
      object to make this possible.

      This behavior of the locked event is required in order to prevent
      possible race conditions with clients that wish to suspend the system
      or similar after locking the session. Without these semantics, clients
      triggering a suspend after receiving the locked event would race with
      the first \"locked\" frame being presented and normal/unlocked frames
      might be briefly visible as the system is resumed if the suspend
      operation wins the race.

      If the client dies while the session is locked, the compositor must not
      unlock the session in response. It is acceptable for the session to be
      permanently locked if this happens. The compositor may choose to continue
      to display the lock surfaces the client had mapped before it died or
      alternatively fall back to a solid color, this is compositor policy.

      Compositors may also allow a secure way to recover the session, the
      details of this are compositor policy. Compositors may allow a new
      client to create a ext_session_lock_v1 object and take responsibility
      for unlocking the session, they may even start a new lock client
      instance automatically.
"))

(DEFGENERIC DESTROY
    (RESOURCE))

(DEFGENERIC GET-LOCK-SURFACE
    (RESOURCE ID SURFACE OUTPUT))

(DEFGENERIC UNLOCK-AND-DESTROY
    (RESOURCE))

(DEFCONTINUE:DEFCONTINUE CL-WL:DESTROY ((DISPATCH DISPATCH))
                         (CL-WL::DEBUG-LOG! "Destroying dispatch object: ~a~%"
                                            "ext_session_lock_v1")
                         (LET ((CLIENT (CL-WL:CLIENT DISPATCH))
                               (RESOURCE-PTR
                                (EXT_SESSION_LOCK_V1-PTR DISPATCH)))
                           (CL-WL:REMOVE-CLIENT-OBJECT CLIENT
                                                       (EXT_SESSION_LOCK_V1-ID
                                                        DISPATCH))
                           (CL-WL::REMOVE-RESOURCE
                            (POINTER-ADDRESS RESOURCE-PTR)))
                         (CL-WL::DN-IF DISPATCH))

(DEFMETHOD DESTROY ((DISPATCH DISPATCH)) (CL-WL:DESTROY DISPATCH))

(DEFMETHOD DESTROY (EMPTY) NIL)

(PUSHNEW
 (LIST "ext_session_lock_v1"
       (LIST "ext_session_lock_surface_v1" "wl_surface" "wl_output")
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
                                             :COUNT 3))
                        (MSG-PTR
                         (MEM-APTR MESSAGES '(:STRUCT CL-WL.FFI:WL_MESSAGE) 1)))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 0)
                            EXT_SESSION_LOCK_SURFACE_V1::*INTERFACE*)
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 1)
                            WL_SURFACE::*INTERFACE*)
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 2)
                            WL_OUTPUT::*INTERFACE*)
                    (WITH-FOREIGN-SLOTS
                     ((NAME SIGNATURE TYPES) MSG-PTR
                      (:STRUCT CL-WL.FFI:WL_MESSAGE))
                     (SETF NAME (FOREIGN-STRING-ALLOC "get_lock_surface")
                           SIGNATURE "noo"
                           TYPES INTERFACE-ARRAY)))
                  (LET ((INTERFACE-ARRAY
                         (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID))
                                             :COUNT 0))
                        (MSG-PTR
                         (MEM-APTR MESSAGES '(:STRUCT CL-WL.FFI:WL_MESSAGE) 2)))
                    (WITH-FOREIGN-SLOTS
                     ((NAME SIGNATURE TYPES) MSG-PTR
                      (:STRUCT CL-WL.FFI:WL_MESSAGE))
                     (SETF NAME (FOREIGN-STRING-ALLOC "unlock_and_destroy")
                           SIGNATURE ""
                           TYPES INTERFACE-ARRAY)))
                  MESSAGES))
               (EVENTS-PTR
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
                     (SETF NAME (FOREIGN-STRING-ALLOC "locked")
                           SIGNATURE ""
                           TYPES INTERFACE-ARRAY)))
                  (LET ((INTERFACE-ARRAY
                         (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID))
                                             :COUNT 0))
                        (MSG-PTR
                         (MEM-APTR MESSAGES '(:STRUCT CL-WL.FFI:WL_MESSAGE) 1)))
                    (WITH-FOREIGN-SLOTS
                     ((NAME SIGNATURE TYPES) MSG-PTR
                      (:STRUCT CL-WL.FFI:WL_MESSAGE))
                     (SETF NAME (FOREIGN-STRING-ALLOC "finished")
                           SIGNATURE ""
                           TYPES INTERFACE-ARRAY)))
                  MESSAGES)))
           (WITH-FOREIGN-SLOTS
            ((NAME VERSION METHOD_COUNT METHODS EVENT_COUNT EVENTS) *INTERFACE*
             (:STRUCT INTERFACE))
            (SETF NAME (FOREIGN-STRING-ALLOC "ext_session_lock_v1")
                  VERSION 1
                  METHOD_COUNT 3
                  METHODS REQUESTS-PTR
                  EVENT_COUNT 2
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
                                       "ext_session_lock_v1" "destroy")
                    (FUNCALL 'DESTROY RESOURCE))
                   (1
                    (CL-WL::DEBUG-LOG! "Dispatching ~a:~a~%"
                                       "ext_session_lock_v1"
                                       "get-lock-surface")
                    (FUNCALL 'GET-LOCK-SURFACE RESOURCE
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
                              CL-WL::*RESOURCE-TRACKER*)
                             (GETHASH
                              (POINTER-ADDRESS
                               (FOREIGN-SLOT-VALUE
                                (MEM-APTR ARGS '(:UNION CL-WL.FFI:WL_ARGUMENT)
                                 2)
                                '(:UNION CL-WL.FFI:WL_ARGUMENT)
                                'WL-FFI::O))
                              CL-WL::*RESOURCE-TRACKER*)))
                   (2
                    (CL-WL::DEBUG-LOG! "Dispatching ~a:~a~%"
                                       "ext_session_lock_v1"
                                       "unlock-and-destroy")
                    (FUNCALL 'UNLOCK-AND-DESTROY RESOURCE)))
     (KILL-CLIENT NIL :REPORT "Kill the client causing errors"
      (CL-WL:DESTROY-CLIENT (CL-WL:CLIENT RESOURCE)) 0)))
 0)

(DEFVAR *DISPATCHER* (CALLBACK DISPATCHER-FFI))

(DEFMETHOD SHARED-INITIALIZE :AFTER ((INSTANCE DISPATCH) SLOT-NAMES &KEY ID)
  (DECLARE (IGNORE SLOT-NAMES))
  (UNLESS (EXT_SESSION_LOCK_V1-ID INSTANCE)
    (LET* ((RESOURCE
            (CL-WL::CREATE-RESOURCE (CL-WL:PTR (CL-WL:CLIENT INSTANCE))
                                    *INTERFACE* (VERSION INSTANCE)
                                    (OR ID 0))))
      (SETF (CL-WL::TRANSIENT-ID INSTANCE)
              (SETF (EXT_SESSION_LOCK_V1-ID INSTANCE)
                      (OR ID (CL-WL.FFI:RESOURCE-GET-ID RESOURCE))))
      (SETF (EXT_SESSION_LOCK_V1-PTR INSTANCE) RESOURCE)
      (SETF (GETHASH (POINTER-ADDRESS RESOURCE) CL-WL::*RESOURCE-TRACKER*)
              INSTANCE)
      (RESOURCE-SET-DISPATCHER RESOURCE *DISPATCHER* (NULL-POINTER)
       (NULL-POINTER) (NULL-POINTER)))))

(DEFMETHOD SEND-LOCKED ((DISPATCH DISPATCH))
  (CL-WL::DEBUG-LOG! "Event: ~a~%" "locked")
  (LET ((ARG-LIST (FOREIGN-ALLOC '(:UNION WL_ARGUMENT) :COUNT 0)))
    (RESOURCE-POST-EVENT-ARRAY (EXT_SESSION_LOCK_V1-PTR DISPATCH) 0
     ARG-LIST)))

(DEFMETHOD SEND-FINISHED ((DISPATCH DISPATCH))
  (CL-WL::DEBUG-LOG! "Event: ~a~%" "finished")
  (LET ((ARG-LIST (FOREIGN-ALLOC '(:UNION WL_ARGUMENT) :COUNT 0)))
    (RESOURCE-POST-EVENT-ARRAY (EXT_SESSION_LOCK_V1-PTR DISPATCH) 1
     ARG-LIST)))

(DEFUN ERROR-FROM-VALUE (NUMBER)
  (LOOP FOR ENTRY IN '((0 :INVALID-DESTROY)
                       (1 :INVALID-UNLOCK) (2 :ROLE)
                       (3 :DUPLICATE-OUTPUT)
                       (4 :ALREADY-CONSTRUCTED))
        FOR VALUE = (CAR ENTRY)
        FOR KEYWORD = (CADR ENTRY)
        WHEN (EQ NUMBER VALUE) RETURN KEYWORD
        FINALLY (ERROR (FORMAT NIL "Unknown enum value: ~a" NUMBER))))

(DEFUN ERROR-TO-VALUE (KEY)
  (LOOP FOR ENTRY IN '((0 :INVALID-DESTROY)
                       (1 :INVALID-UNLOCK) (2 :ROLE)
                       (3 :DUPLICATE-OUTPUT)
                       (4 :ALREADY-CONSTRUCTED))
        FOR VALUE = (CAR ENTRY)
        FOR KEYWORD = (CADR ENTRY)
        WHEN (EQ KEY KEYWORD) RETURN VALUE
        FINALLY (ERROR (FORMAT NIL "Unknown enum keyword: ~a" KEY))))

(DEFCLASS GLOBAL (CL-WL:GLOBAL) NIL
          (:DEFAULT-INITARGS :VERSION 1 :DISPATCH-IMPL 'DISPATCH)
          (:DOCUMENTATION "manage lock state and create lock surfaces

In response to the creation of this object the compositor must send
      either the locked or finished event.

      The locked event indicates that the session is locked. This means
      that the compositor must stop rendering and providing input to normal
      clients. Instead the compositor must blank all outputs with an opaque
      color such that their normal content is fully hidden.

      The only surfaces that should be rendered while the session is locked
      are the lock surfaces created through this interface and optionally,
      at the compositor's discretion, special privileged surfaces such as
      input methods or portions of desktop shell UIs.

      The locked event must not be sent until a new \"locked\" frame (either
      from a session lock surface or the compositor blanking the output) has
      been presented on all outputs and no security sensitive normal/unlocked
      content is possibly visible.

      The finished event should be sent immediately on creation of this
      object if the compositor decides that the locked event will not be sent.

      The compositor may wait for the client to create and render session lock
      surfaces before sending the locked event to avoid displaying intermediate
      blank frames. However, it must impose a reasonable time limit if
      waiting and send the locked event as soon as the hard requirements
      described above can be met if the time limit expires. Clients should
      immediately create lock surfaces for all outputs on creation of this
      object to make this possible.

      This behavior of the locked event is required in order to prevent
      possible race conditions with clients that wish to suspend the system
      or similar after locking the session. Without these semantics, clients
      triggering a suspend after receiving the locked event would race with
      the first \"locked\" frame being presented and normal/unlocked frames
      might be briefly visible as the system is resumed if the suspend
      operation wins the race.

      If the client dies while the session is locked, the compositor must not
      unlock the session in response. It is acceptable for the session to be
      permanently locked if this happens. The compositor may choose to continue
      to display the lock surfaces the client had mapped before it died or
      alternatively fall back to a solid color, this is compositor policy.

      Compositors may also allow a secure way to recover the session, the
      details of this are compositor policy. Compositors may allow a new
      client to create a ext_session_lock_v1 object and take responsibility
      for unlocking the session, they may even start a new lock client
      instance automatically.
"))

(DEFMETHOD DISPATCH-BIND ((GLOBAL GLOBAL) CLIENT DATA VERSION ID)
  "Default bind implementation for the ext_session_lock_v1 global object.
This can be overriden by inheritance in case if custom behaviour is required."
  (CL-WL::DEBUG-LOG! "Binding ~a~%" "ext_session_lock_v1")
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
  (CL-WL::DEBUG-LOG! "Initializing global object: ~a~%" "ext_session_lock_v1")
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

(IN-PACKAGE :EXT_SESSION_LOCK_SURFACE_V1)

(DEFCLASS DISPATCH (CL-WL:OBJECT)
          ((EXT_SESSION_LOCK_SURFACE_V1-ID :INITFORM NIL :ACCESSOR
            EXT_SESSION_LOCK_SURFACE_V1-ID)
           (EXT_SESSION_LOCK_SURFACE_V1-PTR :INITFORM NIL :ACCESSOR
            EXT_SESSION_LOCK_SURFACE_V1-PTR))
          (:DEFAULT-INITARGS :VERSION 1)
          (:DOCUMENTATION "a surface displayed while the session is locked

The client may use lock surfaces to display a screensaver, render a
      dialog to enter a password and unlock the session, or however else it
      sees fit.

      On binding this interface the compositor will immediately send the
      first configure event. After making the ack_configure request in
      response to this event the client should attach and commit the first
      buffer. Committing the surface before acking the first configure is a
      protocol error. Committing the surface with a null buffer at any time
      is a protocol error.

      The compositor is free to handle keyboard/pointer focus for lock
      surfaces however it chooses. A reasonable way to do this would be to
      give the first lock surface created keyboard focus and change keyboard
      focus if the user clicks on other surfaces.
"))

(DEFGENERIC DESTROY
    (RESOURCE))

(DEFGENERIC ACK-CONFIGURE
    (RESOURCE SERIAL))

(DEFCONTINUE:DEFCONTINUE CL-WL:DESTROY ((DISPATCH DISPATCH))
                         (CL-WL::DEBUG-LOG! "Destroying dispatch object: ~a~%"
                                            "ext_session_lock_surface_v1")
                         (LET ((CLIENT (CL-WL:CLIENT DISPATCH))
                               (RESOURCE-PTR
                                (EXT_SESSION_LOCK_SURFACE_V1-PTR
                                 DISPATCH)))
                           (CL-WL:REMOVE-CLIENT-OBJECT CLIENT
                                                       (EXT_SESSION_LOCK_SURFACE_V1-ID
                                                        DISPATCH))
                           (CL-WL::REMOVE-RESOURCE
                            (POINTER-ADDRESS RESOURCE-PTR)))
                         (CL-WL::DN-IF DISPATCH))

(DEFMETHOD DESTROY ((DISPATCH DISPATCH)) (CL-WL:DESTROY DISPATCH))

(DEFMETHOD DESTROY (EMPTY) NIL)

(PUSHNEW
 (LIST "ext_session_lock_surface_v1" (LIST)
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
                                             :COUNT 1))
                        (MSG-PTR
                         (MEM-APTR MESSAGES '(:STRUCT CL-WL.FFI:WL_MESSAGE) 1)))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 0) (NULL-POINTER))
                    (WITH-FOREIGN-SLOTS
                     ((NAME SIGNATURE TYPES) MSG-PTR
                      (:STRUCT CL-WL.FFI:WL_MESSAGE))
                     (SETF NAME (FOREIGN-STRING-ALLOC "ack_configure")
                           SIGNATURE "u"
                           TYPES INTERFACE-ARRAY)))
                  MESSAGES))
               (EVENTS-PTR
                (LET ((MESSAGES
                       (CFFI:FOREIGN-ALLOC '(:STRUCT CL-WL.FFI:WL_MESSAGE)
                                           :COUNT 1)))
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
                  MESSAGES)))
           (WITH-FOREIGN-SLOTS
            ((NAME VERSION METHOD_COUNT METHODS EVENT_COUNT EVENTS) *INTERFACE*
             (:STRUCT INTERFACE))
            (SETF NAME (FOREIGN-STRING-ALLOC "ext_session_lock_surface_v1")
                  VERSION 1
                  METHOD_COUNT 2
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
                                       "ext_session_lock_surface_v1" "destroy")
                    (FUNCALL 'DESTROY RESOURCE))
                   (1
                    (CL-WL::DEBUG-LOG! "Dispatching ~a:~a~%"
                                       "ext_session_lock_surface_v1"
                                       "ack-configure")
                    (FUNCALL 'ACK-CONFIGURE RESOURCE
                             (VALUES
                              (FOREIGN-SLOT-VALUE
                               (MEM-APTR ARGS '(:UNION CL-WL.FFI:WL_ARGUMENT)
                                0)
                               '(:UNION CL-WL.FFI:WL_ARGUMENT)
                               'WL-FFI::U)))))
     (KILL-CLIENT NIL :REPORT "Kill the client causing errors"
      (CL-WL:DESTROY-CLIENT (CL-WL:CLIENT RESOURCE)) 0)))
 0)

(DEFVAR *DISPATCHER* (CALLBACK DISPATCHER-FFI))

(DEFMETHOD SHARED-INITIALIZE :AFTER ((INSTANCE DISPATCH) SLOT-NAMES &KEY ID)
  (DECLARE (IGNORE SLOT-NAMES))
  (UNLESS (EXT_SESSION_LOCK_SURFACE_V1-ID INSTANCE)
    (LET* ((RESOURCE
            (CL-WL::CREATE-RESOURCE (CL-WL:PTR (CL-WL:CLIENT INSTANCE))
                                    *INTERFACE* (VERSION INSTANCE)
                                    (OR ID 0))))
      (SETF (CL-WL::TRANSIENT-ID INSTANCE)
              (SETF (EXT_SESSION_LOCK_SURFACE_V1-ID INSTANCE)
                      (OR ID (CL-WL.FFI:RESOURCE-GET-ID RESOURCE))))
      (SETF (EXT_SESSION_LOCK_SURFACE_V1-PTR INSTANCE) RESOURCE)
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
    (RESOURCE-POST-EVENT-ARRAY
     (EXT_SESSION_LOCK_SURFACE_V1-PTR DISPATCH) 0 ARG-LIST)))

(DEFUN ERROR-FROM-VALUE (NUMBER)
  (LOOP FOR ENTRY IN '((0 :COMMIT-BEFORE-FIRST-ACK)
                       (1 :NULL-BUFFER)
                       (2 :DIMENSIONS-MISMATCH)
                       (3 :INVALID-SERIAL))
        FOR VALUE = (CAR ENTRY)
        FOR KEYWORD = (CADR ENTRY)
        WHEN (EQ NUMBER VALUE) RETURN KEYWORD
        FINALLY (ERROR (FORMAT NIL "Unknown enum value: ~a" NUMBER))))

(DEFUN ERROR-TO-VALUE (KEY)
  (LOOP FOR ENTRY IN '((0 :COMMIT-BEFORE-FIRST-ACK)
                       (1 :NULL-BUFFER)
                       (2 :DIMENSIONS-MISMATCH)
                       (3 :INVALID-SERIAL))
        FOR VALUE = (CAR ENTRY)
        FOR KEYWORD = (CADR ENTRY)
        WHEN (EQ KEY KEYWORD) RETURN VALUE
        FINALLY (ERROR (FORMAT NIL "Unknown enum keyword: ~a" KEY))))

(DEFCLASS GLOBAL (CL-WL:GLOBAL) NIL
          (:DEFAULT-INITARGS :VERSION 1 :DISPATCH-IMPL 'DISPATCH)
          (:DOCUMENTATION "a surface displayed while the session is locked

The client may use lock surfaces to display a screensaver, render a
      dialog to enter a password and unlock the session, or however else it
      sees fit.

      On binding this interface the compositor will immediately send the
      first configure event. After making the ack_configure request in
      response to this event the client should attach and commit the first
      buffer. Committing the surface before acking the first configure is a
      protocol error. Committing the surface with a null buffer at any time
      is a protocol error.

      The compositor is free to handle keyboard/pointer focus for lock
      surfaces however it chooses. A reasonable way to do this would be to
      give the first lock surface created keyboard focus and change keyboard
      focus if the user clicks on other surfaces.
"))

(DEFMETHOD DISPATCH-BIND ((GLOBAL GLOBAL) CLIENT DATA VERSION ID)
  "Default bind implementation for the ext_session_lock_surface_v1 global object.
This can be overriden by inheritance in case if custom behaviour is required."
  (CL-WL::DEBUG-LOG! "Binding ~a~%" "ext_session_lock_surface_v1")
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
                     "ext_session_lock_surface_v1")
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

