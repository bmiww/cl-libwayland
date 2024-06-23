(DEFPACKAGE :WL_DISPLAY
  (:USE :CL :CFFI :CL-WL.FFI)
  (:NICKNAMES :WL-DISPLAY)
  (:EXPORT DISPATCH
           GLOBAL
           DISPATCH-BIND
           SYNC
           GET-REGISTRY
           SEND-ERROR
           SEND-DELETE-ID))

(IN-PACKAGE :WL_DISPLAY)

(DEFCSTRUCT INTERFACE (NAME :STRING) (VERSION :INT) (METHOD_COUNT :INT)
 (METHODS (:POINTER (:STRUCT CL-WL.FFI:WL_MESSAGE))) (EVENT_COUNT :INT)
 (EVENTS (:POINTER (:STRUCT CL-WL.FFI:WL_MESSAGE))))

(DEFVAR *INTERFACE* NIL)

(DEFPACKAGE :WL_REGISTRY
  (:USE :CL :CFFI :CL-WL.FFI)
  (:NICKNAMES :WL-REGISTRY)
  (:EXPORT DISPATCH
           GLOBAL
           DISPATCH-BIND
           BIND
           SEND-GLOBAL
           SEND-GLOBAL-REMOVE))

(IN-PACKAGE :WL_REGISTRY)

(DEFCSTRUCT INTERFACE (NAME :STRING) (VERSION :INT) (METHOD_COUNT :INT)
 (METHODS (:POINTER (:STRUCT CL-WL.FFI:WL_MESSAGE))) (EVENT_COUNT :INT)
 (EVENTS (:POINTER (:STRUCT CL-WL.FFI:WL_MESSAGE))))

(DEFVAR *INTERFACE* NIL)

(DEFPACKAGE :WL_CALLBACK
  (:USE :CL :CFFI :CL-WL.FFI)
  (:NICKNAMES :WL-CALLBACK)
  (:EXPORT DISPATCH GLOBAL DISPATCH-BIND SEND-DONE))

(IN-PACKAGE :WL_CALLBACK)

(DEFCSTRUCT INTERFACE (NAME :STRING) (VERSION :INT) (METHOD_COUNT :INT)
 (METHODS (:POINTER (:STRUCT CL-WL.FFI:WL_MESSAGE))) (EVENT_COUNT :INT)
 (EVENTS (:POINTER (:STRUCT CL-WL.FFI:WL_MESSAGE))))

(DEFVAR *INTERFACE* NIL)

(DEFPACKAGE :WL_COMPOSITOR
  (:USE :CL :CFFI :CL-WL.FFI)
  (:NICKNAMES :WL-COMPOSITOR)
  (:EXPORT DISPATCH
           GLOBAL
           DISPATCH-BIND
           CREATE-SURFACE
           CREATE-REGION))

(IN-PACKAGE :WL_COMPOSITOR)

(DEFCSTRUCT INTERFACE (NAME :STRING) (VERSION :INT) (METHOD_COUNT :INT)
 (METHODS (:POINTER (:STRUCT CL-WL.FFI:WL_MESSAGE))) (EVENT_COUNT :INT)
 (EVENTS (:POINTER (:STRUCT CL-WL.FFI:WL_MESSAGE))))

(DEFVAR *INTERFACE* NIL)

(DEFPACKAGE :WL_SHM_POOL
  (:USE :CL :CFFI :CL-WL.FFI)
  (:NICKNAMES :WL-SHM-POOL)
  (:EXPORT DISPATCH
           GLOBAL
           DISPATCH-BIND
           CREATE-BUFFER
           DESTROY
           RESIZE))

(IN-PACKAGE :WL_SHM_POOL)

(DEFCSTRUCT INTERFACE (NAME :STRING) (VERSION :INT) (METHOD_COUNT :INT)
 (METHODS (:POINTER (:STRUCT CL-WL.FFI:WL_MESSAGE))) (EVENT_COUNT :INT)
 (EVENTS (:POINTER (:STRUCT CL-WL.FFI:WL_MESSAGE))))

(DEFVAR *INTERFACE* NIL)

(DEFPACKAGE :WL_SHM
  (:USE :CL :CFFI :CL-WL.FFI)
  (:NICKNAMES :WL-SHM)
  (:EXPORT DISPATCH
           GLOBAL
           DISPATCH-BIND
           CREATE-POOL
           SEND-FORMAT))

(IN-PACKAGE :WL_SHM)

(DEFCSTRUCT INTERFACE (NAME :STRING) (VERSION :INT) (METHOD_COUNT :INT)
 (METHODS (:POINTER (:STRUCT CL-WL.FFI:WL_MESSAGE))) (EVENT_COUNT :INT)
 (EVENTS (:POINTER (:STRUCT CL-WL.FFI:WL_MESSAGE))))

(DEFVAR *INTERFACE* NIL)

(DEFPACKAGE :WL_BUFFER
  (:USE :CL :CFFI :CL-WL.FFI)
  (:NICKNAMES :WL-BUFFER)
  (:EXPORT DISPATCH
           GLOBAL
           DISPATCH-BIND
           DESTROY
           SEND-RELEASE))

(IN-PACKAGE :WL_BUFFER)

(DEFCSTRUCT INTERFACE (NAME :STRING) (VERSION :INT) (METHOD_COUNT :INT)
 (METHODS (:POINTER (:STRUCT CL-WL.FFI:WL_MESSAGE))) (EVENT_COUNT :INT)
 (EVENTS (:POINTER (:STRUCT CL-WL.FFI:WL_MESSAGE))))

(DEFVAR *INTERFACE* NIL)

(DEFPACKAGE :WL_DATA_OFFER
  (:USE :CL :CFFI :CL-WL.FFI)
  (:NICKNAMES :WL-DATA-OFFER)
  (:EXPORT DISPATCH
           GLOBAL
           DISPATCH-BIND
           ACCEPT
           RECEIVE
           DESTROY
           FINISH
           SET-ACTIONS
           SEND-OFFER
           SEND-SOURCE-ACTIONS
           SEND-ACTION))

(IN-PACKAGE :WL_DATA_OFFER)

(DEFCSTRUCT INTERFACE (NAME :STRING) (VERSION :INT) (METHOD_COUNT :INT)
 (METHODS (:POINTER (:STRUCT CL-WL.FFI:WL_MESSAGE))) (EVENT_COUNT :INT)
 (EVENTS (:POINTER (:STRUCT CL-WL.FFI:WL_MESSAGE))))

(DEFVAR *INTERFACE* NIL)

(DEFPACKAGE :WL_DATA_SOURCE
  (:USE :CL :CFFI :CL-WL.FFI)
  (:NICKNAMES :WL-DATA-SOURCE)
  (:EXPORT DISPATCH
           GLOBAL
           DISPATCH-BIND
           OFFER
           DESTROY
           SET-ACTIONS
           SEND-TARGET
           SEND-SEND
           SEND-CANCELLED
           SEND-DND-DROP-PERFORMED
           SEND-DND-FINISHED
           SEND-ACTION))

(IN-PACKAGE :WL_DATA_SOURCE)

(DEFCSTRUCT INTERFACE (NAME :STRING) (VERSION :INT) (METHOD_COUNT :INT)
 (METHODS (:POINTER (:STRUCT CL-WL.FFI:WL_MESSAGE))) (EVENT_COUNT :INT)
 (EVENTS (:POINTER (:STRUCT CL-WL.FFI:WL_MESSAGE))))

(DEFVAR *INTERFACE* NIL)

(DEFPACKAGE :WL_DATA_DEVICE
  (:USE :CL :CFFI :CL-WL.FFI)
  (:NICKNAMES :WL-DATA-DEVICE)
  (:EXPORT DISPATCH
           GLOBAL
           DISPATCH-BIND
           START-DRAG
           SET-SELECTION
           RELEASE
           SEND-DATA-OFFER
           SEND-ENTER
           SEND-LEAVE
           SEND-MOTION
           SEND-DROP
           SEND-SELECTION))

(IN-PACKAGE :WL_DATA_DEVICE)

(DEFCSTRUCT INTERFACE (NAME :STRING) (VERSION :INT) (METHOD_COUNT :INT)
 (METHODS (:POINTER (:STRUCT CL-WL.FFI:WL_MESSAGE))) (EVENT_COUNT :INT)
 (EVENTS (:POINTER (:STRUCT CL-WL.FFI:WL_MESSAGE))))

(DEFVAR *INTERFACE* NIL)

(DEFPACKAGE :WL_DATA_DEVICE_MANAGER
  (:USE :CL :CFFI :CL-WL.FFI)
  (:NICKNAMES :WL-DATA-DEVICE-MANAGER)
  (:EXPORT DISPATCH
           GLOBAL
           DISPATCH-BIND
           CREATE-DATA-SOURCE
           GET-DATA-DEVICE))

(IN-PACKAGE :WL_DATA_DEVICE_MANAGER)

(DEFCSTRUCT INTERFACE (NAME :STRING) (VERSION :INT) (METHOD_COUNT :INT)
 (METHODS (:POINTER (:STRUCT CL-WL.FFI:WL_MESSAGE))) (EVENT_COUNT :INT)
 (EVENTS (:POINTER (:STRUCT CL-WL.FFI:WL_MESSAGE))))

(DEFVAR *INTERFACE* NIL)

(DEFPACKAGE :WL_SHELL
  (:USE :CL :CFFI :CL-WL.FFI)
  (:NICKNAMES :WL-SHELL)
  (:EXPORT DISPATCH GLOBAL DISPATCH-BIND GET-SHELL-SURFACE))

(IN-PACKAGE :WL_SHELL)

(DEFCSTRUCT INTERFACE (NAME :STRING) (VERSION :INT) (METHOD_COUNT :INT)
 (METHODS (:POINTER (:STRUCT CL-WL.FFI:WL_MESSAGE))) (EVENT_COUNT :INT)
 (EVENTS (:POINTER (:STRUCT CL-WL.FFI:WL_MESSAGE))))

(DEFVAR *INTERFACE* NIL)

(DEFPACKAGE :WL_SHELL_SURFACE
  (:USE :CL :CFFI :CL-WL.FFI)
  (:NICKNAMES :WL-SHELL-SURFACE)
  (:EXPORT DISPATCH
           GLOBAL
           DISPATCH-BIND
           PONG
           MOVE
           RESIZE
           SET-TOPLEVEL
           SET-TRANSIENT
           SET-FULLSCREEN
           SET-POPUP
           SET-MAXIMIZED
           SET-TITLE
           SET-CLASS
           SEND-PING
           SEND-CONFIGURE
           SEND-POPUP-DONE))

(IN-PACKAGE :WL_SHELL_SURFACE)

(DEFCSTRUCT INTERFACE (NAME :STRING) (VERSION :INT) (METHOD_COUNT :INT)
 (METHODS (:POINTER (:STRUCT CL-WL.FFI:WL_MESSAGE))) (EVENT_COUNT :INT)
 (EVENTS (:POINTER (:STRUCT CL-WL.FFI:WL_MESSAGE))))

(DEFVAR *INTERFACE* NIL)

(DEFPACKAGE :WL_SURFACE
  (:USE :CL :CFFI :CL-WL.FFI)
  (:NICKNAMES :WL-SURFACE)
  (:EXPORT DISPATCH
           GLOBAL
           DISPATCH-BIND
           DESTROY
           ATTACH
           DAMAGE
           FRAME
           SET-OPAQUE-REGION
           SET-INPUT-REGION
           COMMIT
           SET-BUFFER-TRANSFORM
           SET-BUFFER-SCALE
           DAMAGE-BUFFER
           OFFSET
           SEND-ENTER
           SEND-LEAVE
           SEND-PREFERRED-BUFFER-SCALE
           SEND-PREFERRED-BUFFER-TRANSFORM))

(IN-PACKAGE :WL_SURFACE)

(DEFCSTRUCT INTERFACE (NAME :STRING) (VERSION :INT) (METHOD_COUNT :INT)
 (METHODS (:POINTER (:STRUCT CL-WL.FFI:WL_MESSAGE))) (EVENT_COUNT :INT)
 (EVENTS (:POINTER (:STRUCT CL-WL.FFI:WL_MESSAGE))))

(DEFVAR *INTERFACE* NIL)

(DEFPACKAGE :WL_SEAT
  (:USE :CL :CFFI :CL-WL.FFI)
  (:NICKNAMES :WL-SEAT)
  (:EXPORT DISPATCH
           GLOBAL
           DISPATCH-BIND
           GET-POINTER
           GET-KEYBOARD
           GET-TOUCH
           RELEASE
           SEND-CAPABILITIES
           SEND-NAME))

(IN-PACKAGE :WL_SEAT)

(DEFCSTRUCT INTERFACE (NAME :STRING) (VERSION :INT) (METHOD_COUNT :INT)
 (METHODS (:POINTER (:STRUCT CL-WL.FFI:WL_MESSAGE))) (EVENT_COUNT :INT)
 (EVENTS (:POINTER (:STRUCT CL-WL.FFI:WL_MESSAGE))))

(DEFVAR *INTERFACE* NIL)

(DEFPACKAGE :WL_POINTER
  (:USE :CL :CFFI :CL-WL.FFI)
  (:NICKNAMES :WL-POINTER)
  (:EXPORT DISPATCH
           GLOBAL
           DISPATCH-BIND
           SET-CURSOR
           RELEASE
           SEND-ENTER
           SEND-LEAVE
           SEND-MOTION
           SEND-BUTTON
           SEND-AXIS
           SEND-FRAME
           SEND-AXIS-SOURCE
           SEND-AXIS-STOP
           SEND-AXIS-DISCRETE
           SEND-AXIS-VALUE120
           SEND-AXIS-RELATIVE-DIRECTION))

(IN-PACKAGE :WL_POINTER)

(DEFCSTRUCT INTERFACE (NAME :STRING) (VERSION :INT) (METHOD_COUNT :INT)
 (METHODS (:POINTER (:STRUCT CL-WL.FFI:WL_MESSAGE))) (EVENT_COUNT :INT)
 (EVENTS (:POINTER (:STRUCT CL-WL.FFI:WL_MESSAGE))))

(DEFVAR *INTERFACE* NIL)

(DEFPACKAGE :WL_KEYBOARD
  (:USE :CL :CFFI :CL-WL.FFI)
  (:NICKNAMES :WL-KEYBOARD)
  (:EXPORT DISPATCH
           GLOBAL
           DISPATCH-BIND
           RELEASE
           SEND-KEYMAP
           SEND-ENTER
           SEND-LEAVE
           SEND-KEY
           SEND-MODIFIERS
           SEND-REPEAT-INFO))

(IN-PACKAGE :WL_KEYBOARD)

(DEFCSTRUCT INTERFACE (NAME :STRING) (VERSION :INT) (METHOD_COUNT :INT)
 (METHODS (:POINTER (:STRUCT CL-WL.FFI:WL_MESSAGE))) (EVENT_COUNT :INT)
 (EVENTS (:POINTER (:STRUCT CL-WL.FFI:WL_MESSAGE))))

(DEFVAR *INTERFACE* NIL)

(DEFPACKAGE :WL_TOUCH
  (:USE :CL :CFFI :CL-WL.FFI)
  (:NICKNAMES :WL-TOUCH)
  (:EXPORT DISPATCH
           GLOBAL
           DISPATCH-BIND
           RELEASE
           SEND-DOWN
           SEND-UP
           SEND-MOTION
           SEND-FRAME
           SEND-CANCEL
           SEND-SHAPE
           SEND-ORIENTATION))

(IN-PACKAGE :WL_TOUCH)

(DEFCSTRUCT INTERFACE (NAME :STRING) (VERSION :INT) (METHOD_COUNT :INT)
 (METHODS (:POINTER (:STRUCT CL-WL.FFI:WL_MESSAGE))) (EVENT_COUNT :INT)
 (EVENTS (:POINTER (:STRUCT CL-WL.FFI:WL_MESSAGE))))

(DEFVAR *INTERFACE* NIL)

(DEFPACKAGE :WL_OUTPUT
  (:USE :CL :CFFI :CL-WL.FFI)
  (:NICKNAMES :WL-OUTPUT)
  (:EXPORT DISPATCH
           GLOBAL
           DISPATCH-BIND
           RELEASE
           SEND-GEOMETRY
           SEND-MODE
           SEND-DONE
           SEND-SCALE
           SEND-NAME
           SEND-DESCRIPTION))

(IN-PACKAGE :WL_OUTPUT)

(DEFCSTRUCT INTERFACE (NAME :STRING) (VERSION :INT) (METHOD_COUNT :INT)
 (METHODS (:POINTER (:STRUCT CL-WL.FFI:WL_MESSAGE))) (EVENT_COUNT :INT)
 (EVENTS (:POINTER (:STRUCT CL-WL.FFI:WL_MESSAGE))))

(DEFVAR *INTERFACE* NIL)

(DEFPACKAGE :WL_REGION
  (:USE :CL :CFFI :CL-WL.FFI)
  (:NICKNAMES :WL-REGION)
  (:EXPORT DISPATCH
           GLOBAL
           DISPATCH-BIND
           DESTROY
           ADD
           SUBTRACT))

(IN-PACKAGE :WL_REGION)

(DEFCSTRUCT INTERFACE (NAME :STRING) (VERSION :INT) (METHOD_COUNT :INT)
 (METHODS (:POINTER (:STRUCT CL-WL.FFI:WL_MESSAGE))) (EVENT_COUNT :INT)
 (EVENTS (:POINTER (:STRUCT CL-WL.FFI:WL_MESSAGE))))

(DEFVAR *INTERFACE* NIL)

(DEFPACKAGE :WL_SUBCOMPOSITOR
  (:USE :CL :CFFI :CL-WL.FFI)
  (:NICKNAMES :WL-SUBCOMPOSITOR)
  (:EXPORT DISPATCH
           GLOBAL
           DISPATCH-BIND
           DESTROY
           GET-SUBSURFACE))

(IN-PACKAGE :WL_SUBCOMPOSITOR)

(DEFCSTRUCT INTERFACE (NAME :STRING) (VERSION :INT) (METHOD_COUNT :INT)
 (METHODS (:POINTER (:STRUCT CL-WL.FFI:WL_MESSAGE))) (EVENT_COUNT :INT)
 (EVENTS (:POINTER (:STRUCT CL-WL.FFI:WL_MESSAGE))))

(DEFVAR *INTERFACE* NIL)

(DEFPACKAGE :WL_SUBSURFACE
  (:USE :CL :CFFI :CL-WL.FFI)
  (:NICKNAMES :WL-SUBSURFACE)
  (:EXPORT DISPATCH
           GLOBAL
           DISPATCH-BIND
           DESTROY
           SET-POSITION
           PLACE-ABOVE
           PLACE-BELOW
           SET-SYNC
           SET-DESYNC))

(IN-PACKAGE :WL_SUBSURFACE)

(DEFCSTRUCT INTERFACE (NAME :STRING) (VERSION :INT) (METHOD_COUNT :INT)
 (METHODS (:POINTER (:STRUCT CL-WL.FFI:WL_MESSAGE))) (EVENT_COUNT :INT)
 (EVENTS (:POINTER (:STRUCT CL-WL.FFI:WL_MESSAGE))))

(DEFVAR *INTERFACE* NIL)

(IN-PACKAGE :WL_DISPLAY)

(DEFCLASS DISPATCH (CL-WL:OBJECT)
          ((WL_DISPLAY-ID :INITFORM NIL :ACCESSOR
            WL_DISPLAY-ID)
           (WL_DISPLAY-PTR :INITFORM NIL :ACCESSOR
            WL_DISPLAY-PTR))
          (:DEFAULT-INITARGS :VERSION 1)
          (:DOCUMENTATION "core global object

The core global object.  This is a special singleton object.  It
      is used for internal Wayland protocol features.
"))

(DEFGENERIC SYNC
    (RESOURCE CALLBACK))

(DEFGENERIC GET-REGISTRY
    (RESOURCE REGISTRY))

(DEFMETHOD CL-WL:DESTROY ((DISPATCH DISPATCH))
  (CL-WL::DEBUG-LOG! "Destroying dispatch object: ~a~%" "wl_display")
  (WHEN (CL-WL::DESTROY-CALLBACK DISPATCH)
    (LOOP FOR CALLBACK IN (CL-WL::DESTROY-CALLBACK DISPATCH)
          DO (FUNCALL CALLBACK DISPATCH)))
  (LET ((RESOURCE-PTR (WL_DISPLAY-PTR DISPATCH)))
    (CL-WL::REMOVE-RESOURCE (POINTER-ADDRESS RESOURCE-PTR)))
  (CL-WL::DN-IF DISPATCH))

(PUSHNEW
 (LIST "wl_display" (LIST "wl_callback" "wl_registry")
       (LAMBDA ()
         (SETF *INTERFACE* (FOREIGN-ALLOC '(:STRUCT INTERFACE)))
         (LET ((REQUESTS-PTR
                (LET ((MESSAGES
                       (CFFI:FOREIGN-ALLOC '(:STRUCT CL-WL.FFI:WL_MESSAGE)
                                           :COUNT 2)))
                  (LET ((INTERFACE-ARRAY
                         (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID))
                                             :COUNT 1))
                        (MSG-PTR
                         (MEM-APTR MESSAGES '(:STRUCT CL-WL.FFI:WL_MESSAGE) 0)))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 0)
                            WL_CALLBACK::*INTERFACE*)
                    (WITH-FOREIGN-SLOTS
                     ((NAME SIGNATURE TYPES) MSG-PTR
                      (:STRUCT CL-WL.FFI:WL_MESSAGE))
                     (SETF NAME (FOREIGN-STRING-ALLOC "sync")
                           SIGNATURE "n"
                           TYPES INTERFACE-ARRAY)))
                  (LET ((INTERFACE-ARRAY
                         (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID))
                                             :COUNT 1))
                        (MSG-PTR
                         (MEM-APTR MESSAGES '(:STRUCT CL-WL.FFI:WL_MESSAGE) 1)))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 0)
                            WL_REGISTRY::*INTERFACE*)
                    (WITH-FOREIGN-SLOTS
                     ((NAME SIGNATURE TYPES) MSG-PTR
                      (:STRUCT CL-WL.FFI:WL_MESSAGE))
                     (SETF NAME (FOREIGN-STRING-ALLOC "get_registry")
                           SIGNATURE "n"
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
                     (SETF NAME (FOREIGN-STRING-ALLOC "error")
                           SIGNATURE "ous"
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
                     (SETF NAME (FOREIGN-STRING-ALLOC "delete_id")
                           SIGNATURE "u"
                           TYPES INTERFACE-ARRAY)))
                  MESSAGES)))
           (WITH-FOREIGN-SLOTS
            ((NAME VERSION METHOD_COUNT METHODS EVENT_COUNT EVENTS) *INTERFACE*
             (:STRUCT INTERFACE))
            (SETF NAME (FOREIGN-STRING-ALLOC "wl_display")
                  VERSION 1
                  METHOD_COUNT 2
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
      (CL-WL::DEBUG-LOG! "Dispatching ~a:~a~%" "wl_display" "sync")
      (FUNCALL 'SYNC RESOURCE
               (VALUES
                (FOREIGN-SLOT-VALUE
                 (MEM-APTR ARGS '(:UNION CL-WL.FFI:WL_ARGUMENT) 0)
                 '(:UNION CL-WL.FFI:WL_ARGUMENT) 'WL-FFI::N))))
     (1
      (CL-WL::DEBUG-LOG! "Dispatching ~a:~a~%" "wl_display" "get-registry")
      (FUNCALL 'GET-REGISTRY RESOURCE
               (VALUES
                (FOREIGN-SLOT-VALUE
                 (MEM-APTR ARGS '(:UNION CL-WL.FFI:WL_ARGUMENT) 0)
                 '(:UNION CL-WL.FFI:WL_ARGUMENT) 'WL-FFI::N))))))
 0)

(DEFVAR *DISPATCHER* (CALLBACK DISPATCHER-FFI))

(DEFMETHOD SHARED-INITIALIZE :AFTER ((INSTANCE DISPATCH) SLOT-NAMES &KEY ID)
  (DECLARE (IGNORE SLOT-NAMES))
  (UNLESS (WL_DISPLAY-ID INSTANCE)
    (LET* ((RESOURCE
            (CL-WL::CREATE-RESOURCE (CL-WL:PTR (CL-WL:CLIENT INSTANCE))
                                    *INTERFACE* (VERSION INSTANCE)
                                    (OR ID 0))))
      (SETF (CL-WL::TRANSIENT-ID INSTANCE)
              (SETF (WL_DISPLAY-ID INSTANCE)
                      (OR ID (CL-WL.FFI:RESOURCE-GET-ID RESOURCE))))
      (SETF (WL_DISPLAY-PTR INSTANCE) RESOURCE)
      (SETF (GETHASH (POINTER-ADDRESS RESOURCE) CL-WL::*RESOURCE-TRACKER*)
              INSTANCE)
      (RESOURCE-SET-DISPATCHER RESOURCE *DISPATCHER* (NULL-POINTER)
       (NULL-POINTER) (NULL-POINTER)))))

(DEFMETHOD SEND-ERROR
           ((DISPATCH DISPATCH) OBJECT_ID CODE
            MESSAGE)
  (CL-WL::DEBUG-LOG! "Event: ~a~%" "error")
  (LET ((ARG-LIST (FOREIGN-ALLOC '(:UNION WL_ARGUMENT) :COUNT 3)))
    (SETF (FOREIGN-SLOT-VALUE
           (MEM-AREF ARG-LIST '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 0)
           '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 'WL-FFI::O)
            (ERROR
             "Protocol did not specify object type. For example see wl_display error event. This is unimplemented"))
    (SETF (FOREIGN-SLOT-VALUE
           (MEM-AREF ARG-LIST '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 1)
           '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 'WL-FFI::U)
            CODE)
    (SETF (FOREIGN-SLOT-VALUE
           (MEM-AREF ARG-LIST '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 2)
           '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 'WL-FFI::S)
            MESSAGE)
    (RESOURCE-POST-EVENT-ARRAY (WL_DISPLAY-PTR DISPATCH) 0
     ARG-LIST)))

(DEFMETHOD SEND-DELETE-ID ((DISPATCH DISPATCH) ID)
  (CL-WL::DEBUG-LOG! "Event: ~a~%" "delete_id")
  (LET ((ARG-LIST (FOREIGN-ALLOC '(:UNION WL_ARGUMENT) :COUNT 1)))
    (SETF (FOREIGN-SLOT-VALUE
           (MEM-AREF ARG-LIST '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 0)
           '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 'WL-FFI::U)
            ID)
    (RESOURCE-POST-EVENT-ARRAY (WL_DISPLAY-PTR DISPATCH) 1
     ARG-LIST)))

(DEFUN ERROR-FROM-VALUE (NUMBER)
  (LOOP FOR ENTRY IN '((0 :INVALID-OBJECT)
                       (1 :INVALID-METHOD)
                       (2 :NO-MEMORY)
                       (3 :IMPLEMENTATION))
        FOR VALUE = (CAR ENTRY)
        FOR KEYWORD = (CADR ENTRY)
        WHEN (EQ NUMBER VALUE) RETURN KEYWORD
        FINALLY (ERROR (FORMAT NIL "Unknown enum value: ~a" NUMBER))))

(DEFUN ERROR-TO-VALUE (KEY)
  (LOOP FOR ENTRY IN '((0 :INVALID-OBJECT)
                       (1 :INVALID-METHOD)
                       (2 :NO-MEMORY)
                       (3 :IMPLEMENTATION))
        FOR VALUE = (CAR ENTRY)
        FOR KEYWORD = (CADR ENTRY)
        WHEN (EQ KEY KEYWORD) RETURN VALUE
        FINALLY (ERROR (FORMAT NIL "Unknown enum keyword: ~a" KEY))))

(DEFCLASS GLOBAL (CL-WL:GLOBAL) NIL
          (:DEFAULT-INITARGS :VERSION 1 :DISPATCH-IMPL 'DISPATCH)
          (:DOCUMENTATION "core global object

The core global object.  This is a special singleton object.  It
      is used for internal Wayland protocol features.
"))

(DEFMETHOD DISPATCH-BIND ((GLOBAL GLOBAL) CLIENT DATA VERSION ID)
  "Default bind implementation for the wl_display global object.
This can be overriden by inheritance in case if custom behaviour is required."
  (CL-WL::DEBUG-LOG! "Binding ~a~%" "wl_display")
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
  (CL-WL::DEBUG-LOG! "Initializing global object: ~a~%" "wl_display")
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

(IN-PACKAGE :WL_REGISTRY)

(DEFCLASS DISPATCH (CL-WL:OBJECT)
          ((WL_REGISTRY-ID :INITFORM NIL :ACCESSOR
            WL_REGISTRY-ID)
           (WL_REGISTRY-PTR :INITFORM NIL :ACCESSOR
            WL_REGISTRY-PTR))
          (:DEFAULT-INITARGS :VERSION 1)
          (:DOCUMENTATION "global registry object

The singleton global registry object.  The server has a number of
      global objects that are available to all clients.  These objects
      typically represent an actual object in the server (for example,
      an input device) or they are singleton objects that provide
      extension functionality.

      When a client creates a registry object, the registry object
      will emit a global event for each global currently in the
      registry.  Globals come and go as a result of device or
      monitor hotplugs, reconfiguration or other events, and the
      registry will send out global and global_remove events to
      keep the client up to date with the changes.  To mark the end
      of the initial burst of events, the client can use the
      wl_display.sync request immediately after calling
      wl_display.get_registry.

      A client can bind to a global object by using the bind
      request.  This creates a client-side handle that lets the object
      emit events to the client and lets the client invoke requests on
      the object.
"))

(DEFGENERIC BIND
    (RESOURCE NAME ID))

(DEFMETHOD CL-WL:DESTROY ((DISPATCH DISPATCH))
  (CL-WL::DEBUG-LOG! "Destroying dispatch object: ~a~%" "wl_registry")
  (WHEN (CL-WL::DESTROY-CALLBACK DISPATCH)
    (LOOP FOR CALLBACK IN (CL-WL::DESTROY-CALLBACK DISPATCH)
          DO (FUNCALL CALLBACK DISPATCH)))
  (LET ((RESOURCE-PTR (WL_REGISTRY-PTR DISPATCH)))
    (CL-WL::REMOVE-RESOURCE (POINTER-ADDRESS RESOURCE-PTR)))
  (CL-WL::DN-IF DISPATCH))

(PUSHNEW
 (LIST "wl_registry" (LIST)
       (LAMBDA ()
         (SETF *INTERFACE* (FOREIGN-ALLOC '(:STRUCT INTERFACE)))
         (LET ((REQUESTS-PTR
                (LET ((MESSAGES
                       (CFFI:FOREIGN-ALLOC '(:STRUCT CL-WL.FFI:WL_MESSAGE)
                                           :COUNT 1)))
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
                     (SETF NAME (FOREIGN-STRING-ALLOC "bind")
                           SIGNATURE "un"
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
                     (SETF NAME (FOREIGN-STRING-ALLOC "global")
                           SIGNATURE "usu"
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
                     (SETF NAME (FOREIGN-STRING-ALLOC "global_remove")
                           SIGNATURE "u"
                           TYPES INTERFACE-ARRAY)))
                  MESSAGES)))
           (WITH-FOREIGN-SLOTS
            ((NAME VERSION METHOD_COUNT METHODS EVENT_COUNT EVENTS) *INTERFACE*
             (:STRUCT INTERFACE))
            (SETF NAME (FOREIGN-STRING-ALLOC "wl_registry")
                  VERSION 1
                  METHOD_COUNT 1
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
      (CL-WL::DEBUG-LOG! "Dispatching ~a:~a~%" "wl_registry" "bind")
      (FUNCALL 'BIND RESOURCE
               (VALUES
                (FOREIGN-SLOT-VALUE
                 (MEM-APTR ARGS '(:UNION CL-WL.FFI:WL_ARGUMENT) 0)
                 '(:UNION CL-WL.FFI:WL_ARGUMENT) 'WL-FFI::U))
               (VALUES
                (FOREIGN-SLOT-VALUE
                 (MEM-APTR ARGS '(:UNION CL-WL.FFI:WL_ARGUMENT) 1)
                 '(:UNION CL-WL.FFI:WL_ARGUMENT) 'WL-FFI::N))))))
 0)

(DEFVAR *DISPATCHER* (CALLBACK DISPATCHER-FFI))

(DEFMETHOD SHARED-INITIALIZE :AFTER ((INSTANCE DISPATCH) SLOT-NAMES &KEY ID)
  (DECLARE (IGNORE SLOT-NAMES))
  (UNLESS (WL_REGISTRY-ID INSTANCE)
    (LET* ((RESOURCE
            (CL-WL::CREATE-RESOURCE (CL-WL:PTR (CL-WL:CLIENT INSTANCE))
                                    *INTERFACE* (VERSION INSTANCE)
                                    (OR ID 0))))
      (SETF (CL-WL::TRANSIENT-ID INSTANCE)
              (SETF (WL_REGISTRY-ID INSTANCE)
                      (OR ID (CL-WL.FFI:RESOURCE-GET-ID RESOURCE))))
      (SETF (WL_REGISTRY-PTR INSTANCE) RESOURCE)
      (SETF (GETHASH (POINTER-ADDRESS RESOURCE) CL-WL::*RESOURCE-TRACKER*)
              INSTANCE)
      (RESOURCE-SET-DISPATCHER RESOURCE *DISPATCHER* (NULL-POINTER)
       (NULL-POINTER) (NULL-POINTER)))))

(DEFMETHOD SEND-GLOBAL
           ((DISPATCH DISPATCH) NAME INTERFACE
            VERSION)
  (CL-WL::DEBUG-LOG! "Event: ~a~%" "global")
  (LET ((ARG-LIST (FOREIGN-ALLOC '(:UNION WL_ARGUMENT) :COUNT 3)))
    (SETF (FOREIGN-SLOT-VALUE
           (MEM-AREF ARG-LIST '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 0)
           '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 'WL-FFI::U)
            NAME)
    (SETF (FOREIGN-SLOT-VALUE
           (MEM-AREF ARG-LIST '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 1)
           '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 'WL-FFI::S)
            INTERFACE)
    (SETF (FOREIGN-SLOT-VALUE
           (MEM-AREF ARG-LIST '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 2)
           '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 'WL-FFI::U)
            VERSION)
    (RESOURCE-POST-EVENT-ARRAY (WL_REGISTRY-PTR DISPATCH) 0
     ARG-LIST)))

(DEFMETHOD SEND-GLOBAL-REMOVE
           ((DISPATCH DISPATCH) NAME)
  (CL-WL::DEBUG-LOG! "Event: ~a~%" "global_remove")
  (LET ((ARG-LIST (FOREIGN-ALLOC '(:UNION WL_ARGUMENT) :COUNT 1)))
    (SETF (FOREIGN-SLOT-VALUE
           (MEM-AREF ARG-LIST '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 0)
           '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 'WL-FFI::U)
            NAME)
    (RESOURCE-POST-EVENT-ARRAY (WL_REGISTRY-PTR DISPATCH) 1
     ARG-LIST)))

(DEFCLASS GLOBAL (CL-WL:GLOBAL) NIL
          (:DEFAULT-INITARGS :VERSION 1 :DISPATCH-IMPL 'DISPATCH)
          (:DOCUMENTATION "global registry object

The singleton global registry object.  The server has a number of
      global objects that are available to all clients.  These objects
      typically represent an actual object in the server (for example,
      an input device) or they are singleton objects that provide
      extension functionality.

      When a client creates a registry object, the registry object
      will emit a global event for each global currently in the
      registry.  Globals come and go as a result of device or
      monitor hotplugs, reconfiguration or other events, and the
      registry will send out global and global_remove events to
      keep the client up to date with the changes.  To mark the end
      of the initial burst of events, the client can use the
      wl_display.sync request immediately after calling
      wl_display.get_registry.

      A client can bind to a global object by using the bind
      request.  This creates a client-side handle that lets the object
      emit events to the client and lets the client invoke requests on
      the object.
"))

(DEFMETHOD DISPATCH-BIND ((GLOBAL GLOBAL) CLIENT DATA VERSION ID)
  "Default bind implementation for the wl_registry global object.
This can be overriden by inheritance in case if custom behaviour is required."
  (CL-WL::DEBUG-LOG! "Binding ~a~%" "wl_registry")
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
  (CL-WL::DEBUG-LOG! "Initializing global object: ~a~%" "wl_registry")
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

(IN-PACKAGE :WL_CALLBACK)

(DEFCLASS DISPATCH (CL-WL:OBJECT)
          ((WL_CALLBACK-ID :INITFORM NIL :ACCESSOR
            WL_CALLBACK-ID)
           (WL_CALLBACK-PTR :INITFORM NIL :ACCESSOR
            WL_CALLBACK-PTR))
          (:DEFAULT-INITARGS :VERSION 1)
          (:DOCUMENTATION "callback object

Clients can handle the 'done' event to get notified when
      the related request is done.

      Note, because wl_callback objects are created from multiple independent
      factory interfaces, the wl_callback interface is frozen at version 1.
"))

(DEFMETHOD CL-WL:DESTROY ((DISPATCH DISPATCH))
  (CL-WL::DEBUG-LOG! "Destroying dispatch object: ~a~%" "wl_callback")
  (WHEN (CL-WL::DESTROY-CALLBACK DISPATCH)
    (LOOP FOR CALLBACK IN (CL-WL::DESTROY-CALLBACK DISPATCH)
          DO (FUNCALL CALLBACK DISPATCH)))
  (LET ((RESOURCE-PTR (WL_CALLBACK-PTR DISPATCH)))
    (CL-WL::REMOVE-RESOURCE (POINTER-ADDRESS RESOURCE-PTR)))
  (CL-WL::DN-IF DISPATCH))

(PUSHNEW
 (LIST "wl_callback" (LIST)
       (LAMBDA ()
         (SETF *INTERFACE* (FOREIGN-ALLOC '(:STRUCT INTERFACE)))
         (LET ((REQUESTS-PTR
                (LET ((MESSAGES
                       (CFFI:FOREIGN-ALLOC '(:STRUCT CL-WL.FFI:WL_MESSAGE)
                                           :COUNT 0)))
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
                     (SETF NAME (FOREIGN-STRING-ALLOC "done")
                           SIGNATURE "u"
                           TYPES INTERFACE-ARRAY)))
                  MESSAGES)))
           (WITH-FOREIGN-SLOTS
            ((NAME VERSION METHOD_COUNT METHODS EVENT_COUNT EVENTS) *INTERFACE*
             (:STRUCT INTERFACE))
            (SETF NAME (FOREIGN-STRING-ALLOC "wl_callback")
                  VERSION 1
                  METHOD_COUNT 0
                  METHODS REQUESTS-PTR
                  EVENT_COUNT 1
                  EVENTS EVENTS-PTR)))))
 CL-WL::*INTERFACE-INIT-LIST* :TEST #'CL-WL::INTERFACE-EXISTS-TEST)

(DEFCALLBACK DISPATCHER-FFI :INT
 ((DATA :POINTER) (TARGET :POINTER) (OPCODE :UINT) (MESSAGE :POINTER)
  (ARGS :POINTER))
 (DECLARE (IGNORE DATA MESSAGE ARGS TARGET OPCODE))
 (ERROR
  (FORMAT NIL
          "A dispatcher without requests has been called for interface: ~a~%"
          "wl_callback"))
 0)

(DEFVAR *DISPATCHER* (CALLBACK DISPATCHER-FFI))

(DEFMETHOD SHARED-INITIALIZE :AFTER ((INSTANCE DISPATCH) SLOT-NAMES &KEY ID)
  (DECLARE (IGNORE SLOT-NAMES))
  (UNLESS (WL_CALLBACK-ID INSTANCE)
    (LET* ((RESOURCE
            (CL-WL::CREATE-RESOURCE (CL-WL:PTR (CL-WL:CLIENT INSTANCE))
                                    *INTERFACE* (VERSION INSTANCE)
                                    (OR ID 0))))
      (SETF (CL-WL::TRANSIENT-ID INSTANCE)
              (SETF (WL_CALLBACK-ID INSTANCE)
                      (OR ID (CL-WL.FFI:RESOURCE-GET-ID RESOURCE))))
      (SETF (WL_CALLBACK-PTR INSTANCE) RESOURCE)
      (SETF (GETHASH (POINTER-ADDRESS RESOURCE) CL-WL::*RESOURCE-TRACKER*)
              INSTANCE)
      (RESOURCE-SET-DISPATCHER RESOURCE *DISPATCHER* (NULL-POINTER)
       (NULL-POINTER) (NULL-POINTER)))))

(DEFMETHOD SEND-DONE
           ((DISPATCH DISPATCH) CALLBACK_DATA)
  (CL-WL::DEBUG-LOG! "Event: ~a~%" "done")
  (LET ((ARG-LIST (FOREIGN-ALLOC '(:UNION WL_ARGUMENT) :COUNT 1)))
    (SETF (FOREIGN-SLOT-VALUE
           (MEM-AREF ARG-LIST '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 0)
           '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 'WL-FFI::U)
            CALLBACK_DATA)
    (RESOURCE-POST-EVENT-ARRAY (WL_CALLBACK-PTR DISPATCH) 0
     ARG-LIST)))

(DEFCLASS GLOBAL (CL-WL:GLOBAL) NIL
          (:DEFAULT-INITARGS :VERSION 1 :DISPATCH-IMPL 'DISPATCH)
          (:DOCUMENTATION "callback object

Clients can handle the 'done' event to get notified when
      the related request is done.

      Note, because wl_callback objects are created from multiple independent
      factory interfaces, the wl_callback interface is frozen at version 1.
"))

(DEFMETHOD DISPATCH-BIND ((GLOBAL GLOBAL) CLIENT DATA VERSION ID)
  "Default bind implementation for the wl_callback global object.
This can be overriden by inheritance in case if custom behaviour is required."
  (CL-WL::DEBUG-LOG! "Binding ~a~%" "wl_callback")
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
  (CL-WL::DEBUG-LOG! "Initializing global object: ~a~%" "wl_callback")
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

(IN-PACKAGE :WL_COMPOSITOR)

(DEFCLASS DISPATCH (CL-WL:OBJECT)
          ((WL_COMPOSITOR-ID :INITFORM NIL :ACCESSOR
            WL_COMPOSITOR-ID)
           (WL_COMPOSITOR-PTR :INITFORM NIL :ACCESSOR
            WL_COMPOSITOR-PTR))
          (:DEFAULT-INITARGS :VERSION 6)
          (:DOCUMENTATION "the compositor singleton

A compositor.  This object is a singleton global.  The
      compositor is in charge of combining the contents of multiple
      surfaces into one displayable output.
"))

(DEFGENERIC CREATE-SURFACE
    (RESOURCE ID))

(DEFGENERIC CREATE-REGION
    (RESOURCE ID))

(DEFMETHOD CL-WL:DESTROY ((DISPATCH DISPATCH))
  (CL-WL::DEBUG-LOG! "Destroying dispatch object: ~a~%" "wl_compositor")
  (WHEN (CL-WL::DESTROY-CALLBACK DISPATCH)
    (LOOP FOR CALLBACK IN (CL-WL::DESTROY-CALLBACK DISPATCH)
          DO (FUNCALL CALLBACK DISPATCH)))
  (LET ((RESOURCE-PTR (WL_COMPOSITOR-PTR DISPATCH)))
    (CL-WL::REMOVE-RESOURCE (POINTER-ADDRESS RESOURCE-PTR)))
  (CL-WL::DN-IF DISPATCH))

(PUSHNEW
 (LIST "wl_compositor" (LIST "wl_surface" "wl_region")
       (LAMBDA ()
         (SETF *INTERFACE* (FOREIGN-ALLOC '(:STRUCT INTERFACE)))
         (LET ((REQUESTS-PTR
                (LET ((MESSAGES
                       (CFFI:FOREIGN-ALLOC '(:STRUCT CL-WL.FFI:WL_MESSAGE)
                                           :COUNT 2)))
                  (LET ((INTERFACE-ARRAY
                         (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID))
                                             :COUNT 1))
                        (MSG-PTR
                         (MEM-APTR MESSAGES '(:STRUCT CL-WL.FFI:WL_MESSAGE) 0)))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 0)
                            WL_SURFACE::*INTERFACE*)
                    (WITH-FOREIGN-SLOTS
                     ((NAME SIGNATURE TYPES) MSG-PTR
                      (:STRUCT CL-WL.FFI:WL_MESSAGE))
                     (SETF NAME (FOREIGN-STRING-ALLOC "create_surface")
                           SIGNATURE "n"
                           TYPES INTERFACE-ARRAY)))
                  (LET ((INTERFACE-ARRAY
                         (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID))
                                             :COUNT 1))
                        (MSG-PTR
                         (MEM-APTR MESSAGES '(:STRUCT CL-WL.FFI:WL_MESSAGE) 1)))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 0)
                            WL_REGION::*INTERFACE*)
                    (WITH-FOREIGN-SLOTS
                     ((NAME SIGNATURE TYPES) MSG-PTR
                      (:STRUCT CL-WL.FFI:WL_MESSAGE))
                     (SETF NAME (FOREIGN-STRING-ALLOC "create_region")
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
            (SETF NAME (FOREIGN-STRING-ALLOC "wl_compositor")
                  VERSION 6
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
      (CL-WL::DEBUG-LOG! "Dispatching ~a:~a~%" "wl_compositor"
                         "create-surface")
      (FUNCALL 'CREATE-SURFACE RESOURCE
               (VALUES
                (FOREIGN-SLOT-VALUE
                 (MEM-APTR ARGS '(:UNION CL-WL.FFI:WL_ARGUMENT) 0)
                 '(:UNION CL-WL.FFI:WL_ARGUMENT) 'WL-FFI::N))))
     (1
      (CL-WL::DEBUG-LOG! "Dispatching ~a:~a~%" "wl_compositor" "create-region")
      (FUNCALL 'CREATE-REGION RESOURCE
               (VALUES
                (FOREIGN-SLOT-VALUE
                 (MEM-APTR ARGS '(:UNION CL-WL.FFI:WL_ARGUMENT) 0)
                 '(:UNION CL-WL.FFI:WL_ARGUMENT) 'WL-FFI::N))))))
 0)

(DEFVAR *DISPATCHER* (CALLBACK DISPATCHER-FFI))

(DEFMETHOD SHARED-INITIALIZE :AFTER ((INSTANCE DISPATCH) SLOT-NAMES &KEY ID)
  (DECLARE (IGNORE SLOT-NAMES))
  (UNLESS (WL_COMPOSITOR-ID INSTANCE)
    (LET* ((RESOURCE
            (CL-WL::CREATE-RESOURCE (CL-WL:PTR (CL-WL:CLIENT INSTANCE))
                                    *INTERFACE* (VERSION INSTANCE)
                                    (OR ID 0))))
      (SETF (CL-WL::TRANSIENT-ID INSTANCE)
              (SETF (WL_COMPOSITOR-ID INSTANCE)
                      (OR ID (CL-WL.FFI:RESOURCE-GET-ID RESOURCE))))
      (SETF (WL_COMPOSITOR-PTR INSTANCE) RESOURCE)
      (SETF (GETHASH (POINTER-ADDRESS RESOURCE) CL-WL::*RESOURCE-TRACKER*)
              INSTANCE)
      (RESOURCE-SET-DISPATCHER RESOURCE *DISPATCHER* (NULL-POINTER)
       (NULL-POINTER) (NULL-POINTER)))))

(DEFCLASS GLOBAL (CL-WL:GLOBAL) NIL
          (:DEFAULT-INITARGS :VERSION 6 :DISPATCH-IMPL 'DISPATCH)
          (:DOCUMENTATION "the compositor singleton

A compositor.  This object is a singleton global.  The
      compositor is in charge of combining the contents of multiple
      surfaces into one displayable output.
"))

(DEFMETHOD DISPATCH-BIND ((GLOBAL GLOBAL) CLIENT DATA VERSION ID)
  "Default bind implementation for the wl_compositor global object.
This can be overriden by inheritance in case if custom behaviour is required."
  (CL-WL::DEBUG-LOG! "Binding ~a~%" "wl_compositor")
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
  (CL-WL::DEBUG-LOG! "Initializing global object: ~a~%" "wl_compositor")
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

(IN-PACKAGE :WL_SHM_POOL)

(DEFCLASS DISPATCH (CL-WL:OBJECT)
          ((WL_SHM_POOL-ID :INITFORM NIL :ACCESSOR
            WL_SHM_POOL-ID)
           (WL_SHM_POOL-PTR :INITFORM NIL :ACCESSOR
            WL_SHM_POOL-PTR))
          (:DEFAULT-INITARGS :VERSION 1)
          (:DOCUMENTATION "a shared memory pool

The wl_shm_pool object encapsulates a piece of memory shared
      between the compositor and client.  Through the wl_shm_pool
      object, the client can allocate shared memory wl_buffer objects.
      All objects created through the same pool share the same
      underlying mapped memory. Reusing the mapped memory avoids the
      setup/teardown overhead and is useful when interactively resizing
      a surface or for many small buffers.
"))

(DEFGENERIC CREATE-BUFFER
    (RESOURCE ID OFFSET WIDTH
     HEIGHT STRIDE FORMAT))

(DEFGENERIC DESTROY
    (RESOURCE))

(DEFGENERIC RESIZE
    (RESOURCE SIZE))

(DEFMETHOD CL-WL:DESTROY ((DISPATCH DISPATCH))
  (CL-WL::DEBUG-LOG! "Destroying dispatch object: ~a~%" "wl_shm_pool")
  (WHEN (CL-WL::DESTROY-CALLBACK DISPATCH)
    (LOOP FOR CALLBACK IN (CL-WL::DESTROY-CALLBACK DISPATCH)
          DO (FUNCALL CALLBACK DISPATCH)))
  (LET ((RESOURCE-PTR (WL_SHM_POOL-PTR DISPATCH)))
    (CL-WL::REMOVE-RESOURCE (POINTER-ADDRESS RESOURCE-PTR)))
  (CL-WL::DN-IF DISPATCH))

(DEFMETHOD DESTROY ((DISPATCH DISPATCH)) (CL-WL:DESTROY DISPATCH))

(DEFMETHOD DESTROY (EMPTY) NIL)

(PUSHNEW
 (LIST "wl_shm_pool" (LIST "wl_buffer")
       (LAMBDA ()
         (SETF *INTERFACE* (FOREIGN-ALLOC '(:STRUCT INTERFACE)))
         (LET ((REQUESTS-PTR
                (LET ((MESSAGES
                       (CFFI:FOREIGN-ALLOC '(:STRUCT CL-WL.FFI:WL_MESSAGE)
                                           :COUNT 3)))
                  (LET ((INTERFACE-ARRAY
                         (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID))
                                             :COUNT 6))
                        (MSG-PTR
                         (MEM-APTR MESSAGES '(:STRUCT CL-WL.FFI:WL_MESSAGE) 0)))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 0)
                            WL_BUFFER::*INTERFACE*)
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 1) (NULL-POINTER))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 2) (NULL-POINTER))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 3) (NULL-POINTER))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 4) (NULL-POINTER))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 5) (NULL-POINTER))
                    (WITH-FOREIGN-SLOTS
                     ((NAME SIGNATURE TYPES) MSG-PTR
                      (:STRUCT CL-WL.FFI:WL_MESSAGE))
                     (SETF NAME (FOREIGN-STRING-ALLOC "create_buffer")
                           SIGNATURE "niiiiu"
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
                  (LET ((INTERFACE-ARRAY
                         (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID))
                                             :COUNT 1))
                        (MSG-PTR
                         (MEM-APTR MESSAGES '(:STRUCT CL-WL.FFI:WL_MESSAGE) 2)))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 0) (NULL-POINTER))
                    (WITH-FOREIGN-SLOTS
                     ((NAME SIGNATURE TYPES) MSG-PTR
                      (:STRUCT CL-WL.FFI:WL_MESSAGE))
                     (SETF NAME (FOREIGN-STRING-ALLOC "resize")
                           SIGNATURE "i"
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
            (SETF NAME (FOREIGN-STRING-ALLOC "wl_shm_pool")
                  VERSION 1
                  METHOD_COUNT 3
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
      (CL-WL::DEBUG-LOG! "Dispatching ~a:~a~%" "wl_shm_pool" "create-buffer")
      (FUNCALL 'CREATE-BUFFER RESOURCE
               (VALUES
                (FOREIGN-SLOT-VALUE
                 (MEM-APTR ARGS '(:UNION CL-WL.FFI:WL_ARGUMENT) 0)
                 '(:UNION CL-WL.FFI:WL_ARGUMENT) 'WL-FFI::N))
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
                 '(:UNION CL-WL.FFI:WL_ARGUMENT) 'WL-FFI::I))
               (VALUES
                (FOREIGN-SLOT-VALUE
                 (MEM-APTR ARGS '(:UNION CL-WL.FFI:WL_ARGUMENT) 4)
                 '(:UNION CL-WL.FFI:WL_ARGUMENT) 'WL-FFI::I))
               (WL-SHM::FORMAT-FROM-VALUE
                (FOREIGN-SLOT-VALUE
                 (MEM-APTR ARGS '(:UNION CL-WL.FFI:WL_ARGUMENT) 5)
                 '(:UNION CL-WL.FFI:WL_ARGUMENT) 'WL-FFI::U))))
     (1
      (CL-WL::DEBUG-LOG! "Dispatching ~a:~a~%" "wl_shm_pool" "destroy")
      (FUNCALL 'DESTROY RESOURCE))
     (2
      (CL-WL::DEBUG-LOG! "Dispatching ~a:~a~%" "wl_shm_pool" "resize")
      (FUNCALL 'RESIZE RESOURCE
               (VALUES
                (FOREIGN-SLOT-VALUE
                 (MEM-APTR ARGS '(:UNION CL-WL.FFI:WL_ARGUMENT) 0)
                 '(:UNION CL-WL.FFI:WL_ARGUMENT) 'WL-FFI::I))))))
 0)

(DEFVAR *DISPATCHER* (CALLBACK DISPATCHER-FFI))

(DEFMETHOD SHARED-INITIALIZE :AFTER ((INSTANCE DISPATCH) SLOT-NAMES &KEY ID)
  (DECLARE (IGNORE SLOT-NAMES))
  (UNLESS (WL_SHM_POOL-ID INSTANCE)
    (LET* ((RESOURCE
            (CL-WL::CREATE-RESOURCE (CL-WL:PTR (CL-WL:CLIENT INSTANCE))
                                    *INTERFACE* (VERSION INSTANCE)
                                    (OR ID 0))))
      (SETF (CL-WL::TRANSIENT-ID INSTANCE)
              (SETF (WL_SHM_POOL-ID INSTANCE)
                      (OR ID (CL-WL.FFI:RESOURCE-GET-ID RESOURCE))))
      (SETF (WL_SHM_POOL-PTR INSTANCE) RESOURCE)
      (SETF (GETHASH (POINTER-ADDRESS RESOURCE) CL-WL::*RESOURCE-TRACKER*)
              INSTANCE)
      (RESOURCE-SET-DISPATCHER RESOURCE *DISPATCHER* (NULL-POINTER)
       (NULL-POINTER) (NULL-POINTER)))))

(DEFCLASS GLOBAL (CL-WL:GLOBAL) NIL
          (:DEFAULT-INITARGS :VERSION 1 :DISPATCH-IMPL 'DISPATCH)
          (:DOCUMENTATION "a shared memory pool

The wl_shm_pool object encapsulates a piece of memory shared
      between the compositor and client.  Through the wl_shm_pool
      object, the client can allocate shared memory wl_buffer objects.
      All objects created through the same pool share the same
      underlying mapped memory. Reusing the mapped memory avoids the
      setup/teardown overhead and is useful when interactively resizing
      a surface or for many small buffers.
"))

(DEFMETHOD DISPATCH-BIND ((GLOBAL GLOBAL) CLIENT DATA VERSION ID)
  "Default bind implementation for the wl_shm_pool global object.
This can be overriden by inheritance in case if custom behaviour is required."
  (CL-WL::DEBUG-LOG! "Binding ~a~%" "wl_shm_pool")
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
  (CL-WL::DEBUG-LOG! "Initializing global object: ~a~%" "wl_shm_pool")
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

(IN-PACKAGE :WL_SHM)

(DEFCLASS DISPATCH (CL-WL:OBJECT)
          ((WL_SHM-ID :INITFORM NIL :ACCESSOR
            WL_SHM-ID)
           (WL_SHM-PTR :INITFORM NIL :ACCESSOR
            WL_SHM-PTR))
          (:DEFAULT-INITARGS :VERSION 1)
          (:DOCUMENTATION "shared memory support

A singleton global object that provides support for shared
      memory.

      Clients can create wl_shm_pool objects using the create_pool
      request.

      On binding the wl_shm object one or more format events
      are emitted to inform clients about the valid pixel formats
      that can be used for buffers.
"))

(DEFGENERIC CREATE-POOL
    (RESOURCE ID FD SIZE))

(DEFMETHOD CL-WL:DESTROY ((DISPATCH DISPATCH))
  (CL-WL::DEBUG-LOG! "Destroying dispatch object: ~a~%" "wl_shm")
  (WHEN (CL-WL::DESTROY-CALLBACK DISPATCH)
    (LOOP FOR CALLBACK IN (CL-WL::DESTROY-CALLBACK DISPATCH)
          DO (FUNCALL CALLBACK DISPATCH)))
  (LET ((RESOURCE-PTR (WL_SHM-PTR DISPATCH)))
    (CL-WL::REMOVE-RESOURCE (POINTER-ADDRESS RESOURCE-PTR)))
  (CL-WL::DN-IF DISPATCH))

(PUSHNEW
 (LIST "wl_shm" (LIST "wl_shm_pool")
       (LAMBDA ()
         (SETF *INTERFACE* (FOREIGN-ALLOC '(:STRUCT INTERFACE)))
         (LET ((REQUESTS-PTR
                (LET ((MESSAGES
                       (CFFI:FOREIGN-ALLOC '(:STRUCT CL-WL.FFI:WL_MESSAGE)
                                           :COUNT 1)))
                  (LET ((INTERFACE-ARRAY
                         (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID))
                                             :COUNT 3))
                        (MSG-PTR
                         (MEM-APTR MESSAGES '(:STRUCT CL-WL.FFI:WL_MESSAGE) 0)))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 0)
                            WL_SHM_POOL::*INTERFACE*)
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 1) (NULL-POINTER))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 2) (NULL-POINTER))
                    (WITH-FOREIGN-SLOTS
                     ((NAME SIGNATURE TYPES) MSG-PTR
                      (:STRUCT CL-WL.FFI:WL_MESSAGE))
                     (SETF NAME (FOREIGN-STRING-ALLOC "create_pool")
                           SIGNATURE "nhi"
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
                     (SETF NAME (FOREIGN-STRING-ALLOC "format")
                           SIGNATURE "u"
                           TYPES INTERFACE-ARRAY)))
                  MESSAGES)))
           (WITH-FOREIGN-SLOTS
            ((NAME VERSION METHOD_COUNT METHODS EVENT_COUNT EVENTS) *INTERFACE*
             (:STRUCT INTERFACE))
            (SETF NAME (FOREIGN-STRING-ALLOC "wl_shm")
                  VERSION 1
                  METHOD_COUNT 1
                  METHODS REQUESTS-PTR
                  EVENT_COUNT 1
                  EVENTS EVENTS-PTR)))))
 CL-WL::*INTERFACE-INIT-LIST* :TEST #'CL-WL::INTERFACE-EXISTS-TEST)

(DEFCALLBACK DISPATCHER-FFI :INT
 ((DATA :POINTER) (TARGET :POINTER) (OPCODE :UINT) (MESSAGE :POINTER)
  (ARGS :POINTER))
 (DECLARE (IGNORE DATA MESSAGE))
 (LET ((RESOURCE (GETHASH (POINTER-ADDRESS TARGET) CL-WL::*RESOURCE-TRACKER*)))
   (ECASE OPCODE
     (0
      (CL-WL::DEBUG-LOG! "Dispatching ~a:~a~%" "wl_shm" "create-pool")
      (FUNCALL 'CREATE-POOL RESOURCE
               (VALUES
                (FOREIGN-SLOT-VALUE
                 (MEM-APTR ARGS '(:UNION CL-WL.FFI:WL_ARGUMENT) 0)
                 '(:UNION CL-WL.FFI:WL_ARGUMENT) 'WL-FFI::N))
               (VALUES
                (FOREIGN-SLOT-VALUE
                 (MEM-APTR ARGS '(:UNION CL-WL.FFI:WL_ARGUMENT) 1)
                 '(:UNION CL-WL.FFI:WL_ARGUMENT) 'WL-FFI::H))
               (VALUES
                (FOREIGN-SLOT-VALUE
                 (MEM-APTR ARGS '(:UNION CL-WL.FFI:WL_ARGUMENT) 2)
                 '(:UNION CL-WL.FFI:WL_ARGUMENT) 'WL-FFI::I))))))
 0)

(DEFVAR *DISPATCHER* (CALLBACK DISPATCHER-FFI))

(DEFMETHOD SHARED-INITIALIZE :AFTER ((INSTANCE DISPATCH) SLOT-NAMES &KEY ID)
  (DECLARE (IGNORE SLOT-NAMES))
  (UNLESS (WL_SHM-ID INSTANCE)
    (LET* ((RESOURCE
            (CL-WL::CREATE-RESOURCE (CL-WL:PTR (CL-WL:CLIENT INSTANCE))
                                    *INTERFACE* (VERSION INSTANCE)
                                    (OR ID 0))))
      (SETF (CL-WL::TRANSIENT-ID INSTANCE)
              (SETF (WL_SHM-ID INSTANCE)
                      (OR ID (CL-WL.FFI:RESOURCE-GET-ID RESOURCE))))
      (SETF (WL_SHM-PTR INSTANCE) RESOURCE)
      (SETF (GETHASH (POINTER-ADDRESS RESOURCE) CL-WL::*RESOURCE-TRACKER*)
              INSTANCE)
      (RESOURCE-SET-DISPATCHER RESOURCE *DISPATCHER* (NULL-POINTER)
       (NULL-POINTER) (NULL-POINTER)))))

(DEFMETHOD SEND-FORMAT ((DISPATCH DISPATCH) FORMAT)
  (CL-WL::DEBUG-LOG! "Event: ~a~%" "format")
  (LET ((ARG-LIST (FOREIGN-ALLOC '(:UNION WL_ARGUMENT) :COUNT 1)))
    (SETF (FOREIGN-SLOT-VALUE
           (MEM-AREF ARG-LIST '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 0)
           '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 'WL-FFI::U)
            (FORMAT-TO-VALUE FORMAT))
    (RESOURCE-POST-EVENT-ARRAY (WL_SHM-PTR DISPATCH) 0 ARG-LIST)))

(DEFUN ERROR-FROM-VALUE (NUMBER)
  (LOOP FOR ENTRY IN '((0 :INVALID-FORMAT)
                       (1 :INVALID-STRIDE)
                       (2 :INVALID-FD))
        FOR VALUE = (CAR ENTRY)
        FOR KEYWORD = (CADR ENTRY)
        WHEN (EQ NUMBER VALUE) RETURN KEYWORD
        FINALLY (ERROR (FORMAT NIL "Unknown enum value: ~a" NUMBER))))

(DEFUN ERROR-TO-VALUE (KEY)
  (LOOP FOR ENTRY IN '((0 :INVALID-FORMAT)
                       (1 :INVALID-STRIDE)
                       (2 :INVALID-FD))
        FOR VALUE = (CAR ENTRY)
        FOR KEYWORD = (CADR ENTRY)
        WHEN (EQ KEY KEYWORD) RETURN VALUE
        FINALLY (ERROR (FORMAT NIL "Unknown enum keyword: ~a" KEY))))

(DEFUN FORMAT-FROM-VALUE (NUMBER)
  (LOOP FOR ENTRY IN '((0 :ARGB8888) (1 :XRGB8888)
                       (538982467 :C8)
                       (943867730 :RGB332)
                       (944916290 :BGR233)
                       (842093144 :XRGB4444)
                       (842089048 :XBGR4444)
                       (842094674 :RGBX4444)
                       (842094658 :BGRX4444)
                       (842093121 :ARGB4444)
                       (842089025 :ABGR4444)
                       (842088786 :RGBA4444)
                       (842088770 :BGRA4444)
                       (892424792 :XRGB1555)
                       (892420696 :XBGR1555)
                       (892426322 :RGBX5551)
                       (892426306 :BGRX5551)
                       (892424769 :ARGB1555)
                       (892420673 :ABGR1555)
                       (892420434 :RGBA5551)
                       (892420418 :BGRA5551)
                       (909199186 :RGB565)
                       (909199170 :BGR565)
                       (875710290 :RGB888)
                       (875710274 :BGR888)
                       (875709016 :XBGR8888)
                       (875714642 :RGBX8888)
                       (875714626 :BGRX8888)
                       (875708993 :ABGR8888)
                       (875708754 :RGBA8888)
                       (875708738 :BGRA8888)
                       (808669784 :XRGB2101010)
                       (808665688 :XBGR2101010)
                       (808671314 :RGBX1010102)
                       (808671298 :BGRX1010102)
                       (808669761 :ARGB2101010)
                       (808665665 :ABGR2101010)
                       (808665426 :RGBA1010102)
                       (808665410 :BGRA1010102)
                       (1448695129 :YUYV)
                       (1431918169 :YVYU)
                       (1498831189 :UYVY)
                       (1498765654 :VYUY)
                       (1448433985 :AYUV)
                       (842094158 :NV12)
                       (825382478 :NV21)
                       (909203022 :NV16)
                       (825644622 :NV61)
                       (961959257 :YUV410)
                       (961893977 :YVU410)
                       (825316697 :YUV411)
                       (825316953 :YVU411)
                       (842093913 :YUV420)
                       (842094169 :YVU420)
                       (909202777 :YUV422)
                       (909203033 :YVU422)
                       (875713881 :YUV444)
                       (875714137 :YVU444)
                       (538982482 :R8) (540422482 :R16)
                       (943212370 :RG88)
                       (943215175 :GR88)
                       (842221394 :RG1616)
                       (842224199 :GR1616)
                       (1211388504 :XRGB16161616F)
                       (1211384408 :XBGR16161616F)
                       (1211388481 :ARGB16161616F)
                       (1211384385 :ABGR16161616F)
                       (1448434008 :XYUV8888)
                       (875713878 :VUY888)
                       (808670550 :VUY101010)
                       (808530521 :Y210)
                       (842084953 :Y212)
                       (909193817 :Y216)
                       (808531033 :Y410)
                       (842085465 :Y412)
                       (909194329 :Y416)
                       (808670808 :XVYU2101010)
                       (909334104 :XVYU12-16161616)
                       (942954072 :XVYU16161616)
                       (810299481 :Y0L0)
                       (810299480 :X0L0)
                       (843853913 :Y0L2)
                       (843853912 :X0L2)
                       (942691673 :YUV420-8BIT)
                       (808539481 :YUV420-10BIT)
                       (943805016 :XRGB8888-A8)
                       (943800920 :XBGR8888-A8)
                       (943806546 :RGBX8888-A8)
                       (943806530 :BGRX8888-A8)
                       (943798354 :RGB888-A8)
                       (943798338 :BGR888-A8)
                       (943797586 :RGB565-A8)
                       (943797570 :BGR565-A8)
                       (875714126 :NV24)
                       (842290766 :NV42)
                       (808530512 :P210)
                       (808530000 :P010)
                       (842084432 :P012)
                       (909193296 :P016)
                       (808534593 :AXBXGXRX106106106106)
                       (892425806 :NV15)
                       (808531025 :Q410)
                       (825242705 :Q401)
                       (942953048 :XRGB16161616)
                       (942948952 :XBGR16161616)
                       (942953025 :ARGB16161616)
                       (942948929 :ABGR16161616))
        FOR VALUE = (CAR ENTRY)
        FOR KEYWORD = (CADR ENTRY)
        WHEN (EQ NUMBER VALUE) RETURN KEYWORD
        FINALLY (ERROR (FORMAT NIL "Unknown enum value: ~a" NUMBER))))

(DEFUN FORMAT-TO-VALUE (KEY)
  (LOOP FOR ENTRY IN '((0 :ARGB8888) (1 :XRGB8888)
                       (538982467 :C8)
                       (943867730 :RGB332)
                       (944916290 :BGR233)
                       (842093144 :XRGB4444)
                       (842089048 :XBGR4444)
                       (842094674 :RGBX4444)
                       (842094658 :BGRX4444)
                       (842093121 :ARGB4444)
                       (842089025 :ABGR4444)
                       (842088786 :RGBA4444)
                       (842088770 :BGRA4444)
                       (892424792 :XRGB1555)
                       (892420696 :XBGR1555)
                       (892426322 :RGBX5551)
                       (892426306 :BGRX5551)
                       (892424769 :ARGB1555)
                       (892420673 :ABGR1555)
                       (892420434 :RGBA5551)
                       (892420418 :BGRA5551)
                       (909199186 :RGB565)
                       (909199170 :BGR565)
                       (875710290 :RGB888)
                       (875710274 :BGR888)
                       (875709016 :XBGR8888)
                       (875714642 :RGBX8888)
                       (875714626 :BGRX8888)
                       (875708993 :ABGR8888)
                       (875708754 :RGBA8888)
                       (875708738 :BGRA8888)
                       (808669784 :XRGB2101010)
                       (808665688 :XBGR2101010)
                       (808671314 :RGBX1010102)
                       (808671298 :BGRX1010102)
                       (808669761 :ARGB2101010)
                       (808665665 :ABGR2101010)
                       (808665426 :RGBA1010102)
                       (808665410 :BGRA1010102)
                       (1448695129 :YUYV)
                       (1431918169 :YVYU)
                       (1498831189 :UYVY)
                       (1498765654 :VYUY)
                       (1448433985 :AYUV)
                       (842094158 :NV12)
                       (825382478 :NV21)
                       (909203022 :NV16)
                       (825644622 :NV61)
                       (961959257 :YUV410)
                       (961893977 :YVU410)
                       (825316697 :YUV411)
                       (825316953 :YVU411)
                       (842093913 :YUV420)
                       (842094169 :YVU420)
                       (909202777 :YUV422)
                       (909203033 :YVU422)
                       (875713881 :YUV444)
                       (875714137 :YVU444)
                       (538982482 :R8) (540422482 :R16)
                       (943212370 :RG88)
                       (943215175 :GR88)
                       (842221394 :RG1616)
                       (842224199 :GR1616)
                       (1211388504 :XRGB16161616F)
                       (1211384408 :XBGR16161616F)
                       (1211388481 :ARGB16161616F)
                       (1211384385 :ABGR16161616F)
                       (1448434008 :XYUV8888)
                       (875713878 :VUY888)
                       (808670550 :VUY101010)
                       (808530521 :Y210)
                       (842084953 :Y212)
                       (909193817 :Y216)
                       (808531033 :Y410)
                       (842085465 :Y412)
                       (909194329 :Y416)
                       (808670808 :XVYU2101010)
                       (909334104 :XVYU12-16161616)
                       (942954072 :XVYU16161616)
                       (810299481 :Y0L0)
                       (810299480 :X0L0)
                       (843853913 :Y0L2)
                       (843853912 :X0L2)
                       (942691673 :YUV420-8BIT)
                       (808539481 :YUV420-10BIT)
                       (943805016 :XRGB8888-A8)
                       (943800920 :XBGR8888-A8)
                       (943806546 :RGBX8888-A8)
                       (943806530 :BGRX8888-A8)
                       (943798354 :RGB888-A8)
                       (943798338 :BGR888-A8)
                       (943797586 :RGB565-A8)
                       (943797570 :BGR565-A8)
                       (875714126 :NV24)
                       (842290766 :NV42)
                       (808530512 :P210)
                       (808530000 :P010)
                       (842084432 :P012)
                       (909193296 :P016)
                       (808534593 :AXBXGXRX106106106106)
                       (892425806 :NV15)
                       (808531025 :Q410)
                       (825242705 :Q401)
                       (942953048 :XRGB16161616)
                       (942948952 :XBGR16161616)
                       (942953025 :ARGB16161616)
                       (942948929 :ABGR16161616))
        FOR VALUE = (CAR ENTRY)
        FOR KEYWORD = (CADR ENTRY)
        WHEN (EQ KEY KEYWORD) RETURN VALUE
        FINALLY (ERROR (FORMAT NIL "Unknown enum keyword: ~a" KEY))))

(DEFCLASS GLOBAL (CL-WL:GLOBAL) NIL
          (:DEFAULT-INITARGS :VERSION 1 :DISPATCH-IMPL 'DISPATCH)
          (:DOCUMENTATION "shared memory support

A singleton global object that provides support for shared
      memory.

      Clients can create wl_shm_pool objects using the create_pool
      request.

      On binding the wl_shm object one or more format events
      are emitted to inform clients about the valid pixel formats
      that can be used for buffers.
"))

(DEFMETHOD DISPATCH-BIND ((GLOBAL GLOBAL) CLIENT DATA VERSION ID)
  "Default bind implementation for the wl_shm global object.
This can be overriden by inheritance in case if custom behaviour is required."
  (CL-WL::DEBUG-LOG! "Binding ~a~%" "wl_shm")
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
  (CL-WL::DEBUG-LOG! "Initializing global object: ~a~%" "wl_shm")
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

(IN-PACKAGE :WL_BUFFER)

(DEFCLASS DISPATCH (CL-WL:OBJECT)
          ((WL_BUFFER-ID :INITFORM NIL :ACCESSOR
            WL_BUFFER-ID)
           (WL_BUFFER-PTR :INITFORM NIL :ACCESSOR
            WL_BUFFER-PTR))
          (:DEFAULT-INITARGS :VERSION 1)
          (:DOCUMENTATION "content for a wl_surface

A buffer provides the content for a wl_surface. Buffers are
      created through factory interfaces such as wl_shm, wp_linux_buffer_params
      (from the linux-dmabuf protocol extension) or similar. It has a width and
      a height and can be attached to a wl_surface, but the mechanism by which a
      client provides and updates the contents is defined by the buffer factory
      interface.

      If the buffer uses a format that has an alpha channel, the alpha channel
      is assumed to be premultiplied in the color channels unless otherwise
      specified.

      Note, because wl_buffer objects are created from multiple independent
      factory interfaces, the wl_buffer interface is frozen at version 1.
"))

(DEFGENERIC DESTROY
    (RESOURCE))

(DEFMETHOD CL-WL:DESTROY ((DISPATCH DISPATCH))
  (CL-WL::DEBUG-LOG! "Destroying dispatch object: ~a~%" "wl_buffer")
  (WHEN (CL-WL::DESTROY-CALLBACK DISPATCH)
    (LOOP FOR CALLBACK IN (CL-WL::DESTROY-CALLBACK DISPATCH)
          DO (FUNCALL CALLBACK DISPATCH)))
  (LET ((RESOURCE-PTR (WL_BUFFER-PTR DISPATCH)))
    (CL-WL::REMOVE-RESOURCE (POINTER-ADDRESS RESOURCE-PTR)))
  (CL-WL::DN-IF DISPATCH))

(DEFMETHOD DESTROY ((DISPATCH DISPATCH)) (CL-WL:DESTROY DISPATCH))

(DEFMETHOD DESTROY (EMPTY) NIL)

(PUSHNEW
 (LIST "wl_buffer" (LIST)
       (LAMBDA ()
         (SETF *INTERFACE* (FOREIGN-ALLOC '(:STRUCT INTERFACE)))
         (LET ((REQUESTS-PTR
                (LET ((MESSAGES
                       (CFFI:FOREIGN-ALLOC '(:STRUCT CL-WL.FFI:WL_MESSAGE)
                                           :COUNT 1)))
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
                  MESSAGES))
               (EVENTS-PTR
                (LET ((MESSAGES
                       (CFFI:FOREIGN-ALLOC '(:STRUCT CL-WL.FFI:WL_MESSAGE)
                                           :COUNT 1)))
                  (LET ((INTERFACE-ARRAY
                         (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID))
                                             :COUNT 0))
                        (MSG-PTR
                         (MEM-APTR MESSAGES '(:STRUCT CL-WL.FFI:WL_MESSAGE) 0)))
                    (WITH-FOREIGN-SLOTS
                     ((NAME SIGNATURE TYPES) MSG-PTR
                      (:STRUCT CL-WL.FFI:WL_MESSAGE))
                     (SETF NAME (FOREIGN-STRING-ALLOC "release")
                           SIGNATURE ""
                           TYPES INTERFACE-ARRAY)))
                  MESSAGES)))
           (WITH-FOREIGN-SLOTS
            ((NAME VERSION METHOD_COUNT METHODS EVENT_COUNT EVENTS) *INTERFACE*
             (:STRUCT INTERFACE))
            (SETF NAME (FOREIGN-STRING-ALLOC "wl_buffer")
                  VERSION 1
                  METHOD_COUNT 1
                  METHODS REQUESTS-PTR
                  EVENT_COUNT 1
                  EVENTS EVENTS-PTR)))))
 CL-WL::*INTERFACE-INIT-LIST* :TEST #'CL-WL::INTERFACE-EXISTS-TEST)

(DEFCALLBACK DISPATCHER-FFI :INT
 ((DATA :POINTER) (TARGET :POINTER) (OPCODE :UINT) (MESSAGE :POINTER)
  (ARGS :POINTER))
 (DECLARE (IGNORE DATA MESSAGE ARGS))
 (LET ((RESOURCE (GETHASH (POINTER-ADDRESS TARGET) CL-WL::*RESOURCE-TRACKER*)))
   (ECASE OPCODE
     (0
      (CL-WL::DEBUG-LOG! "Dispatching ~a:~a~%" "wl_buffer" "destroy")
      (FUNCALL 'DESTROY RESOURCE))))
 0)

(DEFVAR *DISPATCHER* (CALLBACK DISPATCHER-FFI))

(DEFMETHOD SHARED-INITIALIZE :AFTER ((INSTANCE DISPATCH) SLOT-NAMES &KEY ID)
  (DECLARE (IGNORE SLOT-NAMES))
  (UNLESS (WL_BUFFER-ID INSTANCE)
    (LET* ((RESOURCE
            (CL-WL::CREATE-RESOURCE (CL-WL:PTR (CL-WL:CLIENT INSTANCE))
                                    *INTERFACE* (VERSION INSTANCE)
                                    (OR ID 0))))
      (SETF (CL-WL::TRANSIENT-ID INSTANCE)
              (SETF (WL_BUFFER-ID INSTANCE)
                      (OR ID (CL-WL.FFI:RESOURCE-GET-ID RESOURCE))))
      (SETF (WL_BUFFER-PTR INSTANCE) RESOURCE)
      (SETF (GETHASH (POINTER-ADDRESS RESOURCE) CL-WL::*RESOURCE-TRACKER*)
              INSTANCE)
      (RESOURCE-SET-DISPATCHER RESOURCE *DISPATCHER* (NULL-POINTER)
       (NULL-POINTER) (NULL-POINTER)))))

(DEFMETHOD SEND-RELEASE ((DISPATCH DISPATCH))
  (CL-WL::DEBUG-LOG! "Event: ~a~%" "release")
  (LET ((ARG-LIST (FOREIGN-ALLOC '(:UNION WL_ARGUMENT) :COUNT 0)))
    (RESOURCE-POST-EVENT-ARRAY (WL_BUFFER-PTR DISPATCH) 0
     ARG-LIST)))

(DEFCLASS GLOBAL (CL-WL:GLOBAL) NIL
          (:DEFAULT-INITARGS :VERSION 1 :DISPATCH-IMPL 'DISPATCH)
          (:DOCUMENTATION "content for a wl_surface

A buffer provides the content for a wl_surface. Buffers are
      created through factory interfaces such as wl_shm, wp_linux_buffer_params
      (from the linux-dmabuf protocol extension) or similar. It has a width and
      a height and can be attached to a wl_surface, but the mechanism by which a
      client provides and updates the contents is defined by the buffer factory
      interface.

      If the buffer uses a format that has an alpha channel, the alpha channel
      is assumed to be premultiplied in the color channels unless otherwise
      specified.

      Note, because wl_buffer objects are created from multiple independent
      factory interfaces, the wl_buffer interface is frozen at version 1.
"))

(DEFMETHOD DISPATCH-BIND ((GLOBAL GLOBAL) CLIENT DATA VERSION ID)
  "Default bind implementation for the wl_buffer global object.
This can be overriden by inheritance in case if custom behaviour is required."
  (CL-WL::DEBUG-LOG! "Binding ~a~%" "wl_buffer")
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
  (CL-WL::DEBUG-LOG! "Initializing global object: ~a~%" "wl_buffer")
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

(IN-PACKAGE :WL_DATA_OFFER)

(DEFCLASS DISPATCH (CL-WL:OBJECT)
          ((WL_DATA_OFFER-ID :INITFORM NIL :ACCESSOR
            WL_DATA_OFFER-ID)
           (WL_DATA_OFFER-PTR :INITFORM NIL :ACCESSOR
            WL_DATA_OFFER-PTR))
          (:DEFAULT-INITARGS :VERSION 3)
          (:DOCUMENTATION "offer to transfer data

A wl_data_offer represents a piece of data offered for transfer
      by another client (the source client).  It is used by the
      copy-and-paste and drag-and-drop mechanisms.  The offer
      describes the different mime types that the data can be
      converted to and provides the mechanism for transferring the
      data directly from the source client.
"))

(DEFGENERIC ACCEPT
    (RESOURCE SERIAL MIME_TYPE))

(DEFGENERIC RECEIVE
    (RESOURCE MIME_TYPE FD))

(DEFGENERIC DESTROY
    (RESOURCE))

(DEFGENERIC FINISH
    (RESOURCE))

(DEFGENERIC SET-ACTIONS
    (RESOURCE DND_ACTIONS PREFERRED_ACTION))

(DEFMETHOD CL-WL:DESTROY ((DISPATCH DISPATCH))
  (CL-WL::DEBUG-LOG! "Destroying dispatch object: ~a~%" "wl_data_offer")
  (WHEN (CL-WL::DESTROY-CALLBACK DISPATCH)
    (LOOP FOR CALLBACK IN (CL-WL::DESTROY-CALLBACK DISPATCH)
          DO (FUNCALL CALLBACK DISPATCH)))
  (LET ((RESOURCE-PTR (WL_DATA_OFFER-PTR DISPATCH)))
    (CL-WL::REMOVE-RESOURCE (POINTER-ADDRESS RESOURCE-PTR)))
  (CL-WL::DN-IF DISPATCH))

(DEFMETHOD DESTROY ((DISPATCH DISPATCH)) (CL-WL:DESTROY DISPATCH))

(DEFMETHOD DESTROY (EMPTY) NIL)

(PUSHNEW
 (LIST "wl_data_offer" (LIST)
       (LAMBDA ()
         (SETF *INTERFACE* (FOREIGN-ALLOC '(:STRUCT INTERFACE)))
         (LET ((REQUESTS-PTR
                (LET ((MESSAGES
                       (CFFI:FOREIGN-ALLOC '(:STRUCT CL-WL.FFI:WL_MESSAGE)
                                           :COUNT 5)))
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
                     (SETF NAME (FOREIGN-STRING-ALLOC "accept")
                           SIGNATURE "u?s"
                           TYPES INTERFACE-ARRAY)))
                  (LET ((INTERFACE-ARRAY
                         (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID))
                                             :COUNT 2))
                        (MSG-PTR
                         (MEM-APTR MESSAGES '(:STRUCT CL-WL.FFI:WL_MESSAGE) 1)))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 0) (NULL-POINTER))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 1) (NULL-POINTER))
                    (WITH-FOREIGN-SLOTS
                     ((NAME SIGNATURE TYPES) MSG-PTR
                      (:STRUCT CL-WL.FFI:WL_MESSAGE))
                     (SETF NAME (FOREIGN-STRING-ALLOC "receive")
                           SIGNATURE "sh"
                           TYPES INTERFACE-ARRAY)))
                  (LET ((INTERFACE-ARRAY
                         (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID))
                                             :COUNT 0))
                        (MSG-PTR
                         (MEM-APTR MESSAGES '(:STRUCT CL-WL.FFI:WL_MESSAGE) 2)))
                    (WITH-FOREIGN-SLOTS
                     ((NAME SIGNATURE TYPES) MSG-PTR
                      (:STRUCT CL-WL.FFI:WL_MESSAGE))
                     (SETF NAME (FOREIGN-STRING-ALLOC "destroy")
                           SIGNATURE ""
                           TYPES INTERFACE-ARRAY)))
                  (LET ((INTERFACE-ARRAY
                         (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID))
                                             :COUNT 0))
                        (MSG-PTR
                         (MEM-APTR MESSAGES '(:STRUCT CL-WL.FFI:WL_MESSAGE) 3)))
                    (WITH-FOREIGN-SLOTS
                     ((NAME SIGNATURE TYPES) MSG-PTR
                      (:STRUCT CL-WL.FFI:WL_MESSAGE))
                     (SETF NAME (FOREIGN-STRING-ALLOC "finish")
                           SIGNATURE "3"
                           TYPES INTERFACE-ARRAY)))
                  (LET ((INTERFACE-ARRAY
                         (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID))
                                             :COUNT 2))
                        (MSG-PTR
                         (MEM-APTR MESSAGES '(:STRUCT CL-WL.FFI:WL_MESSAGE) 4)))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 0) (NULL-POINTER))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 1) (NULL-POINTER))
                    (WITH-FOREIGN-SLOTS
                     ((NAME SIGNATURE TYPES) MSG-PTR
                      (:STRUCT CL-WL.FFI:WL_MESSAGE))
                     (SETF NAME (FOREIGN-STRING-ALLOC "set_actions")
                           SIGNATURE "3uu"
                           TYPES INTERFACE-ARRAY)))
                  MESSAGES))
               (EVENTS-PTR
                (LET ((MESSAGES
                       (CFFI:FOREIGN-ALLOC '(:STRUCT CL-WL.FFI:WL_MESSAGE)
                                           :COUNT 3)))
                  (LET ((INTERFACE-ARRAY
                         (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID))
                                             :COUNT 1))
                        (MSG-PTR
                         (MEM-APTR MESSAGES '(:STRUCT CL-WL.FFI:WL_MESSAGE) 0)))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 0) (NULL-POINTER))
                    (WITH-FOREIGN-SLOTS
                     ((NAME SIGNATURE TYPES) MSG-PTR
                      (:STRUCT CL-WL.FFI:WL_MESSAGE))
                     (SETF NAME (FOREIGN-STRING-ALLOC "offer")
                           SIGNATURE "s"
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
                     (SETF NAME (FOREIGN-STRING-ALLOC "source_actions")
                           SIGNATURE "3u"
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
                     (SETF NAME (FOREIGN-STRING-ALLOC "action")
                           SIGNATURE "3u"
                           TYPES INTERFACE-ARRAY)))
                  MESSAGES)))
           (WITH-FOREIGN-SLOTS
            ((NAME VERSION METHOD_COUNT METHODS EVENT_COUNT EVENTS) *INTERFACE*
             (:STRUCT INTERFACE))
            (SETF NAME (FOREIGN-STRING-ALLOC "wl_data_offer")
                  VERSION 3
                  METHOD_COUNT 5
                  METHODS REQUESTS-PTR
                  EVENT_COUNT 3
                  EVENTS EVENTS-PTR)))))
 CL-WL::*INTERFACE-INIT-LIST* :TEST #'CL-WL::INTERFACE-EXISTS-TEST)

(DEFCALLBACK DISPATCHER-FFI :INT
 ((DATA :POINTER) (TARGET :POINTER) (OPCODE :UINT) (MESSAGE :POINTER)
  (ARGS :POINTER))
 (DECLARE (IGNORE DATA MESSAGE))
 (LET ((RESOURCE (GETHASH (POINTER-ADDRESS TARGET) CL-WL::*RESOURCE-TRACKER*)))
   (ECASE OPCODE
     (0
      (CL-WL::DEBUG-LOG! "Dispatching ~a:~a~%" "wl_data_offer" "accept")
      (FUNCALL 'ACCEPT RESOURCE
               (VALUES
                (FOREIGN-SLOT-VALUE
                 (MEM-APTR ARGS '(:UNION CL-WL.FFI:WL_ARGUMENT) 0)
                 '(:UNION CL-WL.FFI:WL_ARGUMENT) 'WL-FFI::U))
               (VALUES
                (FOREIGN-SLOT-VALUE
                 (MEM-APTR ARGS '(:UNION CL-WL.FFI:WL_ARGUMENT) 1)
                 '(:UNION CL-WL.FFI:WL_ARGUMENT) 'WL-FFI::S))))
     (1
      (CL-WL::DEBUG-LOG! "Dispatching ~a:~a~%" "wl_data_offer" "receive")
      (FUNCALL 'RECEIVE RESOURCE
               (VALUES
                (FOREIGN-SLOT-VALUE
                 (MEM-APTR ARGS '(:UNION CL-WL.FFI:WL_ARGUMENT) 0)
                 '(:UNION CL-WL.FFI:WL_ARGUMENT) 'WL-FFI::S))
               (VALUES
                (FOREIGN-SLOT-VALUE
                 (MEM-APTR ARGS '(:UNION CL-WL.FFI:WL_ARGUMENT) 1)
                 '(:UNION CL-WL.FFI:WL_ARGUMENT) 'WL-FFI::H))))
     (2
      (CL-WL::DEBUG-LOG! "Dispatching ~a:~a~%" "wl_data_offer" "destroy")
      (FUNCALL 'DESTROY RESOURCE))
     (3
      (CL-WL::DEBUG-LOG! "Dispatching ~a:~a~%" "wl_data_offer" "finish")
      (FUNCALL 'FINISH RESOURCE))
     (4
      (CL-WL::DEBUG-LOG! "Dispatching ~a:~a~%" "wl_data_offer" "set-actions")
      (FUNCALL 'SET-ACTIONS RESOURCE
               (WL-DATA-DEVICE-MANAGER::DND-ACTION-FROM-VALUE
                (FOREIGN-SLOT-VALUE
                 (MEM-APTR ARGS '(:UNION CL-WL.FFI:WL_ARGUMENT) 0)
                 '(:UNION CL-WL.FFI:WL_ARGUMENT) 'WL-FFI::U))
               (WL-DATA-DEVICE-MANAGER::DND-ACTION-FROM-VALUE
                (FOREIGN-SLOT-VALUE
                 (MEM-APTR ARGS '(:UNION CL-WL.FFI:WL_ARGUMENT) 1)
                 '(:UNION CL-WL.FFI:WL_ARGUMENT) 'WL-FFI::U))))))
 0)

(DEFVAR *DISPATCHER* (CALLBACK DISPATCHER-FFI))

(DEFMETHOD SHARED-INITIALIZE :AFTER ((INSTANCE DISPATCH) SLOT-NAMES &KEY ID)
  (DECLARE (IGNORE SLOT-NAMES))
  (UNLESS (WL_DATA_OFFER-ID INSTANCE)
    (LET* ((RESOURCE
            (CL-WL::CREATE-RESOURCE (CL-WL:PTR (CL-WL:CLIENT INSTANCE))
                                    *INTERFACE* (VERSION INSTANCE)
                                    (OR ID 0))))
      (SETF (CL-WL::TRANSIENT-ID INSTANCE)
              (SETF (WL_DATA_OFFER-ID INSTANCE)
                      (OR ID (CL-WL.FFI:RESOURCE-GET-ID RESOURCE))))
      (SETF (WL_DATA_OFFER-PTR INSTANCE) RESOURCE)
      (SETF (GETHASH (POINTER-ADDRESS RESOURCE) CL-WL::*RESOURCE-TRACKER*)
              INSTANCE)
      (RESOURCE-SET-DISPATCHER RESOURCE *DISPATCHER* (NULL-POINTER)
       (NULL-POINTER) (NULL-POINTER)))))

(DEFMETHOD SEND-OFFER ((DISPATCH DISPATCH) MIME_TYPE)
  (CL-WL::DEBUG-LOG! "Event: ~a~%" "offer")
  (LET ((ARG-LIST (FOREIGN-ALLOC '(:UNION WL_ARGUMENT) :COUNT 1)))
    (SETF (FOREIGN-SLOT-VALUE
           (MEM-AREF ARG-LIST '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 0)
           '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 'WL-FFI::S)
            MIME_TYPE)
    (RESOURCE-POST-EVENT-ARRAY (WL_DATA_OFFER-PTR DISPATCH) 0
     ARG-LIST)))

(DEFMETHOD SEND-SOURCE-ACTIONS
           ((DISPATCH DISPATCH) SOURCE_ACTIONS)
  (CL-WL::DEBUG-LOG! "Event: ~a~%" "source_actions")
  (LET ((ARG-LIST (FOREIGN-ALLOC '(:UNION WL_ARGUMENT) :COUNT 1)))
    (SETF (FOREIGN-SLOT-VALUE
           (MEM-AREF ARG-LIST '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 0)
           '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 'WL-FFI::U)
            (WL-DATA-DEVICE-MANAGER::DND-ACTION-TO-VALUE
             SOURCE_ACTIONS))
    (RESOURCE-POST-EVENT-ARRAY (WL_DATA_OFFER-PTR DISPATCH) 1
     ARG-LIST)))

(DEFMETHOD SEND-ACTION ((DISPATCH DISPATCH) DND_ACTION)
  (CL-WL::DEBUG-LOG! "Event: ~a~%" "action")
  (LET ((ARG-LIST (FOREIGN-ALLOC '(:UNION WL_ARGUMENT) :COUNT 1)))
    (SETF (FOREIGN-SLOT-VALUE
           (MEM-AREF ARG-LIST '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 0)
           '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 'WL-FFI::U)
            (WL-DATA-DEVICE-MANAGER::DND-ACTION-TO-VALUE
             DND_ACTION))
    (RESOURCE-POST-EVENT-ARRAY (WL_DATA_OFFER-PTR DISPATCH) 2
     ARG-LIST)))

(DEFUN ERROR-FROM-VALUE (NUMBER)
  (LOOP FOR ENTRY IN '((0 :INVALID-FINISH)
                       (1 :INVALID-ACTION-MASK)
                       (2 :INVALID-ACTION)
                       (3 :INVALID-OFFER))
        FOR VALUE = (CAR ENTRY)
        FOR KEYWORD = (CADR ENTRY)
        WHEN (EQ NUMBER VALUE) RETURN KEYWORD
        FINALLY (ERROR (FORMAT NIL "Unknown enum value: ~a" NUMBER))))

(DEFUN ERROR-TO-VALUE (KEY)
  (LOOP FOR ENTRY IN '((0 :INVALID-FINISH)
                       (1 :INVALID-ACTION-MASK)
                       (2 :INVALID-ACTION)
                       (3 :INVALID-OFFER))
        FOR VALUE = (CAR ENTRY)
        FOR KEYWORD = (CADR ENTRY)
        WHEN (EQ KEY KEYWORD) RETURN VALUE
        FINALLY (ERROR (FORMAT NIL "Unknown enum keyword: ~a" KEY))))

(DEFCLASS GLOBAL (CL-WL:GLOBAL) NIL
          (:DEFAULT-INITARGS :VERSION 3 :DISPATCH-IMPL 'DISPATCH)
          (:DOCUMENTATION "offer to transfer data

A wl_data_offer represents a piece of data offered for transfer
      by another client (the source client).  It is used by the
      copy-and-paste and drag-and-drop mechanisms.  The offer
      describes the different mime types that the data can be
      converted to and provides the mechanism for transferring the
      data directly from the source client.
"))

(DEFMETHOD DISPATCH-BIND ((GLOBAL GLOBAL) CLIENT DATA VERSION ID)
  "Default bind implementation for the wl_data_offer global object.
This can be overriden by inheritance in case if custom behaviour is required."
  (CL-WL::DEBUG-LOG! "Binding ~a~%" "wl_data_offer")
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
  (CL-WL::DEBUG-LOG! "Initializing global object: ~a~%" "wl_data_offer")
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

(IN-PACKAGE :WL_DATA_SOURCE)

(DEFCLASS DISPATCH (CL-WL:OBJECT)
          ((WL_DATA_SOURCE-ID :INITFORM NIL :ACCESSOR
            WL_DATA_SOURCE-ID)
           (WL_DATA_SOURCE-PTR :INITFORM NIL :ACCESSOR
            WL_DATA_SOURCE-PTR))
          (:DEFAULT-INITARGS :VERSION 3)
          (:DOCUMENTATION "offer to transfer data

The wl_data_source object is the source side of a wl_data_offer.
      It is created by the source client in a data transfer and
      provides a way to describe the offered data and a way to respond
      to requests to transfer the data.
"))

(DEFGENERIC OFFER
    (RESOURCE MIME_TYPE))

(DEFGENERIC DESTROY
    (RESOURCE))

(DEFGENERIC SET-ACTIONS
    (RESOURCE DND_ACTIONS))

(DEFMETHOD CL-WL:DESTROY ((DISPATCH DISPATCH))
  (CL-WL::DEBUG-LOG! "Destroying dispatch object: ~a~%" "wl_data_source")
  (WHEN (CL-WL::DESTROY-CALLBACK DISPATCH)
    (LOOP FOR CALLBACK IN (CL-WL::DESTROY-CALLBACK DISPATCH)
          DO (FUNCALL CALLBACK DISPATCH)))
  (LET ((RESOURCE-PTR (WL_DATA_SOURCE-PTR DISPATCH)))
    (CL-WL::REMOVE-RESOURCE (POINTER-ADDRESS RESOURCE-PTR)))
  (CL-WL::DN-IF DISPATCH))

(DEFMETHOD DESTROY ((DISPATCH DISPATCH)) (CL-WL:DESTROY DISPATCH))

(DEFMETHOD DESTROY (EMPTY) NIL)

(PUSHNEW
 (LIST "wl_data_source" (LIST)
       (LAMBDA ()
         (SETF *INTERFACE* (FOREIGN-ALLOC '(:STRUCT INTERFACE)))
         (LET ((REQUESTS-PTR
                (LET ((MESSAGES
                       (CFFI:FOREIGN-ALLOC '(:STRUCT CL-WL.FFI:WL_MESSAGE)
                                           :COUNT 3)))
                  (LET ((INTERFACE-ARRAY
                         (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID))
                                             :COUNT 1))
                        (MSG-PTR
                         (MEM-APTR MESSAGES '(:STRUCT CL-WL.FFI:WL_MESSAGE) 0)))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 0) (NULL-POINTER))
                    (WITH-FOREIGN-SLOTS
                     ((NAME SIGNATURE TYPES) MSG-PTR
                      (:STRUCT CL-WL.FFI:WL_MESSAGE))
                     (SETF NAME (FOREIGN-STRING-ALLOC "offer")
                           SIGNATURE "s"
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
                  (LET ((INTERFACE-ARRAY
                         (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID))
                                             :COUNT 1))
                        (MSG-PTR
                         (MEM-APTR MESSAGES '(:STRUCT CL-WL.FFI:WL_MESSAGE) 2)))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 0) (NULL-POINTER))
                    (WITH-FOREIGN-SLOTS
                     ((NAME SIGNATURE TYPES) MSG-PTR
                      (:STRUCT CL-WL.FFI:WL_MESSAGE))
                     (SETF NAME (FOREIGN-STRING-ALLOC "set_actions")
                           SIGNATURE "3u"
                           TYPES INTERFACE-ARRAY)))
                  MESSAGES))
               (EVENTS-PTR
                (LET ((MESSAGES
                       (CFFI:FOREIGN-ALLOC '(:STRUCT CL-WL.FFI:WL_MESSAGE)
                                           :COUNT 6)))
                  (LET ((INTERFACE-ARRAY
                         (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID))
                                             :COUNT 1))
                        (MSG-PTR
                         (MEM-APTR MESSAGES '(:STRUCT CL-WL.FFI:WL_MESSAGE) 0)))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 0) (NULL-POINTER))
                    (WITH-FOREIGN-SLOTS
                     ((NAME SIGNATURE TYPES) MSG-PTR
                      (:STRUCT CL-WL.FFI:WL_MESSAGE))
                     (SETF NAME (FOREIGN-STRING-ALLOC "target")
                           SIGNATURE "?s"
                           TYPES INTERFACE-ARRAY)))
                  (LET ((INTERFACE-ARRAY
                         (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID))
                                             :COUNT 2))
                        (MSG-PTR
                         (MEM-APTR MESSAGES '(:STRUCT CL-WL.FFI:WL_MESSAGE) 1)))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 0) (NULL-POINTER))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 1) (NULL-POINTER))
                    (WITH-FOREIGN-SLOTS
                     ((NAME SIGNATURE TYPES) MSG-PTR
                      (:STRUCT CL-WL.FFI:WL_MESSAGE))
                     (SETF NAME (FOREIGN-STRING-ALLOC "send")
                           SIGNATURE "sh"
                           TYPES INTERFACE-ARRAY)))
                  (LET ((INTERFACE-ARRAY
                         (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID))
                                             :COUNT 0))
                        (MSG-PTR
                         (MEM-APTR MESSAGES '(:STRUCT CL-WL.FFI:WL_MESSAGE) 2)))
                    (WITH-FOREIGN-SLOTS
                     ((NAME SIGNATURE TYPES) MSG-PTR
                      (:STRUCT CL-WL.FFI:WL_MESSAGE))
                     (SETF NAME (FOREIGN-STRING-ALLOC "cancelled")
                           SIGNATURE ""
                           TYPES INTERFACE-ARRAY)))
                  (LET ((INTERFACE-ARRAY
                         (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID))
                                             :COUNT 0))
                        (MSG-PTR
                         (MEM-APTR MESSAGES '(:STRUCT CL-WL.FFI:WL_MESSAGE) 3)))
                    (WITH-FOREIGN-SLOTS
                     ((NAME SIGNATURE TYPES) MSG-PTR
                      (:STRUCT CL-WL.FFI:WL_MESSAGE))
                     (SETF NAME (FOREIGN-STRING-ALLOC "dnd_drop_performed")
                           SIGNATURE "3"
                           TYPES INTERFACE-ARRAY)))
                  (LET ((INTERFACE-ARRAY
                         (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID))
                                             :COUNT 0))
                        (MSG-PTR
                         (MEM-APTR MESSAGES '(:STRUCT CL-WL.FFI:WL_MESSAGE) 4)))
                    (WITH-FOREIGN-SLOTS
                     ((NAME SIGNATURE TYPES) MSG-PTR
                      (:STRUCT CL-WL.FFI:WL_MESSAGE))
                     (SETF NAME (FOREIGN-STRING-ALLOC "dnd_finished")
                           SIGNATURE "3"
                           TYPES INTERFACE-ARRAY)))
                  (LET ((INTERFACE-ARRAY
                         (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID))
                                             :COUNT 1))
                        (MSG-PTR
                         (MEM-APTR MESSAGES '(:STRUCT CL-WL.FFI:WL_MESSAGE) 5)))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 0) (NULL-POINTER))
                    (WITH-FOREIGN-SLOTS
                     ((NAME SIGNATURE TYPES) MSG-PTR
                      (:STRUCT CL-WL.FFI:WL_MESSAGE))
                     (SETF NAME (FOREIGN-STRING-ALLOC "action")
                           SIGNATURE "3u"
                           TYPES INTERFACE-ARRAY)))
                  MESSAGES)))
           (WITH-FOREIGN-SLOTS
            ((NAME VERSION METHOD_COUNT METHODS EVENT_COUNT EVENTS) *INTERFACE*
             (:STRUCT INTERFACE))
            (SETF NAME (FOREIGN-STRING-ALLOC "wl_data_source")
                  VERSION 3
                  METHOD_COUNT 3
                  METHODS REQUESTS-PTR
                  EVENT_COUNT 6
                  EVENTS EVENTS-PTR)))))
 CL-WL::*INTERFACE-INIT-LIST* :TEST #'CL-WL::INTERFACE-EXISTS-TEST)

(DEFCALLBACK DISPATCHER-FFI :INT
 ((DATA :POINTER) (TARGET :POINTER) (OPCODE :UINT) (MESSAGE :POINTER)
  (ARGS :POINTER))
 (DECLARE (IGNORE DATA MESSAGE))
 (LET ((RESOURCE (GETHASH (POINTER-ADDRESS TARGET) CL-WL::*RESOURCE-TRACKER*)))
   (ECASE OPCODE
     (0
      (CL-WL::DEBUG-LOG! "Dispatching ~a:~a~%" "wl_data_source" "offer")
      (FUNCALL 'OFFER RESOURCE
               (VALUES
                (FOREIGN-SLOT-VALUE
                 (MEM-APTR ARGS '(:UNION CL-WL.FFI:WL_ARGUMENT) 0)
                 '(:UNION CL-WL.FFI:WL_ARGUMENT) 'WL-FFI::S))))
     (1
      (CL-WL::DEBUG-LOG! "Dispatching ~a:~a~%" "wl_data_source" "destroy")
      (FUNCALL 'DESTROY RESOURCE))
     (2
      (CL-WL::DEBUG-LOG! "Dispatching ~a:~a~%" "wl_data_source" "set-actions")
      (FUNCALL 'SET-ACTIONS RESOURCE
               (WL-DATA-DEVICE-MANAGER::DND-ACTION-FROM-VALUE
                (FOREIGN-SLOT-VALUE
                 (MEM-APTR ARGS '(:UNION CL-WL.FFI:WL_ARGUMENT) 0)
                 '(:UNION CL-WL.FFI:WL_ARGUMENT) 'WL-FFI::U))))))
 0)

(DEFVAR *DISPATCHER* (CALLBACK DISPATCHER-FFI))

(DEFMETHOD SHARED-INITIALIZE :AFTER ((INSTANCE DISPATCH) SLOT-NAMES &KEY ID)
  (DECLARE (IGNORE SLOT-NAMES))
  (UNLESS (WL_DATA_SOURCE-ID INSTANCE)
    (LET* ((RESOURCE
            (CL-WL::CREATE-RESOURCE (CL-WL:PTR (CL-WL:CLIENT INSTANCE))
                                    *INTERFACE* (VERSION INSTANCE)
                                    (OR ID 0))))
      (SETF (CL-WL::TRANSIENT-ID INSTANCE)
              (SETF (WL_DATA_SOURCE-ID INSTANCE)
                      (OR ID (CL-WL.FFI:RESOURCE-GET-ID RESOURCE))))
      (SETF (WL_DATA_SOURCE-PTR INSTANCE) RESOURCE)
      (SETF (GETHASH (POINTER-ADDRESS RESOURCE) CL-WL::*RESOURCE-TRACKER*)
              INSTANCE)
      (RESOURCE-SET-DISPATCHER RESOURCE *DISPATCHER* (NULL-POINTER)
       (NULL-POINTER) (NULL-POINTER)))))

(DEFMETHOD SEND-TARGET ((DISPATCH DISPATCH) MIME_TYPE)
  (CL-WL::DEBUG-LOG! "Event: ~a~%" "target")
  (LET ((ARG-LIST (FOREIGN-ALLOC '(:UNION WL_ARGUMENT) :COUNT 1)))
    (SETF (FOREIGN-SLOT-VALUE
           (MEM-AREF ARG-LIST '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 0)
           '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 'WL-FFI::S)
            MIME_TYPE)
    (RESOURCE-POST-EVENT-ARRAY (WL_DATA_SOURCE-PTR DISPATCH) 0
     ARG-LIST)))

(DEFMETHOD SEND-SEND
           ((DISPATCH DISPATCH) MIME_TYPE FD)
  (CL-WL::DEBUG-LOG! "Event: ~a~%" "send")
  (LET ((ARG-LIST (FOREIGN-ALLOC '(:UNION WL_ARGUMENT) :COUNT 2)))
    (SETF (FOREIGN-SLOT-VALUE
           (MEM-AREF ARG-LIST '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 0)
           '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 'WL-FFI::S)
            MIME_TYPE)
    (SETF (FOREIGN-SLOT-VALUE
           (MEM-AREF ARG-LIST '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 1)
           '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 'WL-FFI::H)
            FD)
    (RESOURCE-POST-EVENT-ARRAY (WL_DATA_SOURCE-PTR DISPATCH) 1
     ARG-LIST)))

(DEFMETHOD SEND-CANCELLED ((DISPATCH DISPATCH))
  (CL-WL::DEBUG-LOG! "Event: ~a~%" "cancelled")
  (LET ((ARG-LIST (FOREIGN-ALLOC '(:UNION WL_ARGUMENT) :COUNT 0)))
    (RESOURCE-POST-EVENT-ARRAY (WL_DATA_SOURCE-PTR DISPATCH) 2
     ARG-LIST)))

(DEFMETHOD SEND-DND-DROP-PERFORMED ((DISPATCH DISPATCH))
  (CL-WL::DEBUG-LOG! "Event: ~a~%" "dnd_drop_performed")
  (LET ((ARG-LIST (FOREIGN-ALLOC '(:UNION WL_ARGUMENT) :COUNT 0)))
    (RESOURCE-POST-EVENT-ARRAY (WL_DATA_SOURCE-PTR DISPATCH) 3
     ARG-LIST)))

(DEFMETHOD SEND-DND-FINISHED ((DISPATCH DISPATCH))
  (CL-WL::DEBUG-LOG! "Event: ~a~%" "dnd_finished")
  (LET ((ARG-LIST (FOREIGN-ALLOC '(:UNION WL_ARGUMENT) :COUNT 0)))
    (RESOURCE-POST-EVENT-ARRAY (WL_DATA_SOURCE-PTR DISPATCH) 4
     ARG-LIST)))

(DEFMETHOD SEND-ACTION ((DISPATCH DISPATCH) DND_ACTION)
  (CL-WL::DEBUG-LOG! "Event: ~a~%" "action")
  (LET ((ARG-LIST (FOREIGN-ALLOC '(:UNION WL_ARGUMENT) :COUNT 1)))
    (SETF (FOREIGN-SLOT-VALUE
           (MEM-AREF ARG-LIST '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 0)
           '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 'WL-FFI::U)
            (WL-DATA-DEVICE-MANAGER::DND-ACTION-TO-VALUE
             DND_ACTION))
    (RESOURCE-POST-EVENT-ARRAY (WL_DATA_SOURCE-PTR DISPATCH) 5
     ARG-LIST)))

(DEFUN ERROR-FROM-VALUE (NUMBER)
  (LOOP FOR ENTRY IN '((0 :INVALID-ACTION-MASK)
                       (1 :INVALID-SOURCE))
        FOR VALUE = (CAR ENTRY)
        FOR KEYWORD = (CADR ENTRY)
        WHEN (EQ NUMBER VALUE) RETURN KEYWORD
        FINALLY (ERROR (FORMAT NIL "Unknown enum value: ~a" NUMBER))))

(DEFUN ERROR-TO-VALUE (KEY)
  (LOOP FOR ENTRY IN '((0 :INVALID-ACTION-MASK)
                       (1 :INVALID-SOURCE))
        FOR VALUE = (CAR ENTRY)
        FOR KEYWORD = (CADR ENTRY)
        WHEN (EQ KEY KEYWORD) RETURN VALUE
        FINALLY (ERROR (FORMAT NIL "Unknown enum keyword: ~a" KEY))))

(DEFCLASS GLOBAL (CL-WL:GLOBAL) NIL
          (:DEFAULT-INITARGS :VERSION 3 :DISPATCH-IMPL 'DISPATCH)
          (:DOCUMENTATION "offer to transfer data

The wl_data_source object is the source side of a wl_data_offer.
      It is created by the source client in a data transfer and
      provides a way to describe the offered data and a way to respond
      to requests to transfer the data.
"))

(DEFMETHOD DISPATCH-BIND ((GLOBAL GLOBAL) CLIENT DATA VERSION ID)
  "Default bind implementation for the wl_data_source global object.
This can be overriden by inheritance in case if custom behaviour is required."
  (CL-WL::DEBUG-LOG! "Binding ~a~%" "wl_data_source")
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
  (CL-WL::DEBUG-LOG! "Initializing global object: ~a~%" "wl_data_source")
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

(IN-PACKAGE :WL_DATA_DEVICE)

(DEFCLASS DISPATCH (CL-WL:OBJECT)
          ((WL_DATA_DEVICE-ID :INITFORM NIL :ACCESSOR
            WL_DATA_DEVICE-ID)
           (WL_DATA_DEVICE-PTR :INITFORM NIL :ACCESSOR
            WL_DATA_DEVICE-PTR))
          (:DEFAULT-INITARGS :VERSION 3)
          (:DOCUMENTATION "data transfer device

There is one wl_data_device per seat which can be obtained
      from the global wl_data_device_manager singleton.

      A wl_data_device provides access to inter-client data transfer
      mechanisms such as copy-and-paste and drag-and-drop.
"))

(DEFGENERIC START-DRAG
    (RESOURCE SOURCE ORIGIN ICON
     SERIAL))

(DEFGENERIC SET-SELECTION
    (RESOURCE SOURCE SERIAL))

(DEFGENERIC RELEASE
    (RESOURCE))

(DEFMETHOD CL-WL:DESTROY ((DISPATCH DISPATCH))
  (CL-WL::DEBUG-LOG! "Destroying dispatch object: ~a~%" "wl_data_device")
  (WHEN (CL-WL::DESTROY-CALLBACK DISPATCH)
    (LOOP FOR CALLBACK IN (CL-WL::DESTROY-CALLBACK DISPATCH)
          DO (FUNCALL CALLBACK DISPATCH)))
  (LET ((RESOURCE-PTR (WL_DATA_DEVICE-PTR DISPATCH)))
    (CL-WL::REMOVE-RESOURCE (POINTER-ADDRESS RESOURCE-PTR)))
  (CL-WL::DN-IF DISPATCH))

(PUSHNEW
 (LIST "wl_data_device" (LIST "wl_data_offer" "wl_data_source" "wl_surface")
       (LAMBDA ()
         (SETF *INTERFACE* (FOREIGN-ALLOC '(:STRUCT INTERFACE)))
         (LET ((REQUESTS-PTR
                (LET ((MESSAGES
                       (CFFI:FOREIGN-ALLOC '(:STRUCT CL-WL.FFI:WL_MESSAGE)
                                           :COUNT 3)))
                  (LET ((INTERFACE-ARRAY
                         (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID))
                                             :COUNT 4))
                        (MSG-PTR
                         (MEM-APTR MESSAGES '(:STRUCT CL-WL.FFI:WL_MESSAGE) 0)))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 0)
                            WL_DATA_SOURCE::*INTERFACE*)
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 1)
                            WL_SURFACE::*INTERFACE*)
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 2)
                            WL_SURFACE::*INTERFACE*)
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 3) (NULL-POINTER))
                    (WITH-FOREIGN-SLOTS
                     ((NAME SIGNATURE TYPES) MSG-PTR
                      (:STRUCT CL-WL.FFI:WL_MESSAGE))
                     (SETF NAME (FOREIGN-STRING-ALLOC "start_drag")
                           SIGNATURE "?oo?ou"
                           TYPES INTERFACE-ARRAY)))
                  (LET ((INTERFACE-ARRAY
                         (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID))
                                             :COUNT 2))
                        (MSG-PTR
                         (MEM-APTR MESSAGES '(:STRUCT CL-WL.FFI:WL_MESSAGE) 1)))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 0)
                            WL_DATA_SOURCE::*INTERFACE*)
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 1) (NULL-POINTER))
                    (WITH-FOREIGN-SLOTS
                     ((NAME SIGNATURE TYPES) MSG-PTR
                      (:STRUCT CL-WL.FFI:WL_MESSAGE))
                     (SETF NAME (FOREIGN-STRING-ALLOC "set_selection")
                           SIGNATURE "?ou"
                           TYPES INTERFACE-ARRAY)))
                  (LET ((INTERFACE-ARRAY
                         (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID))
                                             :COUNT 0))
                        (MSG-PTR
                         (MEM-APTR MESSAGES '(:STRUCT CL-WL.FFI:WL_MESSAGE) 2)))
                    (WITH-FOREIGN-SLOTS
                     ((NAME SIGNATURE TYPES) MSG-PTR
                      (:STRUCT CL-WL.FFI:WL_MESSAGE))
                     (SETF NAME (FOREIGN-STRING-ALLOC "release")
                           SIGNATURE "2"
                           TYPES INTERFACE-ARRAY)))
                  MESSAGES))
               (EVENTS-PTR
                (LET ((MESSAGES
                       (CFFI:FOREIGN-ALLOC '(:STRUCT CL-WL.FFI:WL_MESSAGE)
                                           :COUNT 6)))
                  (LET ((INTERFACE-ARRAY
                         (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID))
                                             :COUNT 1))
                        (MSG-PTR
                         (MEM-APTR MESSAGES '(:STRUCT CL-WL.FFI:WL_MESSAGE) 0)))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 0)
                            WL_DATA_OFFER::*INTERFACE*)
                    (WITH-FOREIGN-SLOTS
                     ((NAME SIGNATURE TYPES) MSG-PTR
                      (:STRUCT CL-WL.FFI:WL_MESSAGE))
                     (SETF NAME (FOREIGN-STRING-ALLOC "data_offer")
                           SIGNATURE "n"
                           TYPES INTERFACE-ARRAY)))
                  (LET ((INTERFACE-ARRAY
                         (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID))
                                             :COUNT 5))
                        (MSG-PTR
                         (MEM-APTR MESSAGES '(:STRUCT CL-WL.FFI:WL_MESSAGE) 1)))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 0) (NULL-POINTER))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 1)
                            WL_SURFACE::*INTERFACE*)
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 2) (NULL-POINTER))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 3) (NULL-POINTER))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 4)
                            WL_DATA_OFFER::*INTERFACE*)
                    (WITH-FOREIGN-SLOTS
                     ((NAME SIGNATURE TYPES) MSG-PTR
                      (:STRUCT CL-WL.FFI:WL_MESSAGE))
                     (SETF NAME (FOREIGN-STRING-ALLOC "enter")
                           SIGNATURE "uoff?o"
                           TYPES INTERFACE-ARRAY)))
                  (LET ((INTERFACE-ARRAY
                         (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID))
                                             :COUNT 0))
                        (MSG-PTR
                         (MEM-APTR MESSAGES '(:STRUCT CL-WL.FFI:WL_MESSAGE) 2)))
                    (WITH-FOREIGN-SLOTS
                     ((NAME SIGNATURE TYPES) MSG-PTR
                      (:STRUCT CL-WL.FFI:WL_MESSAGE))
                     (SETF NAME (FOREIGN-STRING-ALLOC "leave")
                           SIGNATURE ""
                           TYPES INTERFACE-ARRAY)))
                  (LET ((INTERFACE-ARRAY
                         (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID))
                                             :COUNT 3))
                        (MSG-PTR
                         (MEM-APTR MESSAGES '(:STRUCT CL-WL.FFI:WL_MESSAGE) 3)))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 0) (NULL-POINTER))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 1) (NULL-POINTER))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 2) (NULL-POINTER))
                    (WITH-FOREIGN-SLOTS
                     ((NAME SIGNATURE TYPES) MSG-PTR
                      (:STRUCT CL-WL.FFI:WL_MESSAGE))
                     (SETF NAME (FOREIGN-STRING-ALLOC "motion")
                           SIGNATURE "uff"
                           TYPES INTERFACE-ARRAY)))
                  (LET ((INTERFACE-ARRAY
                         (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID))
                                             :COUNT 0))
                        (MSG-PTR
                         (MEM-APTR MESSAGES '(:STRUCT CL-WL.FFI:WL_MESSAGE) 4)))
                    (WITH-FOREIGN-SLOTS
                     ((NAME SIGNATURE TYPES) MSG-PTR
                      (:STRUCT CL-WL.FFI:WL_MESSAGE))
                     (SETF NAME (FOREIGN-STRING-ALLOC "drop")
                           SIGNATURE ""
                           TYPES INTERFACE-ARRAY)))
                  (LET ((INTERFACE-ARRAY
                         (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID))
                                             :COUNT 1))
                        (MSG-PTR
                         (MEM-APTR MESSAGES '(:STRUCT CL-WL.FFI:WL_MESSAGE) 5)))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 0)
                            WL_DATA_OFFER::*INTERFACE*)
                    (WITH-FOREIGN-SLOTS
                     ((NAME SIGNATURE TYPES) MSG-PTR
                      (:STRUCT CL-WL.FFI:WL_MESSAGE))
                     (SETF NAME (FOREIGN-STRING-ALLOC "selection")
                           SIGNATURE "?o"
                           TYPES INTERFACE-ARRAY)))
                  MESSAGES)))
           (WITH-FOREIGN-SLOTS
            ((NAME VERSION METHOD_COUNT METHODS EVENT_COUNT EVENTS) *INTERFACE*
             (:STRUCT INTERFACE))
            (SETF NAME (FOREIGN-STRING-ALLOC "wl_data_device")
                  VERSION 3
                  METHOD_COUNT 3
                  METHODS REQUESTS-PTR
                  EVENT_COUNT 6
                  EVENTS EVENTS-PTR)))))
 CL-WL::*INTERFACE-INIT-LIST* :TEST #'CL-WL::INTERFACE-EXISTS-TEST)

(DEFCALLBACK DISPATCHER-FFI :INT
 ((DATA :POINTER) (TARGET :POINTER) (OPCODE :UINT) (MESSAGE :POINTER)
  (ARGS :POINTER))
 (DECLARE (IGNORE DATA MESSAGE))
 (LET ((RESOURCE (GETHASH (POINTER-ADDRESS TARGET) CL-WL::*RESOURCE-TRACKER*)))
   (ECASE OPCODE
     (0
      (CL-WL::DEBUG-LOG! "Dispatching ~a:~a~%" "wl_data_device" "start-drag")
      (FUNCALL 'START-DRAG RESOURCE
               (GETHASH
                (POINTER-ADDRESS
                 (FOREIGN-SLOT-VALUE
                  (MEM-APTR ARGS '(:UNION CL-WL.FFI:WL_ARGUMENT) 0)
                  '(:UNION CL-WL.FFI:WL_ARGUMENT) 'WL-FFI::O))
                CL-WL::*RESOURCE-TRACKER*)
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
               (VALUES
                (FOREIGN-SLOT-VALUE
                 (MEM-APTR ARGS '(:UNION CL-WL.FFI:WL_ARGUMENT) 3)
                 '(:UNION CL-WL.FFI:WL_ARGUMENT) 'WL-FFI::U))))
     (1
      (CL-WL::DEBUG-LOG! "Dispatching ~a:~a~%" "wl_data_device"
                         "set-selection")
      (FUNCALL 'SET-SELECTION RESOURCE
               (GETHASH
                (POINTER-ADDRESS
                 (FOREIGN-SLOT-VALUE
                  (MEM-APTR ARGS '(:UNION CL-WL.FFI:WL_ARGUMENT) 0)
                  '(:UNION CL-WL.FFI:WL_ARGUMENT) 'WL-FFI::O))
                CL-WL::*RESOURCE-TRACKER*)
               (VALUES
                (FOREIGN-SLOT-VALUE
                 (MEM-APTR ARGS '(:UNION CL-WL.FFI:WL_ARGUMENT) 1)
                 '(:UNION CL-WL.FFI:WL_ARGUMENT) 'WL-FFI::U))))
     (2
      (CL-WL::DEBUG-LOG! "Dispatching ~a:~a~%" "wl_data_device" "release")
      (FUNCALL 'RELEASE RESOURCE))))
 0)

(DEFVAR *DISPATCHER* (CALLBACK DISPATCHER-FFI))

(DEFMETHOD SHARED-INITIALIZE :AFTER ((INSTANCE DISPATCH) SLOT-NAMES &KEY ID)
  (DECLARE (IGNORE SLOT-NAMES))
  (UNLESS (WL_DATA_DEVICE-ID INSTANCE)
    (LET* ((RESOURCE
            (CL-WL::CREATE-RESOURCE (CL-WL:PTR (CL-WL:CLIENT INSTANCE))
                                    *INTERFACE* (VERSION INSTANCE)
                                    (OR ID 0))))
      (SETF (CL-WL::TRANSIENT-ID INSTANCE)
              (SETF (WL_DATA_DEVICE-ID INSTANCE)
                      (OR ID (CL-WL.FFI:RESOURCE-GET-ID RESOURCE))))
      (SETF (WL_DATA_DEVICE-PTR INSTANCE) RESOURCE)
      (SETF (GETHASH (POINTER-ADDRESS RESOURCE) CL-WL::*RESOURCE-TRACKER*)
              INSTANCE)
      (RESOURCE-SET-DISPATCHER RESOURCE *DISPATCHER* (NULL-POINTER)
       (NULL-POINTER) (NULL-POINTER)))))

(DEFMETHOD SEND-DATA-OFFER ((DISPATCH DISPATCH) ID)
  (CL-WL::DEBUG-LOG! "Event: ~a~%" "data_offer")
  (LET ((ARG-LIST (FOREIGN-ALLOC '(:UNION WL_ARGUMENT) :COUNT 1)))
    (SETF (FOREIGN-SLOT-VALUE
           (MEM-AREF ARG-LIST '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 0)
           '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 'WL-FFI::N)
            (WL_DATA_OFFER::WL_DATA_OFFER-PTR ID))
    (RESOURCE-POST-EVENT-ARRAY (WL_DATA_DEVICE-PTR DISPATCH) 0
     ARG-LIST)))

(DEFMETHOD SEND-ENTER
           ((DISPATCH DISPATCH) SERIAL SURFACE
            X Y ID)
  (CL-WL::DEBUG-LOG! "Event: ~a~%" "enter")
  (LET ((ARG-LIST (FOREIGN-ALLOC '(:UNION WL_ARGUMENT) :COUNT 5)))
    (SETF (FOREIGN-SLOT-VALUE
           (MEM-AREF ARG-LIST '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 0)
           '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 'WL-FFI::U)
            SERIAL)
    (SETF (FOREIGN-SLOT-VALUE
           (MEM-AREF ARG-LIST '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 1)
           '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 'WL-FFI::O)
            (WL_SURFACE::WL_SURFACE-PTR SURFACE))
    (SETF (FOREIGN-SLOT-VALUE
           (MEM-AREF ARG-LIST '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 2)
           '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 'WL-FFI::F)
            (CL-WL::TO-FIXNUM X))
    (SETF (FOREIGN-SLOT-VALUE
           (MEM-AREF ARG-LIST '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 3)
           '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 'WL-FFI::F)
            (CL-WL::TO-FIXNUM Y))
    (SETF (FOREIGN-SLOT-VALUE
           (MEM-AREF ARG-LIST '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 4)
           '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 'WL-FFI::O)
            (IF ID
                (WL_DATA_OFFER::WL_DATA_OFFER-PTR ID)
                (NULL-POINTER)))
    (RESOURCE-POST-EVENT-ARRAY (WL_DATA_DEVICE-PTR DISPATCH) 1
     ARG-LIST)))

(DEFMETHOD SEND-LEAVE ((DISPATCH DISPATCH))
  (CL-WL::DEBUG-LOG! "Event: ~a~%" "leave")
  (LET ((ARG-LIST (FOREIGN-ALLOC '(:UNION WL_ARGUMENT) :COUNT 0)))
    (RESOURCE-POST-EVENT-ARRAY (WL_DATA_DEVICE-PTR DISPATCH) 2
     ARG-LIST)))

(DEFMETHOD SEND-MOTION
           ((DISPATCH DISPATCH) TIME X Y)
  (CL-WL::DEBUG-LOG! "Event: ~a~%" "motion")
  (LET ((ARG-LIST (FOREIGN-ALLOC '(:UNION WL_ARGUMENT) :COUNT 3)))
    (SETF (FOREIGN-SLOT-VALUE
           (MEM-AREF ARG-LIST '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 0)
           '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 'WL-FFI::U)
            TIME)
    (SETF (FOREIGN-SLOT-VALUE
           (MEM-AREF ARG-LIST '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 1)
           '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 'WL-FFI::F)
            (CL-WL::TO-FIXNUM X))
    (SETF (FOREIGN-SLOT-VALUE
           (MEM-AREF ARG-LIST '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 2)
           '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 'WL-FFI::F)
            (CL-WL::TO-FIXNUM Y))
    (RESOURCE-POST-EVENT-ARRAY (WL_DATA_DEVICE-PTR DISPATCH) 3
     ARG-LIST)))

(DEFMETHOD SEND-DROP ((DISPATCH DISPATCH))
  (CL-WL::DEBUG-LOG! "Event: ~a~%" "drop")
  (LET ((ARG-LIST (FOREIGN-ALLOC '(:UNION WL_ARGUMENT) :COUNT 0)))
    (RESOURCE-POST-EVENT-ARRAY (WL_DATA_DEVICE-PTR DISPATCH) 4
     ARG-LIST)))

(DEFMETHOD SEND-SELECTION ((DISPATCH DISPATCH) ID)
  (CL-WL::DEBUG-LOG! "Event: ~a~%" "selection")
  (LET ((ARG-LIST (FOREIGN-ALLOC '(:UNION WL_ARGUMENT) :COUNT 1)))
    (SETF (FOREIGN-SLOT-VALUE
           (MEM-AREF ARG-LIST '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 0)
           '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 'WL-FFI::O)
            (IF ID
                (WL_DATA_OFFER::WL_DATA_OFFER-PTR ID)
                (NULL-POINTER)))
    (RESOURCE-POST-EVENT-ARRAY (WL_DATA_DEVICE-PTR DISPATCH) 5
     ARG-LIST)))

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
          (:DEFAULT-INITARGS :VERSION 3 :DISPATCH-IMPL 'DISPATCH)
          (:DOCUMENTATION "data transfer device

There is one wl_data_device per seat which can be obtained
      from the global wl_data_device_manager singleton.

      A wl_data_device provides access to inter-client data transfer
      mechanisms such as copy-and-paste and drag-and-drop.
"))

(DEFMETHOD DISPATCH-BIND ((GLOBAL GLOBAL) CLIENT DATA VERSION ID)
  "Default bind implementation for the wl_data_device global object.
This can be overriden by inheritance in case if custom behaviour is required."
  (CL-WL::DEBUG-LOG! "Binding ~a~%" "wl_data_device")
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
  (CL-WL::DEBUG-LOG! "Initializing global object: ~a~%" "wl_data_device")
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

(IN-PACKAGE :WL_DATA_DEVICE_MANAGER)

(DEFCLASS DISPATCH (CL-WL:OBJECT)
          ((WL_DATA_DEVICE_MANAGER-ID :INITFORM NIL :ACCESSOR
            WL_DATA_DEVICE_MANAGER-ID)
           (WL_DATA_DEVICE_MANAGER-PTR :INITFORM NIL :ACCESSOR
            WL_DATA_DEVICE_MANAGER-PTR))
          (:DEFAULT-INITARGS :VERSION 3)
          (:DOCUMENTATION "data transfer interface

The wl_data_device_manager is a singleton global object that
      provides access to inter-client data transfer mechanisms such as
      copy-and-paste and drag-and-drop.  These mechanisms are tied to
      a wl_seat and this interface lets a client get a wl_data_device
      corresponding to a wl_seat.

      Depending on the version bound, the objects created from the bound
      wl_data_device_manager object will have different requirements for
      functioning properly. See wl_data_source.set_actions,
      wl_data_offer.accept and wl_data_offer.finish for details.
"))

(DEFGENERIC CREATE-DATA-SOURCE
    (RESOURCE ID))

(DEFGENERIC GET-DATA-DEVICE
    (RESOURCE ID SEAT))

(DEFMETHOD CL-WL:DESTROY ((DISPATCH DISPATCH))
  (CL-WL::DEBUG-LOG! "Destroying dispatch object: ~a~%"
                     "wl_data_device_manager")
  (WHEN (CL-WL::DESTROY-CALLBACK DISPATCH)
    (LOOP FOR CALLBACK IN (CL-WL::DESTROY-CALLBACK DISPATCH)
          DO (FUNCALL CALLBACK DISPATCH)))
  (LET ((RESOURCE-PTR (WL_DATA_DEVICE_MANAGER-PTR DISPATCH)))
    (CL-WL::REMOVE-RESOURCE (POINTER-ADDRESS RESOURCE-PTR)))
  (CL-WL::DN-IF DISPATCH))

(PUSHNEW
 (LIST "wl_data_device_manager"
       (LIST "wl_data_source" "wl_data_device" "wl_seat")
       (LAMBDA ()
         (SETF *INTERFACE* (FOREIGN-ALLOC '(:STRUCT INTERFACE)))
         (LET ((REQUESTS-PTR
                (LET ((MESSAGES
                       (CFFI:FOREIGN-ALLOC '(:STRUCT CL-WL.FFI:WL_MESSAGE)
                                           :COUNT 2)))
                  (LET ((INTERFACE-ARRAY
                         (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID))
                                             :COUNT 1))
                        (MSG-PTR
                         (MEM-APTR MESSAGES '(:STRUCT CL-WL.FFI:WL_MESSAGE) 0)))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 0)
                            WL_DATA_SOURCE::*INTERFACE*)
                    (WITH-FOREIGN-SLOTS
                     ((NAME SIGNATURE TYPES) MSG-PTR
                      (:STRUCT CL-WL.FFI:WL_MESSAGE))
                     (SETF NAME (FOREIGN-STRING-ALLOC "create_data_source")
                           SIGNATURE "n"
                           TYPES INTERFACE-ARRAY)))
                  (LET ((INTERFACE-ARRAY
                         (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID))
                                             :COUNT 2))
                        (MSG-PTR
                         (MEM-APTR MESSAGES '(:STRUCT CL-WL.FFI:WL_MESSAGE) 1)))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 0)
                            WL_DATA_DEVICE::*INTERFACE*)
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 1)
                            WL_SEAT::*INTERFACE*)
                    (WITH-FOREIGN-SLOTS
                     ((NAME SIGNATURE TYPES) MSG-PTR
                      (:STRUCT CL-WL.FFI:WL_MESSAGE))
                     (SETF NAME (FOREIGN-STRING-ALLOC "get_data_device")
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
            (SETF NAME (FOREIGN-STRING-ALLOC "wl_data_device_manager")
                  VERSION 3
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
      (CL-WL::DEBUG-LOG! "Dispatching ~a:~a~%" "wl_data_device_manager"
                         "create-data-source")
      (FUNCALL 'CREATE-DATA-SOURCE RESOURCE
               (VALUES
                (FOREIGN-SLOT-VALUE
                 (MEM-APTR ARGS '(:UNION CL-WL.FFI:WL_ARGUMENT) 0)
                 '(:UNION CL-WL.FFI:WL_ARGUMENT) 'WL-FFI::N))))
     (1
      (CL-WL::DEBUG-LOG! "Dispatching ~a:~a~%" "wl_data_device_manager"
                         "get-data-device")
      (FUNCALL 'GET-DATA-DEVICE RESOURCE
               (VALUES
                (FOREIGN-SLOT-VALUE
                 (MEM-APTR ARGS '(:UNION CL-WL.FFI:WL_ARGUMENT) 0)
                 '(:UNION CL-WL.FFI:WL_ARGUMENT) 'WL-FFI::N))
               (GETHASH
                (POINTER-ADDRESS
                 (FOREIGN-SLOT-VALUE
                  (MEM-APTR ARGS '(:UNION CL-WL.FFI:WL_ARGUMENT) 1)
                  '(:UNION CL-WL.FFI:WL_ARGUMENT) 'WL-FFI::O))
                CL-WL::*RESOURCE-TRACKER*)))))
 0)

(DEFVAR *DISPATCHER* (CALLBACK DISPATCHER-FFI))

(DEFMETHOD SHARED-INITIALIZE :AFTER ((INSTANCE DISPATCH) SLOT-NAMES &KEY ID)
  (DECLARE (IGNORE SLOT-NAMES))
  (UNLESS (WL_DATA_DEVICE_MANAGER-ID INSTANCE)
    (LET* ((RESOURCE
            (CL-WL::CREATE-RESOURCE (CL-WL:PTR (CL-WL:CLIENT INSTANCE))
                                    *INTERFACE* (VERSION INSTANCE)
                                    (OR ID 0))))
      (SETF (CL-WL::TRANSIENT-ID INSTANCE)
              (SETF (WL_DATA_DEVICE_MANAGER-ID INSTANCE)
                      (OR ID (CL-WL.FFI:RESOURCE-GET-ID RESOURCE))))
      (SETF (WL_DATA_DEVICE_MANAGER-PTR INSTANCE) RESOURCE)
      (SETF (GETHASH (POINTER-ADDRESS RESOURCE) CL-WL::*RESOURCE-TRACKER*)
              INSTANCE)
      (RESOURCE-SET-DISPATCHER RESOURCE *DISPATCHER* (NULL-POINTER)
       (NULL-POINTER) (NULL-POINTER)))))

(DEFUN DND-ACTION-FROM-VALUE (BITS)
  (LOOP FOR ENTRY IN '((0 :NONE) (1 :COPY)
                       (2 :MOVE) (4 :ASK))
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

(DEFUN DND-ACTION-TO-VALUE (KEYWORDS)
  (REDUCE #'+ KEYWORDS :KEY
          (LAMBDA (KEYWORD)
            (OR
             (CADR
              (ASSOC KEYWORD
                     '((:NONE 0) (:COPY 1)
                       (:MOVE 2) (:ASK 4))))
             0))))

(DEFCLASS GLOBAL (CL-WL:GLOBAL) NIL
          (:DEFAULT-INITARGS :VERSION 3 :DISPATCH-IMPL 'DISPATCH)
          (:DOCUMENTATION "data transfer interface

The wl_data_device_manager is a singleton global object that
      provides access to inter-client data transfer mechanisms such as
      copy-and-paste and drag-and-drop.  These mechanisms are tied to
      a wl_seat and this interface lets a client get a wl_data_device
      corresponding to a wl_seat.

      Depending on the version bound, the objects created from the bound
      wl_data_device_manager object will have different requirements for
      functioning properly. See wl_data_source.set_actions,
      wl_data_offer.accept and wl_data_offer.finish for details.
"))

(DEFMETHOD DISPATCH-BIND ((GLOBAL GLOBAL) CLIENT DATA VERSION ID)
  "Default bind implementation for the wl_data_device_manager global object.
This can be overriden by inheritance in case if custom behaviour is required."
  (CL-WL::DEBUG-LOG! "Binding ~a~%" "wl_data_device_manager")
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
                     "wl_data_device_manager")
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

(IN-PACKAGE :WL_SHELL)

(DEFCLASS DISPATCH (CL-WL:OBJECT)
          ((WL_SHELL-ID :INITFORM NIL :ACCESSOR
            WL_SHELL-ID)
           (WL_SHELL-PTR :INITFORM NIL :ACCESSOR
            WL_SHELL-PTR))
          (:DEFAULT-INITARGS :VERSION 1)
          (:DOCUMENTATION "create desktop-style surfaces

This interface is implemented by servers that provide
      desktop-style user interfaces.

      It allows clients to associate a wl_shell_surface with
      a basic surface.

      Note! This protocol is deprecated and not intended for production use.
      For desktop-style user interfaces, use xdg_shell. Compositors and clients
      should not implement this interface.
"))

(DEFGENERIC GET-SHELL-SURFACE
    (RESOURCE ID SURFACE))

(DEFMETHOD CL-WL:DESTROY ((DISPATCH DISPATCH))
  (CL-WL::DEBUG-LOG! "Destroying dispatch object: ~a~%" "wl_shell")
  (WHEN (CL-WL::DESTROY-CALLBACK DISPATCH)
    (LOOP FOR CALLBACK IN (CL-WL::DESTROY-CALLBACK DISPATCH)
          DO (FUNCALL CALLBACK DISPATCH)))
  (LET ((RESOURCE-PTR (WL_SHELL-PTR DISPATCH)))
    (CL-WL::REMOVE-RESOURCE (POINTER-ADDRESS RESOURCE-PTR)))
  (CL-WL::DN-IF DISPATCH))

(PUSHNEW
 (LIST "wl_shell" (LIST "wl_shell_surface" "wl_surface")
       (LAMBDA ()
         (SETF *INTERFACE* (FOREIGN-ALLOC '(:STRUCT INTERFACE)))
         (LET ((REQUESTS-PTR
                (LET ((MESSAGES
                       (CFFI:FOREIGN-ALLOC '(:STRUCT CL-WL.FFI:WL_MESSAGE)
                                           :COUNT 1)))
                  (LET ((INTERFACE-ARRAY
                         (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID))
                                             :COUNT 2))
                        (MSG-PTR
                         (MEM-APTR MESSAGES '(:STRUCT CL-WL.FFI:WL_MESSAGE) 0)))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 0)
                            WL_SHELL_SURFACE::*INTERFACE*)
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 1)
                            WL_SURFACE::*INTERFACE*)
                    (WITH-FOREIGN-SLOTS
                     ((NAME SIGNATURE TYPES) MSG-PTR
                      (:STRUCT CL-WL.FFI:WL_MESSAGE))
                     (SETF NAME (FOREIGN-STRING-ALLOC "get_shell_surface")
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
            (SETF NAME (FOREIGN-STRING-ALLOC "wl_shell")
                  VERSION 1
                  METHOD_COUNT 1
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
      (CL-WL::DEBUG-LOG! "Dispatching ~a:~a~%" "wl_shell" "get-shell-surface")
      (FUNCALL 'GET-SHELL-SURFACE RESOURCE
               (VALUES
                (FOREIGN-SLOT-VALUE
                 (MEM-APTR ARGS '(:UNION CL-WL.FFI:WL_ARGUMENT) 0)
                 '(:UNION CL-WL.FFI:WL_ARGUMENT) 'WL-FFI::N))
               (GETHASH
                (POINTER-ADDRESS
                 (FOREIGN-SLOT-VALUE
                  (MEM-APTR ARGS '(:UNION CL-WL.FFI:WL_ARGUMENT) 1)
                  '(:UNION CL-WL.FFI:WL_ARGUMENT) 'WL-FFI::O))
                CL-WL::*RESOURCE-TRACKER*)))))
 0)

(DEFVAR *DISPATCHER* (CALLBACK DISPATCHER-FFI))

(DEFMETHOD SHARED-INITIALIZE :AFTER ((INSTANCE DISPATCH) SLOT-NAMES &KEY ID)
  (DECLARE (IGNORE SLOT-NAMES))
  (UNLESS (WL_SHELL-ID INSTANCE)
    (LET* ((RESOURCE
            (CL-WL::CREATE-RESOURCE (CL-WL:PTR (CL-WL:CLIENT INSTANCE))
                                    *INTERFACE* (VERSION INSTANCE)
                                    (OR ID 0))))
      (SETF (CL-WL::TRANSIENT-ID INSTANCE)
              (SETF (WL_SHELL-ID INSTANCE)
                      (OR ID (CL-WL.FFI:RESOURCE-GET-ID RESOURCE))))
      (SETF (WL_SHELL-PTR INSTANCE) RESOURCE)
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
          (:DOCUMENTATION "create desktop-style surfaces

This interface is implemented by servers that provide
      desktop-style user interfaces.

      It allows clients to associate a wl_shell_surface with
      a basic surface.

      Note! This protocol is deprecated and not intended for production use.
      For desktop-style user interfaces, use xdg_shell. Compositors and clients
      should not implement this interface.
"))

(DEFMETHOD DISPATCH-BIND ((GLOBAL GLOBAL) CLIENT DATA VERSION ID)
  "Default bind implementation for the wl_shell global object.
This can be overriden by inheritance in case if custom behaviour is required."
  (CL-WL::DEBUG-LOG! "Binding ~a~%" "wl_shell")
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
  (CL-WL::DEBUG-LOG! "Initializing global object: ~a~%" "wl_shell")
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

(IN-PACKAGE :WL_SHELL_SURFACE)

(DEFCLASS DISPATCH (CL-WL:OBJECT)
          ((WL_SHELL_SURFACE-ID :INITFORM NIL :ACCESSOR
            WL_SHELL_SURFACE-ID)
           (WL_SHELL_SURFACE-PTR :INITFORM NIL :ACCESSOR
            WL_SHELL_SURFACE-PTR))
          (:DEFAULT-INITARGS :VERSION 1)
          (:DOCUMENTATION "desktop-style metadata interface

An interface that may be implemented by a wl_surface, for
      implementations that provide a desktop-style user interface.

      It provides requests to treat surfaces like toplevel, fullscreen
      or popup windows, move, resize or maximize them, associate
      metadata like title and class, etc.

      On the server side the object is automatically destroyed when
      the related wl_surface is destroyed. On the client side,
      wl_shell_surface_destroy() must be called before destroying
      the wl_surface object.
"))

(DEFGENERIC PONG
    (RESOURCE SERIAL))

(DEFGENERIC MOVE
    (RESOURCE SEAT SERIAL))

(DEFGENERIC RESIZE
    (RESOURCE SEAT SERIAL EDGES))

(DEFGENERIC SET-TOPLEVEL
    (RESOURCE))

(DEFGENERIC SET-TRANSIENT
    (RESOURCE PARENT X Y FLAGS))

(DEFGENERIC SET-FULLSCREEN
    (RESOURCE METHOD FRAMERATE OUTPUT))

(DEFGENERIC SET-POPUP
    (RESOURCE SEAT SERIAL PARENT
     X Y FLAGS))

(DEFGENERIC SET-MAXIMIZED
    (RESOURCE OUTPUT))

(DEFGENERIC SET-TITLE
    (RESOURCE TITLE))

(DEFGENERIC SET-CLASS
    (RESOURCE CLASS_))

(DEFMETHOD CL-WL:DESTROY ((DISPATCH DISPATCH))
  (CL-WL::DEBUG-LOG! "Destroying dispatch object: ~a~%" "wl_shell_surface")
  (WHEN (CL-WL::DESTROY-CALLBACK DISPATCH)
    (LOOP FOR CALLBACK IN (CL-WL::DESTROY-CALLBACK DISPATCH)
          DO (FUNCALL CALLBACK DISPATCH)))
  (LET ((RESOURCE-PTR (WL_SHELL_SURFACE-PTR DISPATCH)))
    (CL-WL::REMOVE-RESOURCE (POINTER-ADDRESS RESOURCE-PTR)))
  (CL-WL::DN-IF DISPATCH))

(PUSHNEW
 (LIST "wl_shell_surface" (LIST "wl_seat" "wl_surface" "wl_output")
       (LAMBDA ()
         (SETF *INTERFACE* (FOREIGN-ALLOC '(:STRUCT INTERFACE)))
         (LET ((REQUESTS-PTR
                (LET ((MESSAGES
                       (CFFI:FOREIGN-ALLOC '(:STRUCT CL-WL.FFI:WL_MESSAGE)
                                           :COUNT 10)))
                  (LET ((INTERFACE-ARRAY
                         (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID))
                                             :COUNT 1))
                        (MSG-PTR
                         (MEM-APTR MESSAGES '(:STRUCT CL-WL.FFI:WL_MESSAGE) 0)))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 0) (NULL-POINTER))
                    (WITH-FOREIGN-SLOTS
                     ((NAME SIGNATURE TYPES) MSG-PTR
                      (:STRUCT CL-WL.FFI:WL_MESSAGE))
                     (SETF NAME (FOREIGN-STRING-ALLOC "pong")
                           SIGNATURE "u"
                           TYPES INTERFACE-ARRAY)))
                  (LET ((INTERFACE-ARRAY
                         (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID))
                                             :COUNT 2))
                        (MSG-PTR
                         (MEM-APTR MESSAGES '(:STRUCT CL-WL.FFI:WL_MESSAGE) 1)))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 0)
                            WL_SEAT::*INTERFACE*)
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 1) (NULL-POINTER))
                    (WITH-FOREIGN-SLOTS
                     ((NAME SIGNATURE TYPES) MSG-PTR
                      (:STRUCT CL-WL.FFI:WL_MESSAGE))
                     (SETF NAME (FOREIGN-STRING-ALLOC "move")
                           SIGNATURE "ou"
                           TYPES INTERFACE-ARRAY)))
                  (LET ((INTERFACE-ARRAY
                         (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID))
                                             :COUNT 3))
                        (MSG-PTR
                         (MEM-APTR MESSAGES '(:STRUCT CL-WL.FFI:WL_MESSAGE) 2)))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 0)
                            WL_SEAT::*INTERFACE*)
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 1) (NULL-POINTER))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 2) (NULL-POINTER))
                    (WITH-FOREIGN-SLOTS
                     ((NAME SIGNATURE TYPES) MSG-PTR
                      (:STRUCT CL-WL.FFI:WL_MESSAGE))
                     (SETF NAME (FOREIGN-STRING-ALLOC "resize")
                           SIGNATURE "ouu"
                           TYPES INTERFACE-ARRAY)))
                  (LET ((INTERFACE-ARRAY
                         (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID))
                                             :COUNT 0))
                        (MSG-PTR
                         (MEM-APTR MESSAGES '(:STRUCT CL-WL.FFI:WL_MESSAGE) 3)))
                    (WITH-FOREIGN-SLOTS
                     ((NAME SIGNATURE TYPES) MSG-PTR
                      (:STRUCT CL-WL.FFI:WL_MESSAGE))
                     (SETF NAME (FOREIGN-STRING-ALLOC "set_toplevel")
                           SIGNATURE ""
                           TYPES INTERFACE-ARRAY)))
                  (LET ((INTERFACE-ARRAY
                         (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID))
                                             :COUNT 4))
                        (MSG-PTR
                         (MEM-APTR MESSAGES '(:STRUCT CL-WL.FFI:WL_MESSAGE) 4)))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 0)
                            WL_SURFACE::*INTERFACE*)
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 1) (NULL-POINTER))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 2) (NULL-POINTER))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 3) (NULL-POINTER))
                    (WITH-FOREIGN-SLOTS
                     ((NAME SIGNATURE TYPES) MSG-PTR
                      (:STRUCT CL-WL.FFI:WL_MESSAGE))
                     (SETF NAME (FOREIGN-STRING-ALLOC "set_transient")
                           SIGNATURE "oiiu"
                           TYPES INTERFACE-ARRAY)))
                  (LET ((INTERFACE-ARRAY
                         (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID))
                                             :COUNT 3))
                        (MSG-PTR
                         (MEM-APTR MESSAGES '(:STRUCT CL-WL.FFI:WL_MESSAGE) 5)))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 0) (NULL-POINTER))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 1) (NULL-POINTER))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 2)
                            WL_OUTPUT::*INTERFACE*)
                    (WITH-FOREIGN-SLOTS
                     ((NAME SIGNATURE TYPES) MSG-PTR
                      (:STRUCT CL-WL.FFI:WL_MESSAGE))
                     (SETF NAME (FOREIGN-STRING-ALLOC "set_fullscreen")
                           SIGNATURE "uu?o"
                           TYPES INTERFACE-ARRAY)))
                  (LET ((INTERFACE-ARRAY
                         (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID))
                                             :COUNT 6))
                        (MSG-PTR
                         (MEM-APTR MESSAGES '(:STRUCT CL-WL.FFI:WL_MESSAGE) 6)))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 0)
                            WL_SEAT::*INTERFACE*)
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 1) (NULL-POINTER))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 2)
                            WL_SURFACE::*INTERFACE*)
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 3) (NULL-POINTER))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 4) (NULL-POINTER))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 5) (NULL-POINTER))
                    (WITH-FOREIGN-SLOTS
                     ((NAME SIGNATURE TYPES) MSG-PTR
                      (:STRUCT CL-WL.FFI:WL_MESSAGE))
                     (SETF NAME (FOREIGN-STRING-ALLOC "set_popup")
                           SIGNATURE "ouoiiu"
                           TYPES INTERFACE-ARRAY)))
                  (LET ((INTERFACE-ARRAY
                         (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID))
                                             :COUNT 1))
                        (MSG-PTR
                         (MEM-APTR MESSAGES '(:STRUCT CL-WL.FFI:WL_MESSAGE) 7)))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 0)
                            WL_OUTPUT::*INTERFACE*)
                    (WITH-FOREIGN-SLOTS
                     ((NAME SIGNATURE TYPES) MSG-PTR
                      (:STRUCT CL-WL.FFI:WL_MESSAGE))
                     (SETF NAME (FOREIGN-STRING-ALLOC "set_maximized")
                           SIGNATURE "?o"
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
                     (SETF NAME (FOREIGN-STRING-ALLOC "set_title")
                           SIGNATURE "s"
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
                     (SETF NAME (FOREIGN-STRING-ALLOC "set_class")
                           SIGNATURE "s"
                           TYPES INTERFACE-ARRAY)))
                  MESSAGES))
               (EVENTS-PTR
                (LET ((MESSAGES
                       (CFFI:FOREIGN-ALLOC '(:STRUCT CL-WL.FFI:WL_MESSAGE)
                                           :COUNT 3)))
                  (LET ((INTERFACE-ARRAY
                         (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID))
                                             :COUNT 1))
                        (MSG-PTR
                         (MEM-APTR MESSAGES '(:STRUCT CL-WL.FFI:WL_MESSAGE) 0)))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 0) (NULL-POINTER))
                    (WITH-FOREIGN-SLOTS
                     ((NAME SIGNATURE TYPES) MSG-PTR
                      (:STRUCT CL-WL.FFI:WL_MESSAGE))
                     (SETF NAME (FOREIGN-STRING-ALLOC "ping")
                           SIGNATURE "u"
                           TYPES INTERFACE-ARRAY)))
                  (LET ((INTERFACE-ARRAY
                         (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID))
                                             :COUNT 3))
                        (MSG-PTR
                         (MEM-APTR MESSAGES '(:STRUCT CL-WL.FFI:WL_MESSAGE) 1)))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 0) (NULL-POINTER))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 1) (NULL-POINTER))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 2) (NULL-POINTER))
                    (WITH-FOREIGN-SLOTS
                     ((NAME SIGNATURE TYPES) MSG-PTR
                      (:STRUCT CL-WL.FFI:WL_MESSAGE))
                     (SETF NAME (FOREIGN-STRING-ALLOC "configure")
                           SIGNATURE "uii"
                           TYPES INTERFACE-ARRAY)))
                  (LET ((INTERFACE-ARRAY
                         (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID))
                                             :COUNT 0))
                        (MSG-PTR
                         (MEM-APTR MESSAGES '(:STRUCT CL-WL.FFI:WL_MESSAGE) 2)))
                    (WITH-FOREIGN-SLOTS
                     ((NAME SIGNATURE TYPES) MSG-PTR
                      (:STRUCT CL-WL.FFI:WL_MESSAGE))
                     (SETF NAME (FOREIGN-STRING-ALLOC "popup_done")
                           SIGNATURE ""
                           TYPES INTERFACE-ARRAY)))
                  MESSAGES)))
           (WITH-FOREIGN-SLOTS
            ((NAME VERSION METHOD_COUNT METHODS EVENT_COUNT EVENTS) *INTERFACE*
             (:STRUCT INTERFACE))
            (SETF NAME (FOREIGN-STRING-ALLOC "wl_shell_surface")
                  VERSION 1
                  METHOD_COUNT 10
                  METHODS REQUESTS-PTR
                  EVENT_COUNT 3
                  EVENTS EVENTS-PTR)))))
 CL-WL::*INTERFACE-INIT-LIST* :TEST #'CL-WL::INTERFACE-EXISTS-TEST)

(DEFCALLBACK DISPATCHER-FFI :INT
 ((DATA :POINTER) (TARGET :POINTER) (OPCODE :UINT) (MESSAGE :POINTER)
  (ARGS :POINTER))
 (DECLARE (IGNORE DATA MESSAGE))
 (LET ((RESOURCE (GETHASH (POINTER-ADDRESS TARGET) CL-WL::*RESOURCE-TRACKER*)))
   (ECASE OPCODE
     (0
      (CL-WL::DEBUG-LOG! "Dispatching ~a:~a~%" "wl_shell_surface" "pong")
      (FUNCALL 'PONG RESOURCE
               (VALUES
                (FOREIGN-SLOT-VALUE
                 (MEM-APTR ARGS '(:UNION CL-WL.FFI:WL_ARGUMENT) 0)
                 '(:UNION CL-WL.FFI:WL_ARGUMENT) 'WL-FFI::U))))
     (1
      (CL-WL::DEBUG-LOG! "Dispatching ~a:~a~%" "wl_shell_surface" "move")
      (FUNCALL 'MOVE RESOURCE
               (GETHASH
                (POINTER-ADDRESS
                 (FOREIGN-SLOT-VALUE
                  (MEM-APTR ARGS '(:UNION CL-WL.FFI:WL_ARGUMENT) 0)
                  '(:UNION CL-WL.FFI:WL_ARGUMENT) 'WL-FFI::O))
                CL-WL::*RESOURCE-TRACKER*)
               (VALUES
                (FOREIGN-SLOT-VALUE
                 (MEM-APTR ARGS '(:UNION CL-WL.FFI:WL_ARGUMENT) 1)
                 '(:UNION CL-WL.FFI:WL_ARGUMENT) 'WL-FFI::U))))
     (2
      (CL-WL::DEBUG-LOG! "Dispatching ~a:~a~%" "wl_shell_surface" "resize")
      (FUNCALL 'RESIZE RESOURCE
               (GETHASH
                (POINTER-ADDRESS
                 (FOREIGN-SLOT-VALUE
                  (MEM-APTR ARGS '(:UNION CL-WL.FFI:WL_ARGUMENT) 0)
                  '(:UNION CL-WL.FFI:WL_ARGUMENT) 'WL-FFI::O))
                CL-WL::*RESOURCE-TRACKER*)
               (VALUES
                (FOREIGN-SLOT-VALUE
                 (MEM-APTR ARGS '(:UNION CL-WL.FFI:WL_ARGUMENT) 1)
                 '(:UNION CL-WL.FFI:WL_ARGUMENT) 'WL-FFI::U))
               (RESIZE-FROM-VALUE
                (FOREIGN-SLOT-VALUE
                 (MEM-APTR ARGS '(:UNION CL-WL.FFI:WL_ARGUMENT) 2)
                 '(:UNION CL-WL.FFI:WL_ARGUMENT) 'WL-FFI::U))))
     (3
      (CL-WL::DEBUG-LOG! "Dispatching ~a:~a~%" "wl_shell_surface"
                         "set-toplevel")
      (FUNCALL 'SET-TOPLEVEL RESOURCE))
     (4
      (CL-WL::DEBUG-LOG! "Dispatching ~a:~a~%" "wl_shell_surface"
                         "set-transient")
      (FUNCALL 'SET-TRANSIENT RESOURCE
               (GETHASH
                (POINTER-ADDRESS
                 (FOREIGN-SLOT-VALUE
                  (MEM-APTR ARGS '(:UNION CL-WL.FFI:WL_ARGUMENT) 0)
                  '(:UNION CL-WL.FFI:WL_ARGUMENT) 'WL-FFI::O))
                CL-WL::*RESOURCE-TRACKER*)
               (VALUES
                (FOREIGN-SLOT-VALUE
                 (MEM-APTR ARGS '(:UNION CL-WL.FFI:WL_ARGUMENT) 1)
                 '(:UNION CL-WL.FFI:WL_ARGUMENT) 'WL-FFI::I))
               (VALUES
                (FOREIGN-SLOT-VALUE
                 (MEM-APTR ARGS '(:UNION CL-WL.FFI:WL_ARGUMENT) 2)
                 '(:UNION CL-WL.FFI:WL_ARGUMENT) 'WL-FFI::I))
               (TRANSIENT-FROM-VALUE
                (FOREIGN-SLOT-VALUE
                 (MEM-APTR ARGS '(:UNION CL-WL.FFI:WL_ARGUMENT) 3)
                 '(:UNION CL-WL.FFI:WL_ARGUMENT) 'WL-FFI::U))))
     (5
      (CL-WL::DEBUG-LOG! "Dispatching ~a:~a~%" "wl_shell_surface"
                         "set-fullscreen")
      (FUNCALL 'SET-FULLSCREEN RESOURCE
               (FULLSCREEN-METHOD-FROM-VALUE
                (FOREIGN-SLOT-VALUE
                 (MEM-APTR ARGS '(:UNION CL-WL.FFI:WL_ARGUMENT) 0)
                 '(:UNION CL-WL.FFI:WL_ARGUMENT) 'WL-FFI::U))
               (VALUES
                (FOREIGN-SLOT-VALUE
                 (MEM-APTR ARGS '(:UNION CL-WL.FFI:WL_ARGUMENT) 1)
                 '(:UNION CL-WL.FFI:WL_ARGUMENT) 'WL-FFI::U))
               (GETHASH
                (POINTER-ADDRESS
                 (FOREIGN-SLOT-VALUE
                  (MEM-APTR ARGS '(:UNION CL-WL.FFI:WL_ARGUMENT) 2)
                  '(:UNION CL-WL.FFI:WL_ARGUMENT) 'WL-FFI::O))
                CL-WL::*RESOURCE-TRACKER*)))
     (6
      (CL-WL::DEBUG-LOG! "Dispatching ~a:~a~%" "wl_shell_surface" "set-popup")
      (FUNCALL 'SET-POPUP RESOURCE
               (GETHASH
                (POINTER-ADDRESS
                 (FOREIGN-SLOT-VALUE
                  (MEM-APTR ARGS '(:UNION CL-WL.FFI:WL_ARGUMENT) 0)
                  '(:UNION CL-WL.FFI:WL_ARGUMENT) 'WL-FFI::O))
                CL-WL::*RESOURCE-TRACKER*)
               (VALUES
                (FOREIGN-SLOT-VALUE
                 (MEM-APTR ARGS '(:UNION CL-WL.FFI:WL_ARGUMENT) 1)
                 '(:UNION CL-WL.FFI:WL_ARGUMENT) 'WL-FFI::U))
               (GETHASH
                (POINTER-ADDRESS
                 (FOREIGN-SLOT-VALUE
                  (MEM-APTR ARGS '(:UNION CL-WL.FFI:WL_ARGUMENT) 2)
                  '(:UNION CL-WL.FFI:WL_ARGUMENT) 'WL-FFI::O))
                CL-WL::*RESOURCE-TRACKER*)
               (VALUES
                (FOREIGN-SLOT-VALUE
                 (MEM-APTR ARGS '(:UNION CL-WL.FFI:WL_ARGUMENT) 3)
                 '(:UNION CL-WL.FFI:WL_ARGUMENT) 'WL-FFI::I))
               (VALUES
                (FOREIGN-SLOT-VALUE
                 (MEM-APTR ARGS '(:UNION CL-WL.FFI:WL_ARGUMENT) 4)
                 '(:UNION CL-WL.FFI:WL_ARGUMENT) 'WL-FFI::I))
               (TRANSIENT-FROM-VALUE
                (FOREIGN-SLOT-VALUE
                 (MEM-APTR ARGS '(:UNION CL-WL.FFI:WL_ARGUMENT) 5)
                 '(:UNION CL-WL.FFI:WL_ARGUMENT) 'WL-FFI::U))))
     (7
      (CL-WL::DEBUG-LOG! "Dispatching ~a:~a~%" "wl_shell_surface"
                         "set-maximized")
      (FUNCALL 'SET-MAXIMIZED RESOURCE
               (GETHASH
                (POINTER-ADDRESS
                 (FOREIGN-SLOT-VALUE
                  (MEM-APTR ARGS '(:UNION CL-WL.FFI:WL_ARGUMENT) 0)
                  '(:UNION CL-WL.FFI:WL_ARGUMENT) 'WL-FFI::O))
                CL-WL::*RESOURCE-TRACKER*)))
     (8
      (CL-WL::DEBUG-LOG! "Dispatching ~a:~a~%" "wl_shell_surface" "set-title")
      (FUNCALL 'SET-TITLE RESOURCE
               (VALUES
                (FOREIGN-SLOT-VALUE
                 (MEM-APTR ARGS '(:UNION CL-WL.FFI:WL_ARGUMENT) 0)
                 '(:UNION CL-WL.FFI:WL_ARGUMENT) 'WL-FFI::S))))
     (9
      (CL-WL::DEBUG-LOG! "Dispatching ~a:~a~%" "wl_shell_surface" "set-class")
      (FUNCALL 'SET-CLASS RESOURCE
               (VALUES
                (FOREIGN-SLOT-VALUE
                 (MEM-APTR ARGS '(:UNION CL-WL.FFI:WL_ARGUMENT) 0)
                 '(:UNION CL-WL.FFI:WL_ARGUMENT) 'WL-FFI::S))))))
 0)

(DEFVAR *DISPATCHER* (CALLBACK DISPATCHER-FFI))

(DEFMETHOD SHARED-INITIALIZE :AFTER ((INSTANCE DISPATCH) SLOT-NAMES &KEY ID)
  (DECLARE (IGNORE SLOT-NAMES))
  (UNLESS (WL_SHELL_SURFACE-ID INSTANCE)
    (LET* ((RESOURCE
            (CL-WL::CREATE-RESOURCE (CL-WL:PTR (CL-WL:CLIENT INSTANCE))
                                    *INTERFACE* (VERSION INSTANCE)
                                    (OR ID 0))))
      (SETF (CL-WL::TRANSIENT-ID INSTANCE)
              (SETF (WL_SHELL_SURFACE-ID INSTANCE)
                      (OR ID (CL-WL.FFI:RESOURCE-GET-ID RESOURCE))))
      (SETF (WL_SHELL_SURFACE-PTR INSTANCE) RESOURCE)
      (SETF (GETHASH (POINTER-ADDRESS RESOURCE) CL-WL::*RESOURCE-TRACKER*)
              INSTANCE)
      (RESOURCE-SET-DISPATCHER RESOURCE *DISPATCHER* (NULL-POINTER)
       (NULL-POINTER) (NULL-POINTER)))))

(DEFMETHOD SEND-PING ((DISPATCH DISPATCH) SERIAL)
  (CL-WL::DEBUG-LOG! "Event: ~a~%" "ping")
  (LET ((ARG-LIST (FOREIGN-ALLOC '(:UNION WL_ARGUMENT) :COUNT 1)))
    (SETF (FOREIGN-SLOT-VALUE
           (MEM-AREF ARG-LIST '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 0)
           '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 'WL-FFI::U)
            SERIAL)
    (RESOURCE-POST-EVENT-ARRAY (WL_SHELL_SURFACE-PTR DISPATCH) 0
     ARG-LIST)))

(DEFMETHOD SEND-CONFIGURE
           ((DISPATCH DISPATCH) EDGES WIDTH
            HEIGHT)
  (CL-WL::DEBUG-LOG! "Event: ~a~%" "configure")
  (LET ((ARG-LIST (FOREIGN-ALLOC '(:UNION WL_ARGUMENT) :COUNT 3)))
    (SETF (FOREIGN-SLOT-VALUE
           (MEM-AREF ARG-LIST '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 0)
           '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 'WL-FFI::U)
            (RESIZE-TO-VALUE EDGES))
    (SETF (FOREIGN-SLOT-VALUE
           (MEM-AREF ARG-LIST '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 1)
           '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 'WL-FFI::I)
            WIDTH)
    (SETF (FOREIGN-SLOT-VALUE
           (MEM-AREF ARG-LIST '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 2)
           '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 'WL-FFI::I)
            HEIGHT)
    (RESOURCE-POST-EVENT-ARRAY (WL_SHELL_SURFACE-PTR DISPATCH) 1
     ARG-LIST)))

(DEFMETHOD SEND-POPUP-DONE ((DISPATCH DISPATCH))
  (CL-WL::DEBUG-LOG! "Event: ~a~%" "popup_done")
  (LET ((ARG-LIST (FOREIGN-ALLOC '(:UNION WL_ARGUMENT) :COUNT 0)))
    (RESOURCE-POST-EVENT-ARRAY (WL_SHELL_SURFACE-PTR DISPATCH) 2
     ARG-LIST)))

(DEFUN RESIZE-FROM-VALUE (BITS)
  (LOOP FOR ENTRY IN '((0 :NONE) (1 :TOP)
                       (2 :BOTTOM) (4 :LEFT)
                       (5 :TOP-LEFT) (6 :BOTTOM-LEFT)
                       (8 :RIGHT) (9 :TOP-RIGHT)
                       (10 :BOTTOM-RIGHT))
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

(DEFUN RESIZE-TO-VALUE (KEYWORDS)
  (REDUCE #'+ KEYWORDS :KEY
          (LAMBDA (KEYWORD)
            (OR
             (CADR
              (ASSOC KEYWORD
                     '((:NONE 0) (:TOP 1)
                       (:BOTTOM 2) (:LEFT 4)
                       (:TOP-LEFT 5) (:BOTTOM-LEFT 6)
                       (:RIGHT 8) (:TOP-RIGHT 9)
                       (:BOTTOM-RIGHT 10))))
             0))))

(DEFUN TRANSIENT-FROM-VALUE (BITS)
  (LOOP FOR ENTRY IN '((1 :INACTIVE))
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

(DEFUN TRANSIENT-TO-VALUE (KEYWORDS)
  (REDUCE #'+ KEYWORDS :KEY
          (LAMBDA (KEYWORD)
            (OR (CADR (ASSOC KEYWORD '((:INACTIVE 1)))) 0))))

(DEFUN FULLSCREEN-METHOD-FROM-VALUE (NUMBER)
  (LOOP FOR ENTRY IN '((0 :DEFAULT) (1 :SCALE)
                       (2 :DRIVER) (3 :FILL))
        FOR VALUE = (CAR ENTRY)
        FOR KEYWORD = (CADR ENTRY)
        WHEN (EQ NUMBER VALUE) RETURN KEYWORD
        FINALLY (ERROR (FORMAT NIL "Unknown enum value: ~a" NUMBER))))

(DEFUN FULLSCREEN-METHOD-TO-VALUE (KEY)
  (LOOP FOR ENTRY IN '((0 :DEFAULT) (1 :SCALE)
                       (2 :DRIVER) (3 :FILL))
        FOR VALUE = (CAR ENTRY)
        FOR KEYWORD = (CADR ENTRY)
        WHEN (EQ KEY KEYWORD) RETURN VALUE
        FINALLY (ERROR (FORMAT NIL "Unknown enum keyword: ~a" KEY))))

(DEFCLASS GLOBAL (CL-WL:GLOBAL) NIL
          (:DEFAULT-INITARGS :VERSION 1 :DISPATCH-IMPL 'DISPATCH)
          (:DOCUMENTATION "desktop-style metadata interface

An interface that may be implemented by a wl_surface, for
      implementations that provide a desktop-style user interface.

      It provides requests to treat surfaces like toplevel, fullscreen
      or popup windows, move, resize or maximize them, associate
      metadata like title and class, etc.

      On the server side the object is automatically destroyed when
      the related wl_surface is destroyed. On the client side,
      wl_shell_surface_destroy() must be called before destroying
      the wl_surface object.
"))

(DEFMETHOD DISPATCH-BIND ((GLOBAL GLOBAL) CLIENT DATA VERSION ID)
  "Default bind implementation for the wl_shell_surface global object.
This can be overriden by inheritance in case if custom behaviour is required."
  (CL-WL::DEBUG-LOG! "Binding ~a~%" "wl_shell_surface")
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
  (CL-WL::DEBUG-LOG! "Initializing global object: ~a~%" "wl_shell_surface")
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

(IN-PACKAGE :WL_SURFACE)

(DEFCLASS DISPATCH (CL-WL:OBJECT)
          ((WL_SURFACE-ID :INITFORM NIL :ACCESSOR
            WL_SURFACE-ID)
           (WL_SURFACE-PTR :INITFORM NIL :ACCESSOR
            WL_SURFACE-PTR))
          (:DEFAULT-INITARGS :VERSION 6)
          (:DOCUMENTATION "an onscreen surface

A surface is a rectangular area that may be displayed on zero
      or more outputs, and shown any number of times at the compositor's
      discretion. They can present wl_buffers, receive user input, and
      define a local coordinate system.

      The size of a surface (and relative positions on it) is described
      in surface-local coordinates, which may differ from the buffer
      coordinates of the pixel content, in case a buffer_transform
      or a buffer_scale is used.

      A surface without a \"role\" is fairly useless: a compositor does
      not know where, when or how to present it. The role is the
      purpose of a wl_surface. Examples of roles are a cursor for a
      pointer (as set by wl_pointer.set_cursor), a drag icon
      (wl_data_device.start_drag), a sub-surface
      (wl_subcompositor.get_subsurface), and a window as defined by a
      shell protocol (e.g. wl_shell.get_shell_surface).

      A surface can have only one role at a time. Initially a
      wl_surface does not have a role. Once a wl_surface is given a
      role, it is set permanently for the whole lifetime of the
      wl_surface object. Giving the current role again is allowed,
      unless explicitly forbidden by the relevant interface
      specification.

      Surface roles are given by requests in other interfaces such as
      wl_pointer.set_cursor. The request should explicitly mention
      that this request gives a role to a wl_surface. Often, this
      request also creates a new protocol object that represents the
      role and adds additional functionality to wl_surface. When a
      client wants to destroy a wl_surface, they must destroy this role
      object before the wl_surface, otherwise a defunct_role_object error is
      sent.

      Destroying the role object does not remove the role from the
      wl_surface, but it may stop the wl_surface from \"playing the role\".
      For instance, if a wl_subsurface object is destroyed, the wl_surface
      it was created for will be unmapped and forget its position and
      z-order. It is allowed to create a wl_subsurface for the same
      wl_surface again, but it is not allowed to use the wl_surface as
      a cursor (cursor is a different role than sub-surface, and role
      switching is not allowed).
"))

(DEFGENERIC DESTROY
    (RESOURCE))

(DEFGENERIC ATTACH
    (RESOURCE BUFFER X Y))

(DEFGENERIC DAMAGE
    (RESOURCE X Y WIDTH HEIGHT))

(DEFGENERIC FRAME
    (RESOURCE CALLBACK))

(DEFGENERIC SET-OPAQUE-REGION
    (RESOURCE REGION))

(DEFGENERIC SET-INPUT-REGION
    (RESOURCE REGION))

(DEFGENERIC COMMIT
    (RESOURCE))

(DEFGENERIC SET-BUFFER-TRANSFORM
    (RESOURCE TRANSFORM))

(DEFGENERIC SET-BUFFER-SCALE
    (RESOURCE SCALE))

(DEFGENERIC DAMAGE-BUFFER
    (RESOURCE X Y WIDTH HEIGHT))

(DEFGENERIC OFFSET
    (RESOURCE X Y))

(DEFMETHOD CL-WL:DESTROY ((DISPATCH DISPATCH))
  (CL-WL::DEBUG-LOG! "Destroying dispatch object: ~a~%" "wl_surface")
  (WHEN (CL-WL::DESTROY-CALLBACK DISPATCH)
    (LOOP FOR CALLBACK IN (CL-WL::DESTROY-CALLBACK DISPATCH)
          DO (FUNCALL CALLBACK DISPATCH)))
  (LET ((RESOURCE-PTR (WL_SURFACE-PTR DISPATCH)))
    (CL-WL::REMOVE-RESOURCE (POINTER-ADDRESS RESOURCE-PTR)))
  (CL-WL::DN-IF DISPATCH))

(DEFMETHOD DESTROY ((DISPATCH DISPATCH)) (CL-WL:DESTROY DISPATCH))

(DEFMETHOD DESTROY (EMPTY) NIL)

(PUSHNEW
 (LIST "wl_surface" (LIST "wl_output" "wl_buffer" "wl_callback" "wl_region")
       (LAMBDA ()
         (SETF *INTERFACE* (FOREIGN-ALLOC '(:STRUCT INTERFACE)))
         (LET ((REQUESTS-PTR
                (LET ((MESSAGES
                       (CFFI:FOREIGN-ALLOC '(:STRUCT CL-WL.FFI:WL_MESSAGE)
                                           :COUNT 11)))
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
                            WL_BUFFER::*INTERFACE*)
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 1) (NULL-POINTER))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 2) (NULL-POINTER))
                    (WITH-FOREIGN-SLOTS
                     ((NAME SIGNATURE TYPES) MSG-PTR
                      (:STRUCT CL-WL.FFI:WL_MESSAGE))
                     (SETF NAME (FOREIGN-STRING-ALLOC "attach")
                           SIGNATURE "?oii"
                           TYPES INTERFACE-ARRAY)))
                  (LET ((INTERFACE-ARRAY
                         (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID))
                                             :COUNT 4))
                        (MSG-PTR
                         (MEM-APTR MESSAGES '(:STRUCT CL-WL.FFI:WL_MESSAGE) 2)))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 0) (NULL-POINTER))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 1) (NULL-POINTER))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 2) (NULL-POINTER))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 3) (NULL-POINTER))
                    (WITH-FOREIGN-SLOTS
                     ((NAME SIGNATURE TYPES) MSG-PTR
                      (:STRUCT CL-WL.FFI:WL_MESSAGE))
                     (SETF NAME (FOREIGN-STRING-ALLOC "damage")
                           SIGNATURE "iiii"
                           TYPES INTERFACE-ARRAY)))
                  (LET ((INTERFACE-ARRAY
                         (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID))
                                             :COUNT 1))
                        (MSG-PTR
                         (MEM-APTR MESSAGES '(:STRUCT CL-WL.FFI:WL_MESSAGE) 3)))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 0)
                            WL_CALLBACK::*INTERFACE*)
                    (WITH-FOREIGN-SLOTS
                     ((NAME SIGNATURE TYPES) MSG-PTR
                      (:STRUCT CL-WL.FFI:WL_MESSAGE))
                     (SETF NAME (FOREIGN-STRING-ALLOC "frame")
                           SIGNATURE "n"
                           TYPES INTERFACE-ARRAY)))
                  (LET ((INTERFACE-ARRAY
                         (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID))
                                             :COUNT 1))
                        (MSG-PTR
                         (MEM-APTR MESSAGES '(:STRUCT CL-WL.FFI:WL_MESSAGE) 4)))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 0)
                            WL_REGION::*INTERFACE*)
                    (WITH-FOREIGN-SLOTS
                     ((NAME SIGNATURE TYPES) MSG-PTR
                      (:STRUCT CL-WL.FFI:WL_MESSAGE))
                     (SETF NAME (FOREIGN-STRING-ALLOC "set_opaque_region")
                           SIGNATURE "?o"
                           TYPES INTERFACE-ARRAY)))
                  (LET ((INTERFACE-ARRAY
                         (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID))
                                             :COUNT 1))
                        (MSG-PTR
                         (MEM-APTR MESSAGES '(:STRUCT CL-WL.FFI:WL_MESSAGE) 5)))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 0)
                            WL_REGION::*INTERFACE*)
                    (WITH-FOREIGN-SLOTS
                     ((NAME SIGNATURE TYPES) MSG-PTR
                      (:STRUCT CL-WL.FFI:WL_MESSAGE))
                     (SETF NAME (FOREIGN-STRING-ALLOC "set_input_region")
                           SIGNATURE "?o"
                           TYPES INTERFACE-ARRAY)))
                  (LET ((INTERFACE-ARRAY
                         (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID))
                                             :COUNT 0))
                        (MSG-PTR
                         (MEM-APTR MESSAGES '(:STRUCT CL-WL.FFI:WL_MESSAGE) 6)))
                    (WITH-FOREIGN-SLOTS
                     ((NAME SIGNATURE TYPES) MSG-PTR
                      (:STRUCT CL-WL.FFI:WL_MESSAGE))
                     (SETF NAME (FOREIGN-STRING-ALLOC "commit")
                           SIGNATURE ""
                           TYPES INTERFACE-ARRAY)))
                  (LET ((INTERFACE-ARRAY
                         (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID))
                                             :COUNT 1))
                        (MSG-PTR
                         (MEM-APTR MESSAGES '(:STRUCT CL-WL.FFI:WL_MESSAGE) 7)))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 0) (NULL-POINTER))
                    (WITH-FOREIGN-SLOTS
                     ((NAME SIGNATURE TYPES) MSG-PTR
                      (:STRUCT CL-WL.FFI:WL_MESSAGE))
                     (SETF NAME (FOREIGN-STRING-ALLOC "set_buffer_transform")
                           SIGNATURE "2u"
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
                     (SETF NAME (FOREIGN-STRING-ALLOC "set_buffer_scale")
                           SIGNATURE "3i"
                           TYPES INTERFACE-ARRAY)))
                  (LET ((INTERFACE-ARRAY
                         (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID))
                                             :COUNT 4))
                        (MSG-PTR
                         (MEM-APTR MESSAGES '(:STRUCT CL-WL.FFI:WL_MESSAGE) 9)))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 0) (NULL-POINTER))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 1) (NULL-POINTER))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 2) (NULL-POINTER))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 3) (NULL-POINTER))
                    (WITH-FOREIGN-SLOTS
                     ((NAME SIGNATURE TYPES) MSG-PTR
                      (:STRUCT CL-WL.FFI:WL_MESSAGE))
                     (SETF NAME (FOREIGN-STRING-ALLOC "damage_buffer")
                           SIGNATURE "4iiii"
                           TYPES INTERFACE-ARRAY)))
                  (LET ((INTERFACE-ARRAY
                         (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID))
                                             :COUNT 2))
                        (MSG-PTR
                         (MEM-APTR MESSAGES '(:STRUCT CL-WL.FFI:WL_MESSAGE)
                          10)))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 0) (NULL-POINTER))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 1) (NULL-POINTER))
                    (WITH-FOREIGN-SLOTS
                     ((NAME SIGNATURE TYPES) MSG-PTR
                      (:STRUCT CL-WL.FFI:WL_MESSAGE))
                     (SETF NAME (FOREIGN-STRING-ALLOC "offset")
                           SIGNATURE "5ii"
                           TYPES INTERFACE-ARRAY)))
                  MESSAGES))
               (EVENTS-PTR
                (LET ((MESSAGES
                       (CFFI:FOREIGN-ALLOC '(:STRUCT CL-WL.FFI:WL_MESSAGE)
                                           :COUNT 4)))
                  (LET ((INTERFACE-ARRAY
                         (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID))
                                             :COUNT 1))
                        (MSG-PTR
                         (MEM-APTR MESSAGES '(:STRUCT CL-WL.FFI:WL_MESSAGE) 0)))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 0)
                            WL_OUTPUT::*INTERFACE*)
                    (WITH-FOREIGN-SLOTS
                     ((NAME SIGNATURE TYPES) MSG-PTR
                      (:STRUCT CL-WL.FFI:WL_MESSAGE))
                     (SETF NAME (FOREIGN-STRING-ALLOC "enter")
                           SIGNATURE "o"
                           TYPES INTERFACE-ARRAY)))
                  (LET ((INTERFACE-ARRAY
                         (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID))
                                             :COUNT 1))
                        (MSG-PTR
                         (MEM-APTR MESSAGES '(:STRUCT CL-WL.FFI:WL_MESSAGE) 1)))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 0)
                            WL_OUTPUT::*INTERFACE*)
                    (WITH-FOREIGN-SLOTS
                     ((NAME SIGNATURE TYPES) MSG-PTR
                      (:STRUCT CL-WL.FFI:WL_MESSAGE))
                     (SETF NAME (FOREIGN-STRING-ALLOC "leave")
                           SIGNATURE "o"
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
                     (SETF NAME (FOREIGN-STRING-ALLOC "preferred_buffer_scale")
                           SIGNATURE "6i"
                           TYPES INTERFACE-ARRAY)))
                  (LET ((INTERFACE-ARRAY
                         (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID))
                                             :COUNT 1))
                        (MSG-PTR
                         (MEM-APTR MESSAGES '(:STRUCT CL-WL.FFI:WL_MESSAGE) 3)))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 0) (NULL-POINTER))
                    (WITH-FOREIGN-SLOTS
                     ((NAME SIGNATURE TYPES) MSG-PTR
                      (:STRUCT CL-WL.FFI:WL_MESSAGE))
                     (SETF NAME
                             (FOREIGN-STRING-ALLOC
                              "preferred_buffer_transform")
                           SIGNATURE "6u"
                           TYPES INTERFACE-ARRAY)))
                  MESSAGES)))
           (WITH-FOREIGN-SLOTS
            ((NAME VERSION METHOD_COUNT METHODS EVENT_COUNT EVENTS) *INTERFACE*
             (:STRUCT INTERFACE))
            (SETF NAME (FOREIGN-STRING-ALLOC "wl_surface")
                  VERSION 6
                  METHOD_COUNT 11
                  METHODS REQUESTS-PTR
                  EVENT_COUNT 4
                  EVENTS EVENTS-PTR)))))
 CL-WL::*INTERFACE-INIT-LIST* :TEST #'CL-WL::INTERFACE-EXISTS-TEST)

(DEFCALLBACK DISPATCHER-FFI :INT
 ((DATA :POINTER) (TARGET :POINTER) (OPCODE :UINT) (MESSAGE :POINTER)
  (ARGS :POINTER))
 (DECLARE (IGNORE DATA MESSAGE))
 (LET ((RESOURCE (GETHASH (POINTER-ADDRESS TARGET) CL-WL::*RESOURCE-TRACKER*)))
   (ECASE OPCODE
     (0
      (CL-WL::DEBUG-LOG! "Dispatching ~a:~a~%" "wl_surface" "destroy")
      (FUNCALL 'DESTROY RESOURCE))
     (1
      (CL-WL::DEBUG-LOG! "Dispatching ~a:~a~%" "wl_surface" "attach")
      (FUNCALL 'ATTACH RESOURCE
               (GETHASH
                (POINTER-ADDRESS
                 (FOREIGN-SLOT-VALUE
                  (MEM-APTR ARGS '(:UNION CL-WL.FFI:WL_ARGUMENT) 0)
                  '(:UNION CL-WL.FFI:WL_ARGUMENT) 'WL-FFI::O))
                CL-WL::*RESOURCE-TRACKER*)
               (VALUES
                (FOREIGN-SLOT-VALUE
                 (MEM-APTR ARGS '(:UNION CL-WL.FFI:WL_ARGUMENT) 1)
                 '(:UNION CL-WL.FFI:WL_ARGUMENT) 'WL-FFI::I))
               (VALUES
                (FOREIGN-SLOT-VALUE
                 (MEM-APTR ARGS '(:UNION CL-WL.FFI:WL_ARGUMENT) 2)
                 '(:UNION CL-WL.FFI:WL_ARGUMENT) 'WL-FFI::I))))
     (2
      (CL-WL::DEBUG-LOG! "Dispatching ~a:~a~%" "wl_surface" "damage")
      (FUNCALL 'DAMAGE RESOURCE
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
     (3
      (CL-WL::DEBUG-LOG! "Dispatching ~a:~a~%" "wl_surface" "frame")
      (FUNCALL 'FRAME RESOURCE
               (VALUES
                (FOREIGN-SLOT-VALUE
                 (MEM-APTR ARGS '(:UNION CL-WL.FFI:WL_ARGUMENT) 0)
                 '(:UNION CL-WL.FFI:WL_ARGUMENT) 'WL-FFI::N))))
     (4
      (CL-WL::DEBUG-LOG! "Dispatching ~a:~a~%" "wl_surface"
                         "set-opaque-region")
      (FUNCALL 'SET-OPAQUE-REGION RESOURCE
               (GETHASH
                (POINTER-ADDRESS
                 (FOREIGN-SLOT-VALUE
                  (MEM-APTR ARGS '(:UNION CL-WL.FFI:WL_ARGUMENT) 0)
                  '(:UNION CL-WL.FFI:WL_ARGUMENT) 'WL-FFI::O))
                CL-WL::*RESOURCE-TRACKER*)))
     (5
      (CL-WL::DEBUG-LOG! "Dispatching ~a:~a~%" "wl_surface" "set-input-region")
      (FUNCALL 'SET-INPUT-REGION RESOURCE
               (GETHASH
                (POINTER-ADDRESS
                 (FOREIGN-SLOT-VALUE
                  (MEM-APTR ARGS '(:UNION CL-WL.FFI:WL_ARGUMENT) 0)
                  '(:UNION CL-WL.FFI:WL_ARGUMENT) 'WL-FFI::O))
                CL-WL::*RESOURCE-TRACKER*)))
     (6
      (CL-WL::DEBUG-LOG! "Dispatching ~a:~a~%" "wl_surface" "commit")
      (FUNCALL 'COMMIT RESOURCE))
     (7
      (CL-WL::DEBUG-LOG! "Dispatching ~a:~a~%" "wl_surface"
                         "set-buffer-transform")
      (FUNCALL 'SET-BUFFER-TRANSFORM RESOURCE
               (WL-OUTPUT::TRANSFORM-FROM-VALUE
                (FOREIGN-SLOT-VALUE
                 (MEM-APTR ARGS '(:UNION CL-WL.FFI:WL_ARGUMENT) 0)
                 '(:UNION CL-WL.FFI:WL_ARGUMENT) 'WL-FFI::U))))
     (8
      (CL-WL::DEBUG-LOG! "Dispatching ~a:~a~%" "wl_surface" "set-buffer-scale")
      (FUNCALL 'SET-BUFFER-SCALE RESOURCE
               (VALUES
                (FOREIGN-SLOT-VALUE
                 (MEM-APTR ARGS '(:UNION CL-WL.FFI:WL_ARGUMENT) 0)
                 '(:UNION CL-WL.FFI:WL_ARGUMENT) 'WL-FFI::I))))
     (9
      (CL-WL::DEBUG-LOG! "Dispatching ~a:~a~%" "wl_surface" "damage-buffer")
      (FUNCALL 'DAMAGE-BUFFER RESOURCE
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
     (10
      (CL-WL::DEBUG-LOG! "Dispatching ~a:~a~%" "wl_surface" "offset")
      (FUNCALL 'OFFSET RESOURCE
               (VALUES
                (FOREIGN-SLOT-VALUE
                 (MEM-APTR ARGS '(:UNION CL-WL.FFI:WL_ARGUMENT) 0)
                 '(:UNION CL-WL.FFI:WL_ARGUMENT) 'WL-FFI::I))
               (VALUES
                (FOREIGN-SLOT-VALUE
                 (MEM-APTR ARGS '(:UNION CL-WL.FFI:WL_ARGUMENT) 1)
                 '(:UNION CL-WL.FFI:WL_ARGUMENT) 'WL-FFI::I))))))
 0)

(DEFVAR *DISPATCHER* (CALLBACK DISPATCHER-FFI))

(DEFMETHOD SHARED-INITIALIZE :AFTER ((INSTANCE DISPATCH) SLOT-NAMES &KEY ID)
  (DECLARE (IGNORE SLOT-NAMES))
  (UNLESS (WL_SURFACE-ID INSTANCE)
    (LET* ((RESOURCE
            (CL-WL::CREATE-RESOURCE (CL-WL:PTR (CL-WL:CLIENT INSTANCE))
                                    *INTERFACE* (VERSION INSTANCE)
                                    (OR ID 0))))
      (SETF (CL-WL::TRANSIENT-ID INSTANCE)
              (SETF (WL_SURFACE-ID INSTANCE)
                      (OR ID (CL-WL.FFI:RESOURCE-GET-ID RESOURCE))))
      (SETF (WL_SURFACE-PTR INSTANCE) RESOURCE)
      (SETF (GETHASH (POINTER-ADDRESS RESOURCE) CL-WL::*RESOURCE-TRACKER*)
              INSTANCE)
      (RESOURCE-SET-DISPATCHER RESOURCE *DISPATCHER* (NULL-POINTER)
       (NULL-POINTER) (NULL-POINTER)))))

(DEFMETHOD SEND-ENTER ((DISPATCH DISPATCH) OUTPUT)
  (CL-WL::DEBUG-LOG! "Event: ~a~%" "enter")
  (LET ((ARG-LIST (FOREIGN-ALLOC '(:UNION WL_ARGUMENT) :COUNT 1)))
    (SETF (FOREIGN-SLOT-VALUE
           (MEM-AREF ARG-LIST '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 0)
           '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 'WL-FFI::O)
            (WL_OUTPUT::WL_OUTPUT-PTR OUTPUT))
    (RESOURCE-POST-EVENT-ARRAY (WL_SURFACE-PTR DISPATCH) 0
     ARG-LIST)))

(DEFMETHOD SEND-LEAVE ((DISPATCH DISPATCH) OUTPUT)
  (CL-WL::DEBUG-LOG! "Event: ~a~%" "leave")
  (LET ((ARG-LIST (FOREIGN-ALLOC '(:UNION WL_ARGUMENT) :COUNT 1)))
    (SETF (FOREIGN-SLOT-VALUE
           (MEM-AREF ARG-LIST '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 0)
           '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 'WL-FFI::O)
            (WL_OUTPUT::WL_OUTPUT-PTR OUTPUT))
    (RESOURCE-POST-EVENT-ARRAY (WL_SURFACE-PTR DISPATCH) 1
     ARG-LIST)))

(DEFMETHOD SEND-PREFERRED-BUFFER-SCALE
           ((DISPATCH DISPATCH) FACTOR)
  (CL-WL::DEBUG-LOG! "Event: ~a~%" "preferred_buffer_scale")
  (LET ((ARG-LIST (FOREIGN-ALLOC '(:UNION WL_ARGUMENT) :COUNT 1)))
    (SETF (FOREIGN-SLOT-VALUE
           (MEM-AREF ARG-LIST '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 0)
           '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 'WL-FFI::I)
            FACTOR)
    (RESOURCE-POST-EVENT-ARRAY (WL_SURFACE-PTR DISPATCH) 2
     ARG-LIST)))

(DEFMETHOD SEND-PREFERRED-BUFFER-TRANSFORM
           ((DISPATCH DISPATCH) TRANSFORM)
  (CL-WL::DEBUG-LOG! "Event: ~a~%" "preferred_buffer_transform")
  (LET ((ARG-LIST (FOREIGN-ALLOC '(:UNION WL_ARGUMENT) :COUNT 1)))
    (SETF (FOREIGN-SLOT-VALUE
           (MEM-AREF ARG-LIST '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 0)
           '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 'WL-FFI::U)
            (WL-OUTPUT::TRANSFORM-TO-VALUE TRANSFORM))
    (RESOURCE-POST-EVENT-ARRAY (WL_SURFACE-PTR DISPATCH) 3
     ARG-LIST)))

(DEFUN ERROR-FROM-VALUE (NUMBER)
  (LOOP FOR ENTRY IN '((0 :INVALID-SCALE)
                       (1 :INVALID-TRANSFORM)
                       (2 :INVALID-SIZE)
                       (3 :INVALID-OFFSET)
                       (4 :DEFUNCT-ROLE-OBJECT))
        FOR VALUE = (CAR ENTRY)
        FOR KEYWORD = (CADR ENTRY)
        WHEN (EQ NUMBER VALUE) RETURN KEYWORD
        FINALLY (ERROR (FORMAT NIL "Unknown enum value: ~a" NUMBER))))

(DEFUN ERROR-TO-VALUE (KEY)
  (LOOP FOR ENTRY IN '((0 :INVALID-SCALE)
                       (1 :INVALID-TRANSFORM)
                       (2 :INVALID-SIZE)
                       (3 :INVALID-OFFSET)
                       (4 :DEFUNCT-ROLE-OBJECT))
        FOR VALUE = (CAR ENTRY)
        FOR KEYWORD = (CADR ENTRY)
        WHEN (EQ KEY KEYWORD) RETURN VALUE
        FINALLY (ERROR (FORMAT NIL "Unknown enum keyword: ~a" KEY))))

(DEFCLASS GLOBAL (CL-WL:GLOBAL) NIL
          (:DEFAULT-INITARGS :VERSION 6 :DISPATCH-IMPL 'DISPATCH)
          (:DOCUMENTATION "an onscreen surface

A surface is a rectangular area that may be displayed on zero
      or more outputs, and shown any number of times at the compositor's
      discretion. They can present wl_buffers, receive user input, and
      define a local coordinate system.

      The size of a surface (and relative positions on it) is described
      in surface-local coordinates, which may differ from the buffer
      coordinates of the pixel content, in case a buffer_transform
      or a buffer_scale is used.

      A surface without a \"role\" is fairly useless: a compositor does
      not know where, when or how to present it. The role is the
      purpose of a wl_surface. Examples of roles are a cursor for a
      pointer (as set by wl_pointer.set_cursor), a drag icon
      (wl_data_device.start_drag), a sub-surface
      (wl_subcompositor.get_subsurface), and a window as defined by a
      shell protocol (e.g. wl_shell.get_shell_surface).

      A surface can have only one role at a time. Initially a
      wl_surface does not have a role. Once a wl_surface is given a
      role, it is set permanently for the whole lifetime of the
      wl_surface object. Giving the current role again is allowed,
      unless explicitly forbidden by the relevant interface
      specification.

      Surface roles are given by requests in other interfaces such as
      wl_pointer.set_cursor. The request should explicitly mention
      that this request gives a role to a wl_surface. Often, this
      request also creates a new protocol object that represents the
      role and adds additional functionality to wl_surface. When a
      client wants to destroy a wl_surface, they must destroy this role
      object before the wl_surface, otherwise a defunct_role_object error is
      sent.

      Destroying the role object does not remove the role from the
      wl_surface, but it may stop the wl_surface from \"playing the role\".
      For instance, if a wl_subsurface object is destroyed, the wl_surface
      it was created for will be unmapped and forget its position and
      z-order. It is allowed to create a wl_subsurface for the same
      wl_surface again, but it is not allowed to use the wl_surface as
      a cursor (cursor is a different role than sub-surface, and role
      switching is not allowed).
"))

(DEFMETHOD DISPATCH-BIND ((GLOBAL GLOBAL) CLIENT DATA VERSION ID)
  "Default bind implementation for the wl_surface global object.
This can be overriden by inheritance in case if custom behaviour is required."
  (CL-WL::DEBUG-LOG! "Binding ~a~%" "wl_surface")
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
  (CL-WL::DEBUG-LOG! "Initializing global object: ~a~%" "wl_surface")
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

(IN-PACKAGE :WL_SEAT)

(DEFCLASS DISPATCH (CL-WL:OBJECT)
          ((WL_SEAT-ID :INITFORM NIL :ACCESSOR
            WL_SEAT-ID)
           (WL_SEAT-PTR :INITFORM NIL :ACCESSOR
            WL_SEAT-PTR))
          (:DEFAULT-INITARGS :VERSION 9)
          (:DOCUMENTATION "group of input devices

A seat is a group of keyboards, pointer and touch devices. This
      object is published as a global during start up, or when such a
      device is hot plugged.  A seat typically has a pointer and
      maintains a keyboard focus and a pointer focus.
"))

(DEFGENERIC GET-POINTER
    (RESOURCE ID))

(DEFGENERIC GET-KEYBOARD
    (RESOURCE ID))

(DEFGENERIC GET-TOUCH
    (RESOURCE ID))

(DEFGENERIC RELEASE
    (RESOURCE))

(DEFMETHOD CL-WL:DESTROY ((DISPATCH DISPATCH))
  (CL-WL::DEBUG-LOG! "Destroying dispatch object: ~a~%" "wl_seat")
  (WHEN (CL-WL::DESTROY-CALLBACK DISPATCH)
    (LOOP FOR CALLBACK IN (CL-WL::DESTROY-CALLBACK DISPATCH)
          DO (FUNCALL CALLBACK DISPATCH)))
  (LET ((RESOURCE-PTR (WL_SEAT-PTR DISPATCH)))
    (CL-WL::REMOVE-RESOURCE (POINTER-ADDRESS RESOURCE-PTR)))
  (CL-WL::DN-IF DISPATCH))

(PUSHNEW
 (LIST "wl_seat" (LIST "wl_pointer" "wl_keyboard" "wl_touch")
       (LAMBDA ()
         (SETF *INTERFACE* (FOREIGN-ALLOC '(:STRUCT INTERFACE)))
         (LET ((REQUESTS-PTR
                (LET ((MESSAGES
                       (CFFI:FOREIGN-ALLOC '(:STRUCT CL-WL.FFI:WL_MESSAGE)
                                           :COUNT 4)))
                  (LET ((INTERFACE-ARRAY
                         (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID))
                                             :COUNT 1))
                        (MSG-PTR
                         (MEM-APTR MESSAGES '(:STRUCT CL-WL.FFI:WL_MESSAGE) 0)))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 0)
                            WL_POINTER::*INTERFACE*)
                    (WITH-FOREIGN-SLOTS
                     ((NAME SIGNATURE TYPES) MSG-PTR
                      (:STRUCT CL-WL.FFI:WL_MESSAGE))
                     (SETF NAME (FOREIGN-STRING-ALLOC "get_pointer")
                           SIGNATURE "n"
                           TYPES INTERFACE-ARRAY)))
                  (LET ((INTERFACE-ARRAY
                         (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID))
                                             :COUNT 1))
                        (MSG-PTR
                         (MEM-APTR MESSAGES '(:STRUCT CL-WL.FFI:WL_MESSAGE) 1)))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 0)
                            WL_KEYBOARD::*INTERFACE*)
                    (WITH-FOREIGN-SLOTS
                     ((NAME SIGNATURE TYPES) MSG-PTR
                      (:STRUCT CL-WL.FFI:WL_MESSAGE))
                     (SETF NAME (FOREIGN-STRING-ALLOC "get_keyboard")
                           SIGNATURE "n"
                           TYPES INTERFACE-ARRAY)))
                  (LET ((INTERFACE-ARRAY
                         (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID))
                                             :COUNT 1))
                        (MSG-PTR
                         (MEM-APTR MESSAGES '(:STRUCT CL-WL.FFI:WL_MESSAGE) 2)))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 0)
                            WL_TOUCH::*INTERFACE*)
                    (WITH-FOREIGN-SLOTS
                     ((NAME SIGNATURE TYPES) MSG-PTR
                      (:STRUCT CL-WL.FFI:WL_MESSAGE))
                     (SETF NAME (FOREIGN-STRING-ALLOC "get_touch")
                           SIGNATURE "n"
                           TYPES INTERFACE-ARRAY)))
                  (LET ((INTERFACE-ARRAY
                         (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID))
                                             :COUNT 0))
                        (MSG-PTR
                         (MEM-APTR MESSAGES '(:STRUCT CL-WL.FFI:WL_MESSAGE) 3)))
                    (WITH-FOREIGN-SLOTS
                     ((NAME SIGNATURE TYPES) MSG-PTR
                      (:STRUCT CL-WL.FFI:WL_MESSAGE))
                     (SETF NAME (FOREIGN-STRING-ALLOC "release")
                           SIGNATURE "5"
                           TYPES INTERFACE-ARRAY)))
                  MESSAGES))
               (EVENTS-PTR
                (LET ((MESSAGES
                       (CFFI:FOREIGN-ALLOC '(:STRUCT CL-WL.FFI:WL_MESSAGE)
                                           :COUNT 2)))
                  (LET ((INTERFACE-ARRAY
                         (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID))
                                             :COUNT 1))
                        (MSG-PTR
                         (MEM-APTR MESSAGES '(:STRUCT CL-WL.FFI:WL_MESSAGE) 0)))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 0) (NULL-POINTER))
                    (WITH-FOREIGN-SLOTS
                     ((NAME SIGNATURE TYPES) MSG-PTR
                      (:STRUCT CL-WL.FFI:WL_MESSAGE))
                     (SETF NAME (FOREIGN-STRING-ALLOC "capabilities")
                           SIGNATURE "u"
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
                     (SETF NAME (FOREIGN-STRING-ALLOC "name")
                           SIGNATURE "2s"
                           TYPES INTERFACE-ARRAY)))
                  MESSAGES)))
           (WITH-FOREIGN-SLOTS
            ((NAME VERSION METHOD_COUNT METHODS EVENT_COUNT EVENTS) *INTERFACE*
             (:STRUCT INTERFACE))
            (SETF NAME (FOREIGN-STRING-ALLOC "wl_seat")
                  VERSION 9
                  METHOD_COUNT 4
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
      (CL-WL::DEBUG-LOG! "Dispatching ~a:~a~%" "wl_seat" "get-pointer")
      (FUNCALL 'GET-POINTER RESOURCE
               (VALUES
                (FOREIGN-SLOT-VALUE
                 (MEM-APTR ARGS '(:UNION CL-WL.FFI:WL_ARGUMENT) 0)
                 '(:UNION CL-WL.FFI:WL_ARGUMENT) 'WL-FFI::N))))
     (1
      (CL-WL::DEBUG-LOG! "Dispatching ~a:~a~%" "wl_seat" "get-keyboard")
      (FUNCALL 'GET-KEYBOARD RESOURCE
               (VALUES
                (FOREIGN-SLOT-VALUE
                 (MEM-APTR ARGS '(:UNION CL-WL.FFI:WL_ARGUMENT) 0)
                 '(:UNION CL-WL.FFI:WL_ARGUMENT) 'WL-FFI::N))))
     (2
      (CL-WL::DEBUG-LOG! "Dispatching ~a:~a~%" "wl_seat" "get-touch")
      (FUNCALL 'GET-TOUCH RESOURCE
               (VALUES
                (FOREIGN-SLOT-VALUE
                 (MEM-APTR ARGS '(:UNION CL-WL.FFI:WL_ARGUMENT) 0)
                 '(:UNION CL-WL.FFI:WL_ARGUMENT) 'WL-FFI::N))))
     (3
      (CL-WL::DEBUG-LOG! "Dispatching ~a:~a~%" "wl_seat" "release")
      (FUNCALL 'RELEASE RESOURCE))))
 0)

(DEFVAR *DISPATCHER* (CALLBACK DISPATCHER-FFI))

(DEFMETHOD SHARED-INITIALIZE :AFTER ((INSTANCE DISPATCH) SLOT-NAMES &KEY ID)
  (DECLARE (IGNORE SLOT-NAMES))
  (UNLESS (WL_SEAT-ID INSTANCE)
    (LET* ((RESOURCE
            (CL-WL::CREATE-RESOURCE (CL-WL:PTR (CL-WL:CLIENT INSTANCE))
                                    *INTERFACE* (VERSION INSTANCE)
                                    (OR ID 0))))
      (SETF (CL-WL::TRANSIENT-ID INSTANCE)
              (SETF (WL_SEAT-ID INSTANCE)
                      (OR ID (CL-WL.FFI:RESOURCE-GET-ID RESOURCE))))
      (SETF (WL_SEAT-PTR INSTANCE) RESOURCE)
      (SETF (GETHASH (POINTER-ADDRESS RESOURCE) CL-WL::*RESOURCE-TRACKER*)
              INSTANCE)
      (RESOURCE-SET-DISPATCHER RESOURCE *DISPATCHER* (NULL-POINTER)
       (NULL-POINTER) (NULL-POINTER)))))

(DEFMETHOD SEND-CAPABILITIES
           ((DISPATCH DISPATCH) CAPABILITIES)
  (CL-WL::DEBUG-LOG! "Event: ~a~%" "capabilities")
  (LET ((ARG-LIST (FOREIGN-ALLOC '(:UNION WL_ARGUMENT) :COUNT 1)))
    (SETF (FOREIGN-SLOT-VALUE
           (MEM-AREF ARG-LIST '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 0)
           '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 'WL-FFI::U)
            (CAPABILITY-TO-VALUE CAPABILITIES))
    (RESOURCE-POST-EVENT-ARRAY (WL_SEAT-PTR DISPATCH) 0 ARG-LIST)))

(DEFMETHOD SEND-NAME ((DISPATCH DISPATCH) NAME)
  (CL-WL::DEBUG-LOG! "Event: ~a~%" "name")
  (LET ((ARG-LIST (FOREIGN-ALLOC '(:UNION WL_ARGUMENT) :COUNT 1)))
    (SETF (FOREIGN-SLOT-VALUE
           (MEM-AREF ARG-LIST '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 0)
           '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 'WL-FFI::S)
            NAME)
    (RESOURCE-POST-EVENT-ARRAY (WL_SEAT-PTR DISPATCH) 1 ARG-LIST)))

(DEFUN CAPABILITY-FROM-VALUE (BITS)
  (LOOP FOR ENTRY IN '((1 :POINTER) (2 :KEYBOARD)
                       (4 :TOUCH))
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

(DEFUN CAPABILITY-TO-VALUE (KEYWORDS)
  (REDUCE #'+ KEYWORDS :KEY
          (LAMBDA (KEYWORD)
            (OR
             (CADR
              (ASSOC KEYWORD
                     '((:POINTER 1) (:KEYBOARD 2)
                       (:TOUCH 4))))
             0))))

(DEFUN ERROR-FROM-VALUE (NUMBER)
  (LOOP FOR ENTRY IN '((0 :MISSING-CAPABILITY))
        FOR VALUE = (CAR ENTRY)
        FOR KEYWORD = (CADR ENTRY)
        WHEN (EQ NUMBER VALUE) RETURN KEYWORD
        FINALLY (ERROR (FORMAT NIL "Unknown enum value: ~a" NUMBER))))

(DEFUN ERROR-TO-VALUE (KEY)
  (LOOP FOR ENTRY IN '((0 :MISSING-CAPABILITY))
        FOR VALUE = (CAR ENTRY)
        FOR KEYWORD = (CADR ENTRY)
        WHEN (EQ KEY KEYWORD) RETURN VALUE
        FINALLY (ERROR (FORMAT NIL "Unknown enum keyword: ~a" KEY))))

(DEFCLASS GLOBAL (CL-WL:GLOBAL) NIL
          (:DEFAULT-INITARGS :VERSION 9 :DISPATCH-IMPL 'DISPATCH)
          (:DOCUMENTATION "group of input devices

A seat is a group of keyboards, pointer and touch devices. This
      object is published as a global during start up, or when such a
      device is hot plugged.  A seat typically has a pointer and
      maintains a keyboard focus and a pointer focus.
"))

(DEFMETHOD DISPATCH-BIND ((GLOBAL GLOBAL) CLIENT DATA VERSION ID)
  "Default bind implementation for the wl_seat global object.
This can be overriden by inheritance in case if custom behaviour is required."
  (CL-WL::DEBUG-LOG! "Binding ~a~%" "wl_seat")
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
  (CL-WL::DEBUG-LOG! "Initializing global object: ~a~%" "wl_seat")
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

(IN-PACKAGE :WL_POINTER)

(DEFCLASS DISPATCH (CL-WL:OBJECT)
          ((WL_POINTER-ID :INITFORM NIL :ACCESSOR
            WL_POINTER-ID)
           (WL_POINTER-PTR :INITFORM NIL :ACCESSOR
            WL_POINTER-PTR))
          (:DEFAULT-INITARGS :VERSION 9)
          (:DOCUMENTATION "pointer input device

The wl_pointer interface represents one or more input devices,
      such as mice, which control the pointer location and pointer_focus
      of a seat.

      The wl_pointer interface generates motion, enter and leave
      events for the surfaces that the pointer is located over,
      and button and axis events for button presses, button releases
      and scrolling.
"))

(DEFGENERIC SET-CURSOR
    (RESOURCE SERIAL SURFACE HOTSPOT_X
     HOTSPOT_Y))

(DEFGENERIC RELEASE
    (RESOURCE))

(DEFMETHOD CL-WL:DESTROY ((DISPATCH DISPATCH))
  (CL-WL::DEBUG-LOG! "Destroying dispatch object: ~a~%" "wl_pointer")
  (WHEN (CL-WL::DESTROY-CALLBACK DISPATCH)
    (LOOP FOR CALLBACK IN (CL-WL::DESTROY-CALLBACK DISPATCH)
          DO (FUNCALL CALLBACK DISPATCH)))
  (LET ((RESOURCE-PTR (WL_POINTER-PTR DISPATCH)))
    (CL-WL::REMOVE-RESOURCE (POINTER-ADDRESS RESOURCE-PTR)))
  (CL-WL::DN-IF DISPATCH))

(PUSHNEW
 (LIST "wl_pointer" (LIST "wl_surface")
       (LAMBDA ()
         (SETF *INTERFACE* (FOREIGN-ALLOC '(:STRUCT INTERFACE)))
         (LET ((REQUESTS-PTR
                (LET ((MESSAGES
                       (CFFI:FOREIGN-ALLOC '(:STRUCT CL-WL.FFI:WL_MESSAGE)
                                           :COUNT 2)))
                  (LET ((INTERFACE-ARRAY
                         (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID))
                                             :COUNT 4))
                        (MSG-PTR
                         (MEM-APTR MESSAGES '(:STRUCT CL-WL.FFI:WL_MESSAGE) 0)))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 0) (NULL-POINTER))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 1)
                            WL_SURFACE::*INTERFACE*)
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 2) (NULL-POINTER))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 3) (NULL-POINTER))
                    (WITH-FOREIGN-SLOTS
                     ((NAME SIGNATURE TYPES) MSG-PTR
                      (:STRUCT CL-WL.FFI:WL_MESSAGE))
                     (SETF NAME (FOREIGN-STRING-ALLOC "set_cursor")
                           SIGNATURE "u?oii"
                           TYPES INTERFACE-ARRAY)))
                  (LET ((INTERFACE-ARRAY
                         (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID))
                                             :COUNT 0))
                        (MSG-PTR
                         (MEM-APTR MESSAGES '(:STRUCT CL-WL.FFI:WL_MESSAGE) 1)))
                    (WITH-FOREIGN-SLOTS
                     ((NAME SIGNATURE TYPES) MSG-PTR
                      (:STRUCT CL-WL.FFI:WL_MESSAGE))
                     (SETF NAME (FOREIGN-STRING-ALLOC "release")
                           SIGNATURE "3"
                           TYPES INTERFACE-ARRAY)))
                  MESSAGES))
               (EVENTS-PTR
                (LET ((MESSAGES
                       (CFFI:FOREIGN-ALLOC '(:STRUCT CL-WL.FFI:WL_MESSAGE)
                                           :COUNT 11)))
                  (LET ((INTERFACE-ARRAY
                         (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID))
                                             :COUNT 4))
                        (MSG-PTR
                         (MEM-APTR MESSAGES '(:STRUCT CL-WL.FFI:WL_MESSAGE) 0)))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 0) (NULL-POINTER))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 1)
                            WL_SURFACE::*INTERFACE*)
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 2) (NULL-POINTER))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 3) (NULL-POINTER))
                    (WITH-FOREIGN-SLOTS
                     ((NAME SIGNATURE TYPES) MSG-PTR
                      (:STRUCT CL-WL.FFI:WL_MESSAGE))
                     (SETF NAME (FOREIGN-STRING-ALLOC "enter")
                           SIGNATURE "uoff"
                           TYPES INTERFACE-ARRAY)))
                  (LET ((INTERFACE-ARRAY
                         (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID))
                                             :COUNT 2))
                        (MSG-PTR
                         (MEM-APTR MESSAGES '(:STRUCT CL-WL.FFI:WL_MESSAGE) 1)))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 0) (NULL-POINTER))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 1)
                            WL_SURFACE::*INTERFACE*)
                    (WITH-FOREIGN-SLOTS
                     ((NAME SIGNATURE TYPES) MSG-PTR
                      (:STRUCT CL-WL.FFI:WL_MESSAGE))
                     (SETF NAME (FOREIGN-STRING-ALLOC "leave")
                           SIGNATURE "uo"
                           TYPES INTERFACE-ARRAY)))
                  (LET ((INTERFACE-ARRAY
                         (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID))
                                             :COUNT 3))
                        (MSG-PTR
                         (MEM-APTR MESSAGES '(:STRUCT CL-WL.FFI:WL_MESSAGE) 2)))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 0) (NULL-POINTER))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 1) (NULL-POINTER))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 2) (NULL-POINTER))
                    (WITH-FOREIGN-SLOTS
                     ((NAME SIGNATURE TYPES) MSG-PTR
                      (:STRUCT CL-WL.FFI:WL_MESSAGE))
                     (SETF NAME (FOREIGN-STRING-ALLOC "motion")
                           SIGNATURE "uff"
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
                     (SETF NAME (FOREIGN-STRING-ALLOC "button")
                           SIGNATURE "uuuu"
                           TYPES INTERFACE-ARRAY)))
                  (LET ((INTERFACE-ARRAY
                         (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID))
                                             :COUNT 3))
                        (MSG-PTR
                         (MEM-APTR MESSAGES '(:STRUCT CL-WL.FFI:WL_MESSAGE) 4)))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 0) (NULL-POINTER))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 1) (NULL-POINTER))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 2) (NULL-POINTER))
                    (WITH-FOREIGN-SLOTS
                     ((NAME SIGNATURE TYPES) MSG-PTR
                      (:STRUCT CL-WL.FFI:WL_MESSAGE))
                     (SETF NAME (FOREIGN-STRING-ALLOC "axis")
                           SIGNATURE "uuf"
                           TYPES INTERFACE-ARRAY)))
                  (LET ((INTERFACE-ARRAY
                         (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID))
                                             :COUNT 0))
                        (MSG-PTR
                         (MEM-APTR MESSAGES '(:STRUCT CL-WL.FFI:WL_MESSAGE) 5)))
                    (WITH-FOREIGN-SLOTS
                     ((NAME SIGNATURE TYPES) MSG-PTR
                      (:STRUCT CL-WL.FFI:WL_MESSAGE))
                     (SETF NAME (FOREIGN-STRING-ALLOC "frame")
                           SIGNATURE "5"
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
                     (SETF NAME (FOREIGN-STRING-ALLOC "axis_source")
                           SIGNATURE "5u"
                           TYPES INTERFACE-ARRAY)))
                  (LET ((INTERFACE-ARRAY
                         (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID))
                                             :COUNT 2))
                        (MSG-PTR
                         (MEM-APTR MESSAGES '(:STRUCT CL-WL.FFI:WL_MESSAGE) 7)))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 0) (NULL-POINTER))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 1) (NULL-POINTER))
                    (WITH-FOREIGN-SLOTS
                     ((NAME SIGNATURE TYPES) MSG-PTR
                      (:STRUCT CL-WL.FFI:WL_MESSAGE))
                     (SETF NAME (FOREIGN-STRING-ALLOC "axis_stop")
                           SIGNATURE "5uu"
                           TYPES INTERFACE-ARRAY)))
                  (LET ((INTERFACE-ARRAY
                         (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID))
                                             :COUNT 2))
                        (MSG-PTR
                         (MEM-APTR MESSAGES '(:STRUCT CL-WL.FFI:WL_MESSAGE) 8)))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 0) (NULL-POINTER))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 1) (NULL-POINTER))
                    (WITH-FOREIGN-SLOTS
                     ((NAME SIGNATURE TYPES) MSG-PTR
                      (:STRUCT CL-WL.FFI:WL_MESSAGE))
                     (SETF NAME (FOREIGN-STRING-ALLOC "axis_discrete")
                           SIGNATURE "5ui"
                           TYPES INTERFACE-ARRAY)))
                  (LET ((INTERFACE-ARRAY
                         (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID))
                                             :COUNT 2))
                        (MSG-PTR
                         (MEM-APTR MESSAGES '(:STRUCT CL-WL.FFI:WL_MESSAGE) 9)))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 0) (NULL-POINTER))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 1) (NULL-POINTER))
                    (WITH-FOREIGN-SLOTS
                     ((NAME SIGNATURE TYPES) MSG-PTR
                      (:STRUCT CL-WL.FFI:WL_MESSAGE))
                     (SETF NAME (FOREIGN-STRING-ALLOC "axis_value120")
                           SIGNATURE "8ui"
                           TYPES INTERFACE-ARRAY)))
                  (LET ((INTERFACE-ARRAY
                         (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID))
                                             :COUNT 2))
                        (MSG-PTR
                         (MEM-APTR MESSAGES '(:STRUCT CL-WL.FFI:WL_MESSAGE)
                          10)))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 0) (NULL-POINTER))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 1) (NULL-POINTER))
                    (WITH-FOREIGN-SLOTS
                     ((NAME SIGNATURE TYPES) MSG-PTR
                      (:STRUCT CL-WL.FFI:WL_MESSAGE))
                     (SETF NAME
                             (FOREIGN-STRING-ALLOC "axis_relative_direction")
                           SIGNATURE "9uu"
                           TYPES INTERFACE-ARRAY)))
                  MESSAGES)))
           (WITH-FOREIGN-SLOTS
            ((NAME VERSION METHOD_COUNT METHODS EVENT_COUNT EVENTS) *INTERFACE*
             (:STRUCT INTERFACE))
            (SETF NAME (FOREIGN-STRING-ALLOC "wl_pointer")
                  VERSION 9
                  METHOD_COUNT 2
                  METHODS REQUESTS-PTR
                  EVENT_COUNT 11
                  EVENTS EVENTS-PTR)))))
 CL-WL::*INTERFACE-INIT-LIST* :TEST #'CL-WL::INTERFACE-EXISTS-TEST)

(DEFCALLBACK DISPATCHER-FFI :INT
 ((DATA :POINTER) (TARGET :POINTER) (OPCODE :UINT) (MESSAGE :POINTER)
  (ARGS :POINTER))
 (DECLARE (IGNORE DATA MESSAGE))
 (LET ((RESOURCE (GETHASH (POINTER-ADDRESS TARGET) CL-WL::*RESOURCE-TRACKER*)))
   (ECASE OPCODE
     (0
      (CL-WL::DEBUG-LOG! "Dispatching ~a:~a~%" "wl_pointer" "set-cursor")
      (FUNCALL 'SET-CURSOR RESOURCE
               (VALUES
                (FOREIGN-SLOT-VALUE
                 (MEM-APTR ARGS '(:UNION CL-WL.FFI:WL_ARGUMENT) 0)
                 '(:UNION CL-WL.FFI:WL_ARGUMENT) 'WL-FFI::U))
               (GETHASH
                (POINTER-ADDRESS
                 (FOREIGN-SLOT-VALUE
                  (MEM-APTR ARGS '(:UNION CL-WL.FFI:WL_ARGUMENT) 1)
                  '(:UNION CL-WL.FFI:WL_ARGUMENT) 'WL-FFI::O))
                CL-WL::*RESOURCE-TRACKER*)
               (VALUES
                (FOREIGN-SLOT-VALUE
                 (MEM-APTR ARGS '(:UNION CL-WL.FFI:WL_ARGUMENT) 2)
                 '(:UNION CL-WL.FFI:WL_ARGUMENT) 'WL-FFI::I))
               (VALUES
                (FOREIGN-SLOT-VALUE
                 (MEM-APTR ARGS '(:UNION CL-WL.FFI:WL_ARGUMENT) 3)
                 '(:UNION CL-WL.FFI:WL_ARGUMENT) 'WL-FFI::I))))
     (1
      (CL-WL::DEBUG-LOG! "Dispatching ~a:~a~%" "wl_pointer" "release")
      (FUNCALL 'RELEASE RESOURCE))))
 0)

(DEFVAR *DISPATCHER* (CALLBACK DISPATCHER-FFI))

(DEFMETHOD SHARED-INITIALIZE :AFTER ((INSTANCE DISPATCH) SLOT-NAMES &KEY ID)
  (DECLARE (IGNORE SLOT-NAMES))
  (UNLESS (WL_POINTER-ID INSTANCE)
    (LET* ((RESOURCE
            (CL-WL::CREATE-RESOURCE (CL-WL:PTR (CL-WL:CLIENT INSTANCE))
                                    *INTERFACE* (VERSION INSTANCE)
                                    (OR ID 0))))
      (SETF (CL-WL::TRANSIENT-ID INSTANCE)
              (SETF (WL_POINTER-ID INSTANCE)
                      (OR ID (CL-WL.FFI:RESOURCE-GET-ID RESOURCE))))
      (SETF (WL_POINTER-PTR INSTANCE) RESOURCE)
      (SETF (GETHASH (POINTER-ADDRESS RESOURCE) CL-WL::*RESOURCE-TRACKER*)
              INSTANCE)
      (RESOURCE-SET-DISPATCHER RESOURCE *DISPATCHER* (NULL-POINTER)
       (NULL-POINTER) (NULL-POINTER)))))

(DEFMETHOD SEND-ENTER
           ((DISPATCH DISPATCH) SERIAL SURFACE
            SURFACE_X SURFACE_Y)
  (CL-WL::DEBUG-LOG! "Event: ~a~%" "enter")
  (LET ((ARG-LIST (FOREIGN-ALLOC '(:UNION WL_ARGUMENT) :COUNT 4)))
    (SETF (FOREIGN-SLOT-VALUE
           (MEM-AREF ARG-LIST '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 0)
           '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 'WL-FFI::U)
            SERIAL)
    (SETF (FOREIGN-SLOT-VALUE
           (MEM-AREF ARG-LIST '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 1)
           '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 'WL-FFI::O)
            (WL_SURFACE::WL_SURFACE-PTR SURFACE))
    (SETF (FOREIGN-SLOT-VALUE
           (MEM-AREF ARG-LIST '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 2)
           '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 'WL-FFI::F)
            (CL-WL::TO-FIXNUM SURFACE_X))
    (SETF (FOREIGN-SLOT-VALUE
           (MEM-AREF ARG-LIST '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 3)
           '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 'WL-FFI::F)
            (CL-WL::TO-FIXNUM SURFACE_Y))
    (RESOURCE-POST-EVENT-ARRAY (WL_POINTER-PTR DISPATCH) 0
     ARG-LIST)))

(DEFMETHOD SEND-LEAVE
           ((DISPATCH DISPATCH) SERIAL SURFACE)
  (CL-WL::DEBUG-LOG! "Event: ~a~%" "leave")
  (LET ((ARG-LIST (FOREIGN-ALLOC '(:UNION WL_ARGUMENT) :COUNT 2)))
    (SETF (FOREIGN-SLOT-VALUE
           (MEM-AREF ARG-LIST '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 0)
           '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 'WL-FFI::U)
            SERIAL)
    (SETF (FOREIGN-SLOT-VALUE
           (MEM-AREF ARG-LIST '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 1)
           '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 'WL-FFI::O)
            (WL_SURFACE::WL_SURFACE-PTR SURFACE))
    (RESOURCE-POST-EVENT-ARRAY (WL_POINTER-PTR DISPATCH) 1
     ARG-LIST)))

(DEFMETHOD SEND-MOTION
           ((DISPATCH DISPATCH) TIME SURFACE_X
            SURFACE_Y)
  (CL-WL::DEBUG-LOG! "Event: ~a~%" "motion")
  (LET ((ARG-LIST (FOREIGN-ALLOC '(:UNION WL_ARGUMENT) :COUNT 3)))
    (SETF (FOREIGN-SLOT-VALUE
           (MEM-AREF ARG-LIST '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 0)
           '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 'WL-FFI::U)
            TIME)
    (SETF (FOREIGN-SLOT-VALUE
           (MEM-AREF ARG-LIST '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 1)
           '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 'WL-FFI::F)
            (CL-WL::TO-FIXNUM SURFACE_X))
    (SETF (FOREIGN-SLOT-VALUE
           (MEM-AREF ARG-LIST '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 2)
           '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 'WL-FFI::F)
            (CL-WL::TO-FIXNUM SURFACE_Y))
    (RESOURCE-POST-EVENT-ARRAY (WL_POINTER-PTR DISPATCH) 2
     ARG-LIST)))

(DEFMETHOD SEND-BUTTON
           ((DISPATCH DISPATCH) SERIAL TIME
            BUTTON STATE)
  (CL-WL::DEBUG-LOG! "Event: ~a~%" "button")
  (LET ((ARG-LIST (FOREIGN-ALLOC '(:UNION WL_ARGUMENT) :COUNT 4)))
    (SETF (FOREIGN-SLOT-VALUE
           (MEM-AREF ARG-LIST '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 0)
           '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 'WL-FFI::U)
            SERIAL)
    (SETF (FOREIGN-SLOT-VALUE
           (MEM-AREF ARG-LIST '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 1)
           '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 'WL-FFI::U)
            TIME)
    (SETF (FOREIGN-SLOT-VALUE
           (MEM-AREF ARG-LIST '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 2)
           '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 'WL-FFI::U)
            BUTTON)
    (SETF (FOREIGN-SLOT-VALUE
           (MEM-AREF ARG-LIST '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 3)
           '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 'WL-FFI::U)
            (BUTTON-STATE-TO-VALUE STATE))
    (RESOURCE-POST-EVENT-ARRAY (WL_POINTER-PTR DISPATCH) 3
     ARG-LIST)))

(DEFMETHOD SEND-AXIS
           ((DISPATCH DISPATCH) TIME AXIS
            VALUE)
  (CL-WL::DEBUG-LOG! "Event: ~a~%" "axis")
  (LET ((ARG-LIST (FOREIGN-ALLOC '(:UNION WL_ARGUMENT) :COUNT 3)))
    (SETF (FOREIGN-SLOT-VALUE
           (MEM-AREF ARG-LIST '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 0)
           '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 'WL-FFI::U)
            TIME)
    (SETF (FOREIGN-SLOT-VALUE
           (MEM-AREF ARG-LIST '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 1)
           '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 'WL-FFI::U)
            (AXIS-TO-VALUE AXIS))
    (SETF (FOREIGN-SLOT-VALUE
           (MEM-AREF ARG-LIST '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 2)
           '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 'WL-FFI::F)
            (CL-WL::TO-FIXNUM VALUE))
    (RESOURCE-POST-EVENT-ARRAY (WL_POINTER-PTR DISPATCH) 4
     ARG-LIST)))

(DEFMETHOD SEND-FRAME ((DISPATCH DISPATCH))
  (CL-WL::DEBUG-LOG! "Event: ~a~%" "frame")
  (LET ((ARG-LIST (FOREIGN-ALLOC '(:UNION WL_ARGUMENT) :COUNT 0)))
    (RESOURCE-POST-EVENT-ARRAY (WL_POINTER-PTR DISPATCH) 5
     ARG-LIST)))

(DEFMETHOD SEND-AXIS-SOURCE
           ((DISPATCH DISPATCH) AXIS_SOURCE)
  (CL-WL::DEBUG-LOG! "Event: ~a~%" "axis_source")
  (LET ((ARG-LIST (FOREIGN-ALLOC '(:UNION WL_ARGUMENT) :COUNT 1)))
    (SETF (FOREIGN-SLOT-VALUE
           (MEM-AREF ARG-LIST '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 0)
           '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 'WL-FFI::U)
            (AXIS-SOURCE-TO-VALUE AXIS_SOURCE))
    (RESOURCE-POST-EVENT-ARRAY (WL_POINTER-PTR DISPATCH) 6
     ARG-LIST)))

(DEFMETHOD SEND-AXIS-STOP
           ((DISPATCH DISPATCH) TIME AXIS)
  (CL-WL::DEBUG-LOG! "Event: ~a~%" "axis_stop")
  (LET ((ARG-LIST (FOREIGN-ALLOC '(:UNION WL_ARGUMENT) :COUNT 2)))
    (SETF (FOREIGN-SLOT-VALUE
           (MEM-AREF ARG-LIST '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 0)
           '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 'WL-FFI::U)
            TIME)
    (SETF (FOREIGN-SLOT-VALUE
           (MEM-AREF ARG-LIST '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 1)
           '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 'WL-FFI::U)
            (AXIS-TO-VALUE AXIS))
    (RESOURCE-POST-EVENT-ARRAY (WL_POINTER-PTR DISPATCH) 7
     ARG-LIST)))

(DEFMETHOD SEND-AXIS-DISCRETE
           ((DISPATCH DISPATCH) AXIS DISCRETE)
  (CL-WL::DEBUG-LOG! "Event: ~a~%" "axis_discrete")
  (LET ((ARG-LIST (FOREIGN-ALLOC '(:UNION WL_ARGUMENT) :COUNT 2)))
    (SETF (FOREIGN-SLOT-VALUE
           (MEM-AREF ARG-LIST '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 0)
           '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 'WL-FFI::U)
            (AXIS-TO-VALUE AXIS))
    (SETF (FOREIGN-SLOT-VALUE
           (MEM-AREF ARG-LIST '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 1)
           '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 'WL-FFI::I)
            DISCRETE)
    (RESOURCE-POST-EVENT-ARRAY (WL_POINTER-PTR DISPATCH) 8
     ARG-LIST)))

(DEFMETHOD SEND-AXIS-VALUE120
           ((DISPATCH DISPATCH) AXIS VALUE120)
  (CL-WL::DEBUG-LOG! "Event: ~a~%" "axis_value120")
  (LET ((ARG-LIST (FOREIGN-ALLOC '(:UNION WL_ARGUMENT) :COUNT 2)))
    (SETF (FOREIGN-SLOT-VALUE
           (MEM-AREF ARG-LIST '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 0)
           '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 'WL-FFI::U)
            (AXIS-TO-VALUE AXIS))
    (SETF (FOREIGN-SLOT-VALUE
           (MEM-AREF ARG-LIST '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 1)
           '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 'WL-FFI::I)
            VALUE120)
    (RESOURCE-POST-EVENT-ARRAY (WL_POINTER-PTR DISPATCH) 9
     ARG-LIST)))

(DEFMETHOD SEND-AXIS-RELATIVE-DIRECTION
           ((DISPATCH DISPATCH) AXIS DIRECTION)
  (CL-WL::DEBUG-LOG! "Event: ~a~%" "axis_relative_direction")
  (LET ((ARG-LIST (FOREIGN-ALLOC '(:UNION WL_ARGUMENT) :COUNT 2)))
    (SETF (FOREIGN-SLOT-VALUE
           (MEM-AREF ARG-LIST '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 0)
           '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 'WL-FFI::U)
            (AXIS-TO-VALUE AXIS))
    (SETF (FOREIGN-SLOT-VALUE
           (MEM-AREF ARG-LIST '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 1)
           '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 'WL-FFI::U)
            (AXIS-RELATIVE-DIRECTION-TO-VALUE
             DIRECTION))
    (RESOURCE-POST-EVENT-ARRAY (WL_POINTER-PTR DISPATCH) 10
     ARG-LIST)))

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

(DEFUN BUTTON-STATE-FROM-VALUE (NUMBER)
  (LOOP FOR ENTRY IN '((0 :RELEASED) (1 :PRESSED))
        FOR VALUE = (CAR ENTRY)
        FOR KEYWORD = (CADR ENTRY)
        WHEN (EQ NUMBER VALUE) RETURN KEYWORD
        FINALLY (ERROR (FORMAT NIL "Unknown enum value: ~a" NUMBER))))

(DEFUN BUTTON-STATE-TO-VALUE (KEY)
  (LOOP FOR ENTRY IN '((0 :RELEASED) (1 :PRESSED))
        FOR VALUE = (CAR ENTRY)
        FOR KEYWORD = (CADR ENTRY)
        WHEN (EQ KEY KEYWORD) RETURN VALUE
        FINALLY (ERROR (FORMAT NIL "Unknown enum keyword: ~a" KEY))))

(DEFUN AXIS-FROM-VALUE (NUMBER)
  (LOOP FOR ENTRY IN '((0 :VERTICAL-SCROLL)
                       (1 :HORIZONTAL-SCROLL))
        FOR VALUE = (CAR ENTRY)
        FOR KEYWORD = (CADR ENTRY)
        WHEN (EQ NUMBER VALUE) RETURN KEYWORD
        FINALLY (ERROR (FORMAT NIL "Unknown enum value: ~a" NUMBER))))

(DEFUN AXIS-TO-VALUE (KEY)
  (LOOP FOR ENTRY IN '((0 :VERTICAL-SCROLL)
                       (1 :HORIZONTAL-SCROLL))
        FOR VALUE = (CAR ENTRY)
        FOR KEYWORD = (CADR ENTRY)
        WHEN (EQ KEY KEYWORD) RETURN VALUE
        FINALLY (ERROR (FORMAT NIL "Unknown enum keyword: ~a" KEY))))

(DEFUN AXIS-SOURCE-FROM-VALUE (NUMBER)
  (LOOP FOR ENTRY IN '((0 :WHEEL) (1 :FINGER)
                       (2 :CONTINUOUS) (3 :WHEEL-TILT))
        FOR VALUE = (CAR ENTRY)
        FOR KEYWORD = (CADR ENTRY)
        WHEN (EQ NUMBER VALUE) RETURN KEYWORD
        FINALLY (ERROR (FORMAT NIL "Unknown enum value: ~a" NUMBER))))

(DEFUN AXIS-SOURCE-TO-VALUE (KEY)
  (LOOP FOR ENTRY IN '((0 :WHEEL) (1 :FINGER)
                       (2 :CONTINUOUS) (3 :WHEEL-TILT))
        FOR VALUE = (CAR ENTRY)
        FOR KEYWORD = (CADR ENTRY)
        WHEN (EQ KEY KEYWORD) RETURN VALUE
        FINALLY (ERROR (FORMAT NIL "Unknown enum keyword: ~a" KEY))))

(DEFUN AXIS-RELATIVE-DIRECTION-FROM-VALUE (NUMBER)
  (LOOP FOR ENTRY IN '((0 :IDENTICAL) (1 :INVERTED))
        FOR VALUE = (CAR ENTRY)
        FOR KEYWORD = (CADR ENTRY)
        WHEN (EQ NUMBER VALUE) RETURN KEYWORD
        FINALLY (ERROR (FORMAT NIL "Unknown enum value: ~a" NUMBER))))

(DEFUN AXIS-RELATIVE-DIRECTION-TO-VALUE (KEY)
  (LOOP FOR ENTRY IN '((0 :IDENTICAL) (1 :INVERTED))
        FOR VALUE = (CAR ENTRY)
        FOR KEYWORD = (CADR ENTRY)
        WHEN (EQ KEY KEYWORD) RETURN VALUE
        FINALLY (ERROR (FORMAT NIL "Unknown enum keyword: ~a" KEY))))

(DEFCLASS GLOBAL (CL-WL:GLOBAL) NIL
          (:DEFAULT-INITARGS :VERSION 9 :DISPATCH-IMPL 'DISPATCH)
          (:DOCUMENTATION "pointer input device

The wl_pointer interface represents one or more input devices,
      such as mice, which control the pointer location and pointer_focus
      of a seat.

      The wl_pointer interface generates motion, enter and leave
      events for the surfaces that the pointer is located over,
      and button and axis events for button presses, button releases
      and scrolling.
"))

(DEFMETHOD DISPATCH-BIND ((GLOBAL GLOBAL) CLIENT DATA VERSION ID)
  "Default bind implementation for the wl_pointer global object.
This can be overriden by inheritance in case if custom behaviour is required."
  (CL-WL::DEBUG-LOG! "Binding ~a~%" "wl_pointer")
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
  (CL-WL::DEBUG-LOG! "Initializing global object: ~a~%" "wl_pointer")
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

(IN-PACKAGE :WL_KEYBOARD)

(DEFCLASS DISPATCH (CL-WL:OBJECT)
          ((WL_KEYBOARD-ID :INITFORM NIL :ACCESSOR
            WL_KEYBOARD-ID)
           (WL_KEYBOARD-PTR :INITFORM NIL :ACCESSOR
            WL_KEYBOARD-PTR))
          (:DEFAULT-INITARGS :VERSION 9)
          (:DOCUMENTATION "keyboard input device

The wl_keyboard interface represents one or more keyboards
      associated with a seat.
"))

(DEFGENERIC RELEASE
    (RESOURCE))

(DEFMETHOD CL-WL:DESTROY ((DISPATCH DISPATCH))
  (CL-WL::DEBUG-LOG! "Destroying dispatch object: ~a~%" "wl_keyboard")
  (WHEN (CL-WL::DESTROY-CALLBACK DISPATCH)
    (LOOP FOR CALLBACK IN (CL-WL::DESTROY-CALLBACK DISPATCH)
          DO (FUNCALL CALLBACK DISPATCH)))
  (LET ((RESOURCE-PTR (WL_KEYBOARD-PTR DISPATCH)))
    (CL-WL::REMOVE-RESOURCE (POINTER-ADDRESS RESOURCE-PTR)))
  (CL-WL::DN-IF DISPATCH))

(PUSHNEW
 (LIST "wl_keyboard" (LIST "wl_surface")
       (LAMBDA ()
         (SETF *INTERFACE* (FOREIGN-ALLOC '(:STRUCT INTERFACE)))
         (LET ((REQUESTS-PTR
                (LET ((MESSAGES
                       (CFFI:FOREIGN-ALLOC '(:STRUCT CL-WL.FFI:WL_MESSAGE)
                                           :COUNT 1)))
                  (LET ((INTERFACE-ARRAY
                         (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID))
                                             :COUNT 0))
                        (MSG-PTR
                         (MEM-APTR MESSAGES '(:STRUCT CL-WL.FFI:WL_MESSAGE) 0)))
                    (WITH-FOREIGN-SLOTS
                     ((NAME SIGNATURE TYPES) MSG-PTR
                      (:STRUCT CL-WL.FFI:WL_MESSAGE))
                     (SETF NAME (FOREIGN-STRING-ALLOC "release")
                           SIGNATURE "3"
                           TYPES INTERFACE-ARRAY)))
                  MESSAGES))
               (EVENTS-PTR
                (LET ((MESSAGES
                       (CFFI:FOREIGN-ALLOC '(:STRUCT CL-WL.FFI:WL_MESSAGE)
                                           :COUNT 6)))
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
                     (SETF NAME (FOREIGN-STRING-ALLOC "keymap")
                           SIGNATURE "uhu"
                           TYPES INTERFACE-ARRAY)))
                  (LET ((INTERFACE-ARRAY
                         (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID))
                                             :COUNT 3))
                        (MSG-PTR
                         (MEM-APTR MESSAGES '(:STRUCT CL-WL.FFI:WL_MESSAGE) 1)))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 0) (NULL-POINTER))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 1)
                            WL_SURFACE::*INTERFACE*)
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 2) (NULL-POINTER))
                    (WITH-FOREIGN-SLOTS
                     ((NAME SIGNATURE TYPES) MSG-PTR
                      (:STRUCT CL-WL.FFI:WL_MESSAGE))
                     (SETF NAME (FOREIGN-STRING-ALLOC "enter")
                           SIGNATURE "uoa"
                           TYPES INTERFACE-ARRAY)))
                  (LET ((INTERFACE-ARRAY
                         (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID))
                                             :COUNT 2))
                        (MSG-PTR
                         (MEM-APTR MESSAGES '(:STRUCT CL-WL.FFI:WL_MESSAGE) 2)))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 0) (NULL-POINTER))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 1)
                            WL_SURFACE::*INTERFACE*)
                    (WITH-FOREIGN-SLOTS
                     ((NAME SIGNATURE TYPES) MSG-PTR
                      (:STRUCT CL-WL.FFI:WL_MESSAGE))
                     (SETF NAME (FOREIGN-STRING-ALLOC "leave")
                           SIGNATURE "uo"
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
                     (SETF NAME (FOREIGN-STRING-ALLOC "key")
                           SIGNATURE "uuuu"
                           TYPES INTERFACE-ARRAY)))
                  (LET ((INTERFACE-ARRAY
                         (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID))
                                             :COUNT 5))
                        (MSG-PTR
                         (MEM-APTR MESSAGES '(:STRUCT CL-WL.FFI:WL_MESSAGE) 4)))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 0) (NULL-POINTER))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 1) (NULL-POINTER))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 2) (NULL-POINTER))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 3) (NULL-POINTER))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 4) (NULL-POINTER))
                    (WITH-FOREIGN-SLOTS
                     ((NAME SIGNATURE TYPES) MSG-PTR
                      (:STRUCT CL-WL.FFI:WL_MESSAGE))
                     (SETF NAME (FOREIGN-STRING-ALLOC "modifiers")
                           SIGNATURE "uuuuu"
                           TYPES INTERFACE-ARRAY)))
                  (LET ((INTERFACE-ARRAY
                         (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID))
                                             :COUNT 2))
                        (MSG-PTR
                         (MEM-APTR MESSAGES '(:STRUCT CL-WL.FFI:WL_MESSAGE) 5)))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 0) (NULL-POINTER))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 1) (NULL-POINTER))
                    (WITH-FOREIGN-SLOTS
                     ((NAME SIGNATURE TYPES) MSG-PTR
                      (:STRUCT CL-WL.FFI:WL_MESSAGE))
                     (SETF NAME (FOREIGN-STRING-ALLOC "repeat_info")
                           SIGNATURE "4ii"
                           TYPES INTERFACE-ARRAY)))
                  MESSAGES)))
           (WITH-FOREIGN-SLOTS
            ((NAME VERSION METHOD_COUNT METHODS EVENT_COUNT EVENTS) *INTERFACE*
             (:STRUCT INTERFACE))
            (SETF NAME (FOREIGN-STRING-ALLOC "wl_keyboard")
                  VERSION 9
                  METHOD_COUNT 1
                  METHODS REQUESTS-PTR
                  EVENT_COUNT 6
                  EVENTS EVENTS-PTR)))))
 CL-WL::*INTERFACE-INIT-LIST* :TEST #'CL-WL::INTERFACE-EXISTS-TEST)

(DEFCALLBACK DISPATCHER-FFI :INT
 ((DATA :POINTER) (TARGET :POINTER) (OPCODE :UINT) (MESSAGE :POINTER)
  (ARGS :POINTER))
 (DECLARE (IGNORE DATA MESSAGE ARGS))
 (LET ((RESOURCE (GETHASH (POINTER-ADDRESS TARGET) CL-WL::*RESOURCE-TRACKER*)))
   (ECASE OPCODE
     (0
      (CL-WL::DEBUG-LOG! "Dispatching ~a:~a~%" "wl_keyboard" "release")
      (FUNCALL 'RELEASE RESOURCE))))
 0)

(DEFVAR *DISPATCHER* (CALLBACK DISPATCHER-FFI))

(DEFMETHOD SHARED-INITIALIZE :AFTER ((INSTANCE DISPATCH) SLOT-NAMES &KEY ID)
  (DECLARE (IGNORE SLOT-NAMES))
  (UNLESS (WL_KEYBOARD-ID INSTANCE)
    (LET* ((RESOURCE
            (CL-WL::CREATE-RESOURCE (CL-WL:PTR (CL-WL:CLIENT INSTANCE))
                                    *INTERFACE* (VERSION INSTANCE)
                                    (OR ID 0))))
      (SETF (CL-WL::TRANSIENT-ID INSTANCE)
              (SETF (WL_KEYBOARD-ID INSTANCE)
                      (OR ID (CL-WL.FFI:RESOURCE-GET-ID RESOURCE))))
      (SETF (WL_KEYBOARD-PTR INSTANCE) RESOURCE)
      (SETF (GETHASH (POINTER-ADDRESS RESOURCE) CL-WL::*RESOURCE-TRACKER*)
              INSTANCE)
      (RESOURCE-SET-DISPATCHER RESOURCE *DISPATCHER* (NULL-POINTER)
       (NULL-POINTER) (NULL-POINTER)))))

(DEFMETHOD SEND-KEYMAP
           ((DISPATCH DISPATCH) FORMAT FD
            SIZE)
  (CL-WL::DEBUG-LOG! "Event: ~a~%" "keymap")
  (LET ((ARG-LIST (FOREIGN-ALLOC '(:UNION WL_ARGUMENT) :COUNT 3)))
    (SETF (FOREIGN-SLOT-VALUE
           (MEM-AREF ARG-LIST '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 0)
           '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 'WL-FFI::U)
            (KEYMAP-FORMAT-TO-VALUE FORMAT))
    (SETF (FOREIGN-SLOT-VALUE
           (MEM-AREF ARG-LIST '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 1)
           '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 'WL-FFI::H)
            FD)
    (SETF (FOREIGN-SLOT-VALUE
           (MEM-AREF ARG-LIST '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 2)
           '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 'WL-FFI::U)
            SIZE)
    (RESOURCE-POST-EVENT-ARRAY (WL_KEYBOARD-PTR DISPATCH) 0
     ARG-LIST)))

(DEFMETHOD SEND-ENTER
           ((DISPATCH DISPATCH) SERIAL SURFACE
            KEYS)
  (CL-WL::DEBUG-LOG! "Event: ~a~%" "enter")
  (LET ((ARG-LIST (FOREIGN-ALLOC '(:UNION WL_ARGUMENT) :COUNT 3)))
    (SETF (FOREIGN-SLOT-VALUE
           (MEM-AREF ARG-LIST '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 0)
           '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 'WL-FFI::U)
            SERIAL)
    (SETF (FOREIGN-SLOT-VALUE
           (MEM-AREF ARG-LIST '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 1)
           '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 'WL-FFI::O)
            (WL_SURFACE::WL_SURFACE-PTR SURFACE))
    (SETF (FOREIGN-SLOT-VALUE
           (MEM-AREF ARG-LIST '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 2)
           '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 'WL-FFI::A)
            (LET* ((LENGTH (LENGTH KEYS))
                   (STRUCT (FOREIGN-ALLOC '(:STRUCT WL_ARRAY)))
                   (DATA (FOREIGN-ALLOC :UINT32 :COUNT LENGTH)))
              (LOOP FOR INDEX BELOW LENGTH
                    DO (SETF (MEM-AREF DATA :UINT32 INDEX)
                               (NTH INDEX KEYS)))
              (SETF (FOREIGN-SLOT-VALUE STRUCT '(:STRUCT WL_ARRAY)
                     'CL-WL.FFI::SIZE)
                      (* LENGTH (FOREIGN-TYPE-SIZE :UINT32))
                    (FOREIGN-SLOT-VALUE STRUCT '(:STRUCT WL_ARRAY)
                     'CL-WL.FFI::ALLOC)
                      LENGTH
                    (FOREIGN-SLOT-VALUE STRUCT '(:STRUCT WL_ARRAY)
                     'CL-WL.FFI::DATA)
                      DATA)
              STRUCT))
    (RESOURCE-POST-EVENT-ARRAY (WL_KEYBOARD-PTR DISPATCH) 1
     ARG-LIST)))

(DEFMETHOD SEND-LEAVE
           ((DISPATCH DISPATCH) SERIAL SURFACE)
  (CL-WL::DEBUG-LOG! "Event: ~a~%" "leave")
  (LET ((ARG-LIST (FOREIGN-ALLOC '(:UNION WL_ARGUMENT) :COUNT 2)))
    (SETF (FOREIGN-SLOT-VALUE
           (MEM-AREF ARG-LIST '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 0)
           '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 'WL-FFI::U)
            SERIAL)
    (SETF (FOREIGN-SLOT-VALUE
           (MEM-AREF ARG-LIST '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 1)
           '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 'WL-FFI::O)
            (WL_SURFACE::WL_SURFACE-PTR SURFACE))
    (RESOURCE-POST-EVENT-ARRAY (WL_KEYBOARD-PTR DISPATCH) 2
     ARG-LIST)))

(DEFMETHOD SEND-KEY
           ((DISPATCH DISPATCH) SERIAL TIME
            KEY STATE)
  (CL-WL::DEBUG-LOG! "Event: ~a~%" "key")
  (LET ((ARG-LIST (FOREIGN-ALLOC '(:UNION WL_ARGUMENT) :COUNT 4)))
    (SETF (FOREIGN-SLOT-VALUE
           (MEM-AREF ARG-LIST '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 0)
           '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 'WL-FFI::U)
            SERIAL)
    (SETF (FOREIGN-SLOT-VALUE
           (MEM-AREF ARG-LIST '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 1)
           '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 'WL-FFI::U)
            TIME)
    (SETF (FOREIGN-SLOT-VALUE
           (MEM-AREF ARG-LIST '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 2)
           '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 'WL-FFI::U)
            KEY)
    (SETF (FOREIGN-SLOT-VALUE
           (MEM-AREF ARG-LIST '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 3)
           '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 'WL-FFI::U)
            (KEY-STATE-TO-VALUE STATE))
    (RESOURCE-POST-EVENT-ARRAY (WL_KEYBOARD-PTR DISPATCH) 3
     ARG-LIST)))

(DEFMETHOD SEND-MODIFIERS
           ((DISPATCH DISPATCH) SERIAL MODS_DEPRESSED
            MODS_LATCHED MODS_LOCKED GROUP)
  (CL-WL::DEBUG-LOG! "Event: ~a~%" "modifiers")
  (LET ((ARG-LIST (FOREIGN-ALLOC '(:UNION WL_ARGUMENT) :COUNT 5)))
    (SETF (FOREIGN-SLOT-VALUE
           (MEM-AREF ARG-LIST '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 0)
           '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 'WL-FFI::U)
            SERIAL)
    (SETF (FOREIGN-SLOT-VALUE
           (MEM-AREF ARG-LIST '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 1)
           '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 'WL-FFI::U)
            MODS_DEPRESSED)
    (SETF (FOREIGN-SLOT-VALUE
           (MEM-AREF ARG-LIST '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 2)
           '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 'WL-FFI::U)
            MODS_LATCHED)
    (SETF (FOREIGN-SLOT-VALUE
           (MEM-AREF ARG-LIST '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 3)
           '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 'WL-FFI::U)
            MODS_LOCKED)
    (SETF (FOREIGN-SLOT-VALUE
           (MEM-AREF ARG-LIST '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 4)
           '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 'WL-FFI::U)
            GROUP)
    (RESOURCE-POST-EVENT-ARRAY (WL_KEYBOARD-PTR DISPATCH) 4
     ARG-LIST)))

(DEFMETHOD SEND-REPEAT-INFO
           ((DISPATCH DISPATCH) RATE DELAY)
  (CL-WL::DEBUG-LOG! "Event: ~a~%" "repeat_info")
  (LET ((ARG-LIST (FOREIGN-ALLOC '(:UNION WL_ARGUMENT) :COUNT 2)))
    (SETF (FOREIGN-SLOT-VALUE
           (MEM-AREF ARG-LIST '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 0)
           '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 'WL-FFI::I)
            RATE)
    (SETF (FOREIGN-SLOT-VALUE
           (MEM-AREF ARG-LIST '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 1)
           '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 'WL-FFI::I)
            DELAY)
    (RESOURCE-POST-EVENT-ARRAY (WL_KEYBOARD-PTR DISPATCH) 5
     ARG-LIST)))

(DEFUN KEYMAP-FORMAT-FROM-VALUE (NUMBER)
  (LOOP FOR ENTRY IN '((0 :NO-KEYMAP) (1 :XKB-V1))
        FOR VALUE = (CAR ENTRY)
        FOR KEYWORD = (CADR ENTRY)
        WHEN (EQ NUMBER VALUE) RETURN KEYWORD
        FINALLY (ERROR (FORMAT NIL "Unknown enum value: ~a" NUMBER))))

(DEFUN KEYMAP-FORMAT-TO-VALUE (KEY)
  (LOOP FOR ENTRY IN '((0 :NO-KEYMAP) (1 :XKB-V1))
        FOR VALUE = (CAR ENTRY)
        FOR KEYWORD = (CADR ENTRY)
        WHEN (EQ KEY KEYWORD) RETURN VALUE
        FINALLY (ERROR (FORMAT NIL "Unknown enum keyword: ~a" KEY))))

(DEFUN KEY-STATE-FROM-VALUE (NUMBER)
  (LOOP FOR ENTRY IN '((0 :RELEASED) (1 :PRESSED))
        FOR VALUE = (CAR ENTRY)
        FOR KEYWORD = (CADR ENTRY)
        WHEN (EQ NUMBER VALUE) RETURN KEYWORD
        FINALLY (ERROR (FORMAT NIL "Unknown enum value: ~a" NUMBER))))

(DEFUN KEY-STATE-TO-VALUE (KEY)
  (LOOP FOR ENTRY IN '((0 :RELEASED) (1 :PRESSED))
        FOR VALUE = (CAR ENTRY)
        FOR KEYWORD = (CADR ENTRY)
        WHEN (EQ KEY KEYWORD) RETURN VALUE
        FINALLY (ERROR (FORMAT NIL "Unknown enum keyword: ~a" KEY))))

(DEFCLASS GLOBAL (CL-WL:GLOBAL) NIL
          (:DEFAULT-INITARGS :VERSION 9 :DISPATCH-IMPL 'DISPATCH)
          (:DOCUMENTATION "keyboard input device

The wl_keyboard interface represents one or more keyboards
      associated with a seat.
"))

(DEFMETHOD DISPATCH-BIND ((GLOBAL GLOBAL) CLIENT DATA VERSION ID)
  "Default bind implementation for the wl_keyboard global object.
This can be overriden by inheritance in case if custom behaviour is required."
  (CL-WL::DEBUG-LOG! "Binding ~a~%" "wl_keyboard")
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
  (CL-WL::DEBUG-LOG! "Initializing global object: ~a~%" "wl_keyboard")
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

(IN-PACKAGE :WL_TOUCH)

(DEFCLASS DISPATCH (CL-WL:OBJECT)
          ((WL_TOUCH-ID :INITFORM NIL :ACCESSOR
            WL_TOUCH-ID)
           (WL_TOUCH-PTR :INITFORM NIL :ACCESSOR
            WL_TOUCH-PTR))
          (:DEFAULT-INITARGS :VERSION 9)
          (:DOCUMENTATION "touchscreen input device

The wl_touch interface represents a touchscreen
      associated with a seat.

      Touch interactions can consist of one or more contacts.
      For each contact, a series of events is generated, starting
      with a down event, followed by zero or more motion events,
      and ending with an up event. Events relating to the same
      contact point can be identified by the ID of the sequence.
"))

(DEFGENERIC RELEASE
    (RESOURCE))

(DEFMETHOD CL-WL:DESTROY ((DISPATCH DISPATCH))
  (CL-WL::DEBUG-LOG! "Destroying dispatch object: ~a~%" "wl_touch")
  (WHEN (CL-WL::DESTROY-CALLBACK DISPATCH)
    (LOOP FOR CALLBACK IN (CL-WL::DESTROY-CALLBACK DISPATCH)
          DO (FUNCALL CALLBACK DISPATCH)))
  (LET ((RESOURCE-PTR (WL_TOUCH-PTR DISPATCH)))
    (CL-WL::REMOVE-RESOURCE (POINTER-ADDRESS RESOURCE-PTR)))
  (CL-WL::DN-IF DISPATCH))

(PUSHNEW
 (LIST "wl_touch" (LIST "wl_surface")
       (LAMBDA ()
         (SETF *INTERFACE* (FOREIGN-ALLOC '(:STRUCT INTERFACE)))
         (LET ((REQUESTS-PTR
                (LET ((MESSAGES
                       (CFFI:FOREIGN-ALLOC '(:STRUCT CL-WL.FFI:WL_MESSAGE)
                                           :COUNT 1)))
                  (LET ((INTERFACE-ARRAY
                         (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID))
                                             :COUNT 0))
                        (MSG-PTR
                         (MEM-APTR MESSAGES '(:STRUCT CL-WL.FFI:WL_MESSAGE) 0)))
                    (WITH-FOREIGN-SLOTS
                     ((NAME SIGNATURE TYPES) MSG-PTR
                      (:STRUCT CL-WL.FFI:WL_MESSAGE))
                     (SETF NAME (FOREIGN-STRING-ALLOC "release")
                           SIGNATURE "3"
                           TYPES INTERFACE-ARRAY)))
                  MESSAGES))
               (EVENTS-PTR
                (LET ((MESSAGES
                       (CFFI:FOREIGN-ALLOC '(:STRUCT CL-WL.FFI:WL_MESSAGE)
                                           :COUNT 7)))
                  (LET ((INTERFACE-ARRAY
                         (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID))
                                             :COUNT 6))
                        (MSG-PTR
                         (MEM-APTR MESSAGES '(:STRUCT CL-WL.FFI:WL_MESSAGE) 0)))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 0) (NULL-POINTER))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 1) (NULL-POINTER))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 2)
                            WL_SURFACE::*INTERFACE*)
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 3) (NULL-POINTER))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 4) (NULL-POINTER))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 5) (NULL-POINTER))
                    (WITH-FOREIGN-SLOTS
                     ((NAME SIGNATURE TYPES) MSG-PTR
                      (:STRUCT CL-WL.FFI:WL_MESSAGE))
                     (SETF NAME (FOREIGN-STRING-ALLOC "down")
                           SIGNATURE "uuoiff"
                           TYPES INTERFACE-ARRAY)))
                  (LET ((INTERFACE-ARRAY
                         (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID))
                                             :COUNT 3))
                        (MSG-PTR
                         (MEM-APTR MESSAGES '(:STRUCT CL-WL.FFI:WL_MESSAGE) 1)))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 0) (NULL-POINTER))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 1) (NULL-POINTER))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 2) (NULL-POINTER))
                    (WITH-FOREIGN-SLOTS
                     ((NAME SIGNATURE TYPES) MSG-PTR
                      (:STRUCT CL-WL.FFI:WL_MESSAGE))
                     (SETF NAME (FOREIGN-STRING-ALLOC "up")
                           SIGNATURE "uui"
                           TYPES INTERFACE-ARRAY)))
                  (LET ((INTERFACE-ARRAY
                         (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID))
                                             :COUNT 4))
                        (MSG-PTR
                         (MEM-APTR MESSAGES '(:STRUCT CL-WL.FFI:WL_MESSAGE) 2)))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 0) (NULL-POINTER))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 1) (NULL-POINTER))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 2) (NULL-POINTER))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 3) (NULL-POINTER))
                    (WITH-FOREIGN-SLOTS
                     ((NAME SIGNATURE TYPES) MSG-PTR
                      (:STRUCT CL-WL.FFI:WL_MESSAGE))
                     (SETF NAME (FOREIGN-STRING-ALLOC "motion")
                           SIGNATURE "uiff"
                           TYPES INTERFACE-ARRAY)))
                  (LET ((INTERFACE-ARRAY
                         (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID))
                                             :COUNT 0))
                        (MSG-PTR
                         (MEM-APTR MESSAGES '(:STRUCT CL-WL.FFI:WL_MESSAGE) 3)))
                    (WITH-FOREIGN-SLOTS
                     ((NAME SIGNATURE TYPES) MSG-PTR
                      (:STRUCT CL-WL.FFI:WL_MESSAGE))
                     (SETF NAME (FOREIGN-STRING-ALLOC "frame")
                           SIGNATURE ""
                           TYPES INTERFACE-ARRAY)))
                  (LET ((INTERFACE-ARRAY
                         (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID))
                                             :COUNT 0))
                        (MSG-PTR
                         (MEM-APTR MESSAGES '(:STRUCT CL-WL.FFI:WL_MESSAGE) 4)))
                    (WITH-FOREIGN-SLOTS
                     ((NAME SIGNATURE TYPES) MSG-PTR
                      (:STRUCT CL-WL.FFI:WL_MESSAGE))
                     (SETF NAME (FOREIGN-STRING-ALLOC "cancel")
                           SIGNATURE ""
                           TYPES INTERFACE-ARRAY)))
                  (LET ((INTERFACE-ARRAY
                         (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID))
                                             :COUNT 3))
                        (MSG-PTR
                         (MEM-APTR MESSAGES '(:STRUCT CL-WL.FFI:WL_MESSAGE) 5)))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 0) (NULL-POINTER))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 1) (NULL-POINTER))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 2) (NULL-POINTER))
                    (WITH-FOREIGN-SLOTS
                     ((NAME SIGNATURE TYPES) MSG-PTR
                      (:STRUCT CL-WL.FFI:WL_MESSAGE))
                     (SETF NAME (FOREIGN-STRING-ALLOC "shape")
                           SIGNATURE "6iff"
                           TYPES INTERFACE-ARRAY)))
                  (LET ((INTERFACE-ARRAY
                         (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID))
                                             :COUNT 2))
                        (MSG-PTR
                         (MEM-APTR MESSAGES '(:STRUCT CL-WL.FFI:WL_MESSAGE) 6)))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 0) (NULL-POINTER))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 1) (NULL-POINTER))
                    (WITH-FOREIGN-SLOTS
                     ((NAME SIGNATURE TYPES) MSG-PTR
                      (:STRUCT CL-WL.FFI:WL_MESSAGE))
                     (SETF NAME (FOREIGN-STRING-ALLOC "orientation")
                           SIGNATURE "6if"
                           TYPES INTERFACE-ARRAY)))
                  MESSAGES)))
           (WITH-FOREIGN-SLOTS
            ((NAME VERSION METHOD_COUNT METHODS EVENT_COUNT EVENTS) *INTERFACE*
             (:STRUCT INTERFACE))
            (SETF NAME (FOREIGN-STRING-ALLOC "wl_touch")
                  VERSION 9
                  METHOD_COUNT 1
                  METHODS REQUESTS-PTR
                  EVENT_COUNT 7
                  EVENTS EVENTS-PTR)))))
 CL-WL::*INTERFACE-INIT-LIST* :TEST #'CL-WL::INTERFACE-EXISTS-TEST)

(DEFCALLBACK DISPATCHER-FFI :INT
 ((DATA :POINTER) (TARGET :POINTER) (OPCODE :UINT) (MESSAGE :POINTER)
  (ARGS :POINTER))
 (DECLARE (IGNORE DATA MESSAGE ARGS))
 (LET ((RESOURCE (GETHASH (POINTER-ADDRESS TARGET) CL-WL::*RESOURCE-TRACKER*)))
   (ECASE OPCODE
     (0
      (CL-WL::DEBUG-LOG! "Dispatching ~a:~a~%" "wl_touch" "release")
      (FUNCALL 'RELEASE RESOURCE))))
 0)

(DEFVAR *DISPATCHER* (CALLBACK DISPATCHER-FFI))

(DEFMETHOD SHARED-INITIALIZE :AFTER ((INSTANCE DISPATCH) SLOT-NAMES &KEY ID)
  (DECLARE (IGNORE SLOT-NAMES))
  (UNLESS (WL_TOUCH-ID INSTANCE)
    (LET* ((RESOURCE
            (CL-WL::CREATE-RESOURCE (CL-WL:PTR (CL-WL:CLIENT INSTANCE))
                                    *INTERFACE* (VERSION INSTANCE)
                                    (OR ID 0))))
      (SETF (CL-WL::TRANSIENT-ID INSTANCE)
              (SETF (WL_TOUCH-ID INSTANCE)
                      (OR ID (CL-WL.FFI:RESOURCE-GET-ID RESOURCE))))
      (SETF (WL_TOUCH-PTR INSTANCE) RESOURCE)
      (SETF (GETHASH (POINTER-ADDRESS RESOURCE) CL-WL::*RESOURCE-TRACKER*)
              INSTANCE)
      (RESOURCE-SET-DISPATCHER RESOURCE *DISPATCHER* (NULL-POINTER)
       (NULL-POINTER) (NULL-POINTER)))))

(DEFMETHOD SEND-DOWN
           ((DISPATCH DISPATCH) SERIAL TIME
            SURFACE ID X Y)
  (CL-WL::DEBUG-LOG! "Event: ~a~%" "down")
  (LET ((ARG-LIST (FOREIGN-ALLOC '(:UNION WL_ARGUMENT) :COUNT 6)))
    (SETF (FOREIGN-SLOT-VALUE
           (MEM-AREF ARG-LIST '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 0)
           '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 'WL-FFI::U)
            SERIAL)
    (SETF (FOREIGN-SLOT-VALUE
           (MEM-AREF ARG-LIST '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 1)
           '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 'WL-FFI::U)
            TIME)
    (SETF (FOREIGN-SLOT-VALUE
           (MEM-AREF ARG-LIST '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 2)
           '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 'WL-FFI::O)
            (WL_SURFACE::WL_SURFACE-PTR SURFACE))
    (SETF (FOREIGN-SLOT-VALUE
           (MEM-AREF ARG-LIST '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 3)
           '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 'WL-FFI::I)
            ID)
    (SETF (FOREIGN-SLOT-VALUE
           (MEM-AREF ARG-LIST '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 4)
           '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 'WL-FFI::F)
            (CL-WL::TO-FIXNUM X))
    (SETF (FOREIGN-SLOT-VALUE
           (MEM-AREF ARG-LIST '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 5)
           '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 'WL-FFI::F)
            (CL-WL::TO-FIXNUM Y))
    (RESOURCE-POST-EVENT-ARRAY (WL_TOUCH-PTR DISPATCH) 0 ARG-LIST)))

(DEFMETHOD SEND-UP
           ((DISPATCH DISPATCH) SERIAL TIME
            ID)
  (CL-WL::DEBUG-LOG! "Event: ~a~%" "up")
  (LET ((ARG-LIST (FOREIGN-ALLOC '(:UNION WL_ARGUMENT) :COUNT 3)))
    (SETF (FOREIGN-SLOT-VALUE
           (MEM-AREF ARG-LIST '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 0)
           '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 'WL-FFI::U)
            SERIAL)
    (SETF (FOREIGN-SLOT-VALUE
           (MEM-AREF ARG-LIST '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 1)
           '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 'WL-FFI::U)
            TIME)
    (SETF (FOREIGN-SLOT-VALUE
           (MEM-AREF ARG-LIST '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 2)
           '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 'WL-FFI::I)
            ID)
    (RESOURCE-POST-EVENT-ARRAY (WL_TOUCH-PTR DISPATCH) 1 ARG-LIST)))

(DEFMETHOD SEND-MOTION
           ((DISPATCH DISPATCH) TIME ID X
            Y)
  (CL-WL::DEBUG-LOG! "Event: ~a~%" "motion")
  (LET ((ARG-LIST (FOREIGN-ALLOC '(:UNION WL_ARGUMENT) :COUNT 4)))
    (SETF (FOREIGN-SLOT-VALUE
           (MEM-AREF ARG-LIST '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 0)
           '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 'WL-FFI::U)
            TIME)
    (SETF (FOREIGN-SLOT-VALUE
           (MEM-AREF ARG-LIST '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 1)
           '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 'WL-FFI::I)
            ID)
    (SETF (FOREIGN-SLOT-VALUE
           (MEM-AREF ARG-LIST '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 2)
           '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 'WL-FFI::F)
            (CL-WL::TO-FIXNUM X))
    (SETF (FOREIGN-SLOT-VALUE
           (MEM-AREF ARG-LIST '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 3)
           '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 'WL-FFI::F)
            (CL-WL::TO-FIXNUM Y))
    (RESOURCE-POST-EVENT-ARRAY (WL_TOUCH-PTR DISPATCH) 2 ARG-LIST)))

(DEFMETHOD SEND-FRAME ((DISPATCH DISPATCH))
  (CL-WL::DEBUG-LOG! "Event: ~a~%" "frame")
  (LET ((ARG-LIST (FOREIGN-ALLOC '(:UNION WL_ARGUMENT) :COUNT 0)))
    (RESOURCE-POST-EVENT-ARRAY (WL_TOUCH-PTR DISPATCH) 3 ARG-LIST)))

(DEFMETHOD SEND-CANCEL ((DISPATCH DISPATCH))
  (CL-WL::DEBUG-LOG! "Event: ~a~%" "cancel")
  (LET ((ARG-LIST (FOREIGN-ALLOC '(:UNION WL_ARGUMENT) :COUNT 0)))
    (RESOURCE-POST-EVENT-ARRAY (WL_TOUCH-PTR DISPATCH) 4 ARG-LIST)))

(DEFMETHOD SEND-SHAPE
           ((DISPATCH DISPATCH) ID MAJOR
            MINOR)
  (CL-WL::DEBUG-LOG! "Event: ~a~%" "shape")
  (LET ((ARG-LIST (FOREIGN-ALLOC '(:UNION WL_ARGUMENT) :COUNT 3)))
    (SETF (FOREIGN-SLOT-VALUE
           (MEM-AREF ARG-LIST '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 0)
           '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 'WL-FFI::I)
            ID)
    (SETF (FOREIGN-SLOT-VALUE
           (MEM-AREF ARG-LIST '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 1)
           '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 'WL-FFI::F)
            (CL-WL::TO-FIXNUM MAJOR))
    (SETF (FOREIGN-SLOT-VALUE
           (MEM-AREF ARG-LIST '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 2)
           '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 'WL-FFI::F)
            (CL-WL::TO-FIXNUM MINOR))
    (RESOURCE-POST-EVENT-ARRAY (WL_TOUCH-PTR DISPATCH) 5 ARG-LIST)))

(DEFMETHOD SEND-ORIENTATION
           ((DISPATCH DISPATCH) ID ORIENTATION)
  (CL-WL::DEBUG-LOG! "Event: ~a~%" "orientation")
  (LET ((ARG-LIST (FOREIGN-ALLOC '(:UNION WL_ARGUMENT) :COUNT 2)))
    (SETF (FOREIGN-SLOT-VALUE
           (MEM-AREF ARG-LIST '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 0)
           '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 'WL-FFI::I)
            ID)
    (SETF (FOREIGN-SLOT-VALUE
           (MEM-AREF ARG-LIST '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 1)
           '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 'WL-FFI::F)
            (CL-WL::TO-FIXNUM ORIENTATION))
    (RESOURCE-POST-EVENT-ARRAY (WL_TOUCH-PTR DISPATCH) 6 ARG-LIST)))

(DEFCLASS GLOBAL (CL-WL:GLOBAL) NIL
          (:DEFAULT-INITARGS :VERSION 9 :DISPATCH-IMPL 'DISPATCH)
          (:DOCUMENTATION "touchscreen input device

The wl_touch interface represents a touchscreen
      associated with a seat.

      Touch interactions can consist of one or more contacts.
      For each contact, a series of events is generated, starting
      with a down event, followed by zero or more motion events,
      and ending with an up event. Events relating to the same
      contact point can be identified by the ID of the sequence.
"))

(DEFMETHOD DISPATCH-BIND ((GLOBAL GLOBAL) CLIENT DATA VERSION ID)
  "Default bind implementation for the wl_touch global object.
This can be overriden by inheritance in case if custom behaviour is required."
  (CL-WL::DEBUG-LOG! "Binding ~a~%" "wl_touch")
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
  (CL-WL::DEBUG-LOG! "Initializing global object: ~a~%" "wl_touch")
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

(IN-PACKAGE :WL_OUTPUT)

(DEFCLASS DISPATCH (CL-WL:OBJECT)
          ((WL_OUTPUT-ID :INITFORM NIL :ACCESSOR
            WL_OUTPUT-ID)
           (WL_OUTPUT-PTR :INITFORM NIL :ACCESSOR
            WL_OUTPUT-PTR))
          (:DEFAULT-INITARGS :VERSION 4)
          (:DOCUMENTATION "compositor output region

An output describes part of the compositor geometry.  The
      compositor works in the 'compositor coordinate system' and an
      output corresponds to a rectangular area in that space that is
      actually visible.  This typically corresponds to a monitor that
      displays part of the compositor space.  This object is published
      as global during start up, or when a monitor is hotplugged.
"))

(DEFGENERIC RELEASE
    (RESOURCE))

(DEFMETHOD CL-WL:DESTROY ((DISPATCH DISPATCH))
  (CL-WL::DEBUG-LOG! "Destroying dispatch object: ~a~%" "wl_output")
  (WHEN (CL-WL::DESTROY-CALLBACK DISPATCH)
    (LOOP FOR CALLBACK IN (CL-WL::DESTROY-CALLBACK DISPATCH)
          DO (FUNCALL CALLBACK DISPATCH)))
  (LET ((RESOURCE-PTR (WL_OUTPUT-PTR DISPATCH)))
    (CL-WL::REMOVE-RESOURCE (POINTER-ADDRESS RESOURCE-PTR)))
  (CL-WL::DN-IF DISPATCH))

(PUSHNEW
 (LIST "wl_output" (LIST)
       (LAMBDA ()
         (SETF *INTERFACE* (FOREIGN-ALLOC '(:STRUCT INTERFACE)))
         (LET ((REQUESTS-PTR
                (LET ((MESSAGES
                       (CFFI:FOREIGN-ALLOC '(:STRUCT CL-WL.FFI:WL_MESSAGE)
                                           :COUNT 1)))
                  (LET ((INTERFACE-ARRAY
                         (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID))
                                             :COUNT 0))
                        (MSG-PTR
                         (MEM-APTR MESSAGES '(:STRUCT CL-WL.FFI:WL_MESSAGE) 0)))
                    (WITH-FOREIGN-SLOTS
                     ((NAME SIGNATURE TYPES) MSG-PTR
                      (:STRUCT CL-WL.FFI:WL_MESSAGE))
                     (SETF NAME (FOREIGN-STRING-ALLOC "release")
                           SIGNATURE "3"
                           TYPES INTERFACE-ARRAY)))
                  MESSAGES))
               (EVENTS-PTR
                (LET ((MESSAGES
                       (CFFI:FOREIGN-ALLOC '(:STRUCT CL-WL.FFI:WL_MESSAGE)
                                           :COUNT 6)))
                  (LET ((INTERFACE-ARRAY
                         (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID))
                                             :COUNT 8))
                        (MSG-PTR
                         (MEM-APTR MESSAGES '(:STRUCT CL-WL.FFI:WL_MESSAGE) 0)))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 0) (NULL-POINTER))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 1) (NULL-POINTER))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 2) (NULL-POINTER))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 3) (NULL-POINTER))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 4) (NULL-POINTER))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 5) (NULL-POINTER))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 6) (NULL-POINTER))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 7) (NULL-POINTER))
                    (WITH-FOREIGN-SLOTS
                     ((NAME SIGNATURE TYPES) MSG-PTR
                      (:STRUCT CL-WL.FFI:WL_MESSAGE))
                     (SETF NAME (FOREIGN-STRING-ALLOC "geometry")
                           SIGNATURE "iiiiussu"
                           TYPES INTERFACE-ARRAY)))
                  (LET ((INTERFACE-ARRAY
                         (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID))
                                             :COUNT 4))
                        (MSG-PTR
                         (MEM-APTR MESSAGES '(:STRUCT CL-WL.FFI:WL_MESSAGE) 1)))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 0) (NULL-POINTER))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 1) (NULL-POINTER))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 2) (NULL-POINTER))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 3) (NULL-POINTER))
                    (WITH-FOREIGN-SLOTS
                     ((NAME SIGNATURE TYPES) MSG-PTR
                      (:STRUCT CL-WL.FFI:WL_MESSAGE))
                     (SETF NAME (FOREIGN-STRING-ALLOC "mode")
                           SIGNATURE "uiii"
                           TYPES INTERFACE-ARRAY)))
                  (LET ((INTERFACE-ARRAY
                         (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID))
                                             :COUNT 0))
                        (MSG-PTR
                         (MEM-APTR MESSAGES '(:STRUCT CL-WL.FFI:WL_MESSAGE) 2)))
                    (WITH-FOREIGN-SLOTS
                     ((NAME SIGNATURE TYPES) MSG-PTR
                      (:STRUCT CL-WL.FFI:WL_MESSAGE))
                     (SETF NAME (FOREIGN-STRING-ALLOC "done")
                           SIGNATURE "2"
                           TYPES INTERFACE-ARRAY)))
                  (LET ((INTERFACE-ARRAY
                         (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID))
                                             :COUNT 1))
                        (MSG-PTR
                         (MEM-APTR MESSAGES '(:STRUCT CL-WL.FFI:WL_MESSAGE) 3)))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 0) (NULL-POINTER))
                    (WITH-FOREIGN-SLOTS
                     ((NAME SIGNATURE TYPES) MSG-PTR
                      (:STRUCT CL-WL.FFI:WL_MESSAGE))
                     (SETF NAME (FOREIGN-STRING-ALLOC "scale")
                           SIGNATURE "2i"
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
                     (SETF NAME (FOREIGN-STRING-ALLOC "name")
                           SIGNATURE "4s"
                           TYPES INTERFACE-ARRAY)))
                  (LET ((INTERFACE-ARRAY
                         (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID))
                                             :COUNT 1))
                        (MSG-PTR
                         (MEM-APTR MESSAGES '(:STRUCT CL-WL.FFI:WL_MESSAGE) 5)))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 0) (NULL-POINTER))
                    (WITH-FOREIGN-SLOTS
                     ((NAME SIGNATURE TYPES) MSG-PTR
                      (:STRUCT CL-WL.FFI:WL_MESSAGE))
                     (SETF NAME (FOREIGN-STRING-ALLOC "description")
                           SIGNATURE "4s"
                           TYPES INTERFACE-ARRAY)))
                  MESSAGES)))
           (WITH-FOREIGN-SLOTS
            ((NAME VERSION METHOD_COUNT METHODS EVENT_COUNT EVENTS) *INTERFACE*
             (:STRUCT INTERFACE))
            (SETF NAME (FOREIGN-STRING-ALLOC "wl_output")
                  VERSION 4
                  METHOD_COUNT 1
                  METHODS REQUESTS-PTR
                  EVENT_COUNT 6
                  EVENTS EVENTS-PTR)))))
 CL-WL::*INTERFACE-INIT-LIST* :TEST #'CL-WL::INTERFACE-EXISTS-TEST)

(DEFCALLBACK DISPATCHER-FFI :INT
 ((DATA :POINTER) (TARGET :POINTER) (OPCODE :UINT) (MESSAGE :POINTER)
  (ARGS :POINTER))
 (DECLARE (IGNORE DATA MESSAGE ARGS))
 (LET ((RESOURCE (GETHASH (POINTER-ADDRESS TARGET) CL-WL::*RESOURCE-TRACKER*)))
   (ECASE OPCODE
     (0
      (CL-WL::DEBUG-LOG! "Dispatching ~a:~a~%" "wl_output" "release")
      (FUNCALL 'RELEASE RESOURCE))))
 0)

(DEFVAR *DISPATCHER* (CALLBACK DISPATCHER-FFI))

(DEFMETHOD SHARED-INITIALIZE :AFTER ((INSTANCE DISPATCH) SLOT-NAMES &KEY ID)
  (DECLARE (IGNORE SLOT-NAMES))
  (UNLESS (WL_OUTPUT-ID INSTANCE)
    (LET* ((RESOURCE
            (CL-WL::CREATE-RESOURCE (CL-WL:PTR (CL-WL:CLIENT INSTANCE))
                                    *INTERFACE* (VERSION INSTANCE)
                                    (OR ID 0))))
      (SETF (CL-WL::TRANSIENT-ID INSTANCE)
              (SETF (WL_OUTPUT-ID INSTANCE)
                      (OR ID (CL-WL.FFI:RESOURCE-GET-ID RESOURCE))))
      (SETF (WL_OUTPUT-PTR INSTANCE) RESOURCE)
      (SETF (GETHASH (POINTER-ADDRESS RESOURCE) CL-WL::*RESOURCE-TRACKER*)
              INSTANCE)
      (RESOURCE-SET-DISPATCHER RESOURCE *DISPATCHER* (NULL-POINTER)
       (NULL-POINTER) (NULL-POINTER)))))

(DEFMETHOD SEND-GEOMETRY
           ((DISPATCH DISPATCH) X Y
            PHYSICAL_WIDTH PHYSICAL_HEIGHT
            SUBPIXEL MAKE MODEL
            TRANSFORM)
  (CL-WL::DEBUG-LOG! "Event: ~a~%" "geometry")
  (LET ((ARG-LIST (FOREIGN-ALLOC '(:UNION WL_ARGUMENT) :COUNT 8)))
    (SETF (FOREIGN-SLOT-VALUE
           (MEM-AREF ARG-LIST '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 0)
           '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 'WL-FFI::I)
            X)
    (SETF (FOREIGN-SLOT-VALUE
           (MEM-AREF ARG-LIST '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 1)
           '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 'WL-FFI::I)
            Y)
    (SETF (FOREIGN-SLOT-VALUE
           (MEM-AREF ARG-LIST '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 2)
           '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 'WL-FFI::I)
            PHYSICAL_WIDTH)
    (SETF (FOREIGN-SLOT-VALUE
           (MEM-AREF ARG-LIST '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 3)
           '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 'WL-FFI::I)
            PHYSICAL_HEIGHT)
    (SETF (FOREIGN-SLOT-VALUE
           (MEM-AREF ARG-LIST '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 4)
           '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 'WL-FFI::U)
            (SUBPIXEL-TO-VALUE SUBPIXEL))
    (SETF (FOREIGN-SLOT-VALUE
           (MEM-AREF ARG-LIST '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 5)
           '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 'WL-FFI::S)
            MAKE)
    (SETF (FOREIGN-SLOT-VALUE
           (MEM-AREF ARG-LIST '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 6)
           '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 'WL-FFI::S)
            MODEL)
    (SETF (FOREIGN-SLOT-VALUE
           (MEM-AREF ARG-LIST '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 7)
           '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 'WL-FFI::U)
            (TRANSFORM-TO-VALUE TRANSFORM))
    (RESOURCE-POST-EVENT-ARRAY (WL_OUTPUT-PTR DISPATCH) 0
     ARG-LIST)))

(DEFMETHOD SEND-MODE
           ((DISPATCH DISPATCH) FLAGS WIDTH
            HEIGHT REFRESH)
  (CL-WL::DEBUG-LOG! "Event: ~a~%" "mode")
  (LET ((ARG-LIST (FOREIGN-ALLOC '(:UNION WL_ARGUMENT) :COUNT 4)))
    (SETF (FOREIGN-SLOT-VALUE
           (MEM-AREF ARG-LIST '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 0)
           '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 'WL-FFI::U)
            (MODE-TO-VALUE FLAGS))
    (SETF (FOREIGN-SLOT-VALUE
           (MEM-AREF ARG-LIST '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 1)
           '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 'WL-FFI::I)
            WIDTH)
    (SETF (FOREIGN-SLOT-VALUE
           (MEM-AREF ARG-LIST '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 2)
           '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 'WL-FFI::I)
            HEIGHT)
    (SETF (FOREIGN-SLOT-VALUE
           (MEM-AREF ARG-LIST '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 3)
           '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 'WL-FFI::I)
            REFRESH)
    (RESOURCE-POST-EVENT-ARRAY (WL_OUTPUT-PTR DISPATCH) 1
     ARG-LIST)))

(DEFMETHOD SEND-DONE ((DISPATCH DISPATCH))
  (CL-WL::DEBUG-LOG! "Event: ~a~%" "done")
  (LET ((ARG-LIST (FOREIGN-ALLOC '(:UNION WL_ARGUMENT) :COUNT 0)))
    (RESOURCE-POST-EVENT-ARRAY (WL_OUTPUT-PTR DISPATCH) 2
     ARG-LIST)))

(DEFMETHOD SEND-SCALE ((DISPATCH DISPATCH) FACTOR)
  (CL-WL::DEBUG-LOG! "Event: ~a~%" "scale")
  (LET ((ARG-LIST (FOREIGN-ALLOC '(:UNION WL_ARGUMENT) :COUNT 1)))
    (SETF (FOREIGN-SLOT-VALUE
           (MEM-AREF ARG-LIST '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 0)
           '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 'WL-FFI::I)
            FACTOR)
    (RESOURCE-POST-EVENT-ARRAY (WL_OUTPUT-PTR DISPATCH) 3
     ARG-LIST)))

(DEFMETHOD SEND-NAME ((DISPATCH DISPATCH) NAME)
  (CL-WL::DEBUG-LOG! "Event: ~a~%" "name")
  (LET ((ARG-LIST (FOREIGN-ALLOC '(:UNION WL_ARGUMENT) :COUNT 1)))
    (SETF (FOREIGN-SLOT-VALUE
           (MEM-AREF ARG-LIST '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 0)
           '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 'WL-FFI::S)
            NAME)
    (RESOURCE-POST-EVENT-ARRAY (WL_OUTPUT-PTR DISPATCH) 4
     ARG-LIST)))

(DEFMETHOD SEND-DESCRIPTION
           ((DISPATCH DISPATCH) DESCRIPTION)
  (CL-WL::DEBUG-LOG! "Event: ~a~%" "description")
  (LET ((ARG-LIST (FOREIGN-ALLOC '(:UNION WL_ARGUMENT) :COUNT 1)))
    (SETF (FOREIGN-SLOT-VALUE
           (MEM-AREF ARG-LIST '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 0)
           '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 'WL-FFI::S)
            DESCRIPTION)
    (RESOURCE-POST-EVENT-ARRAY (WL_OUTPUT-PTR DISPATCH) 5
     ARG-LIST)))

(DEFUN SUBPIXEL-FROM-VALUE (NUMBER)
  (LOOP FOR ENTRY IN '((0 :UNKNOWN) (1 :NONE)
                       (2 :HORIZONTAL-RGB)
                       (3 :HORIZONTAL-BGR)
                       (4 :VERTICAL-RGB)
                       (5 :VERTICAL-BGR))
        FOR VALUE = (CAR ENTRY)
        FOR KEYWORD = (CADR ENTRY)
        WHEN (EQ NUMBER VALUE) RETURN KEYWORD
        FINALLY (ERROR (FORMAT NIL "Unknown enum value: ~a" NUMBER))))

(DEFUN SUBPIXEL-TO-VALUE (KEY)
  (LOOP FOR ENTRY IN '((0 :UNKNOWN) (1 :NONE)
                       (2 :HORIZONTAL-RGB)
                       (3 :HORIZONTAL-BGR)
                       (4 :VERTICAL-RGB)
                       (5 :VERTICAL-BGR))
        FOR VALUE = (CAR ENTRY)
        FOR KEYWORD = (CADR ENTRY)
        WHEN (EQ KEY KEYWORD) RETURN VALUE
        FINALLY (ERROR (FORMAT NIL "Unknown enum keyword: ~a" KEY))))

(DEFUN TRANSFORM-FROM-VALUE (NUMBER)
  (LOOP FOR ENTRY IN '((0 :NORMAL) (1 :90)
                       (2 :180) (3 :270)
                       (4 :FLIPPED) (5 :FLIPPED-90)
                       (6 :FLIPPED-180)
                       (7 :FLIPPED-270))
        FOR VALUE = (CAR ENTRY)
        FOR KEYWORD = (CADR ENTRY)
        WHEN (EQ NUMBER VALUE) RETURN KEYWORD
        FINALLY (ERROR (FORMAT NIL "Unknown enum value: ~a" NUMBER))))

(DEFUN TRANSFORM-TO-VALUE (KEY)
  (LOOP FOR ENTRY IN '((0 :NORMAL) (1 :90)
                       (2 :180) (3 :270)
                       (4 :FLIPPED) (5 :FLIPPED-90)
                       (6 :FLIPPED-180)
                       (7 :FLIPPED-270))
        FOR VALUE = (CAR ENTRY)
        FOR KEYWORD = (CADR ENTRY)
        WHEN (EQ KEY KEYWORD) RETURN VALUE
        FINALLY (ERROR (FORMAT NIL "Unknown enum keyword: ~a" KEY))))

(DEFUN MODE-FROM-VALUE (BITS)
  (LOOP FOR ENTRY IN '((1 :CURRENT) (2 :PREFERRED))
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

(DEFUN MODE-TO-VALUE (KEYWORDS)
  (REDUCE #'+ KEYWORDS :KEY
          (LAMBDA (KEYWORD)
            (OR
             (CADR
              (ASSOC KEYWORD
                     '((:CURRENT 1) (:PREFERRED 2))))
             0))))

(DEFCLASS GLOBAL (CL-WL:GLOBAL) NIL
          (:DEFAULT-INITARGS :VERSION 4 :DISPATCH-IMPL 'DISPATCH)
          (:DOCUMENTATION "compositor output region

An output describes part of the compositor geometry.  The
      compositor works in the 'compositor coordinate system' and an
      output corresponds to a rectangular area in that space that is
      actually visible.  This typically corresponds to a monitor that
      displays part of the compositor space.  This object is published
      as global during start up, or when a monitor is hotplugged.
"))

(DEFMETHOD DISPATCH-BIND ((GLOBAL GLOBAL) CLIENT DATA VERSION ID)
  "Default bind implementation for the wl_output global object.
This can be overriden by inheritance in case if custom behaviour is required."
  (CL-WL::DEBUG-LOG! "Binding ~a~%" "wl_output")
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
  (CL-WL::DEBUG-LOG! "Initializing global object: ~a~%" "wl_output")
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

(IN-PACKAGE :WL_REGION)

(DEFCLASS DISPATCH (CL-WL:OBJECT)
          ((WL_REGION-ID :INITFORM NIL :ACCESSOR
            WL_REGION-ID)
           (WL_REGION-PTR :INITFORM NIL :ACCESSOR
            WL_REGION-PTR))
          (:DEFAULT-INITARGS :VERSION 1)
          (:DOCUMENTATION "region interface

A region object describes an area.

      Region objects are used to describe the opaque and input
      regions of a surface.
"))

(DEFGENERIC DESTROY
    (RESOURCE))

(DEFGENERIC ADD
    (RESOURCE X Y WIDTH HEIGHT))

(DEFGENERIC SUBTRACT
    (RESOURCE X Y WIDTH HEIGHT))

(DEFMETHOD CL-WL:DESTROY ((DISPATCH DISPATCH))
  (CL-WL::DEBUG-LOG! "Destroying dispatch object: ~a~%" "wl_region")
  (WHEN (CL-WL::DESTROY-CALLBACK DISPATCH)
    (LOOP FOR CALLBACK IN (CL-WL::DESTROY-CALLBACK DISPATCH)
          DO (FUNCALL CALLBACK DISPATCH)))
  (LET ((RESOURCE-PTR (WL_REGION-PTR DISPATCH)))
    (CL-WL::REMOVE-RESOURCE (POINTER-ADDRESS RESOURCE-PTR)))
  (CL-WL::DN-IF DISPATCH))

(DEFMETHOD DESTROY ((DISPATCH DISPATCH)) (CL-WL:DESTROY DISPATCH))

(DEFMETHOD DESTROY (EMPTY) NIL)

(PUSHNEW
 (LIST "wl_region" (LIST)
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
                                             :COUNT 4))
                        (MSG-PTR
                         (MEM-APTR MESSAGES '(:STRUCT CL-WL.FFI:WL_MESSAGE) 1)))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 0) (NULL-POINTER))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 1) (NULL-POINTER))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 2) (NULL-POINTER))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 3) (NULL-POINTER))
                    (WITH-FOREIGN-SLOTS
                     ((NAME SIGNATURE TYPES) MSG-PTR
                      (:STRUCT CL-WL.FFI:WL_MESSAGE))
                     (SETF NAME (FOREIGN-STRING-ALLOC "add")
                           SIGNATURE "iiii"
                           TYPES INTERFACE-ARRAY)))
                  (LET ((INTERFACE-ARRAY
                         (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID))
                                             :COUNT 4))
                        (MSG-PTR
                         (MEM-APTR MESSAGES '(:STRUCT CL-WL.FFI:WL_MESSAGE) 2)))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 0) (NULL-POINTER))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 1) (NULL-POINTER))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 2) (NULL-POINTER))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 3) (NULL-POINTER))
                    (WITH-FOREIGN-SLOTS
                     ((NAME SIGNATURE TYPES) MSG-PTR
                      (:STRUCT CL-WL.FFI:WL_MESSAGE))
                     (SETF NAME (FOREIGN-STRING-ALLOC "subtract")
                           SIGNATURE "iiii"
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
            (SETF NAME (FOREIGN-STRING-ALLOC "wl_region")
                  VERSION 1
                  METHOD_COUNT 3
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
      (CL-WL::DEBUG-LOG! "Dispatching ~a:~a~%" "wl_region" "destroy")
      (FUNCALL 'DESTROY RESOURCE))
     (1
      (CL-WL::DEBUG-LOG! "Dispatching ~a:~a~%" "wl_region" "add")
      (FUNCALL 'ADD RESOURCE
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
     (2
      (CL-WL::DEBUG-LOG! "Dispatching ~a:~a~%" "wl_region" "subtract")
      (FUNCALL 'SUBTRACT RESOURCE
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
                 '(:UNION CL-WL.FFI:WL_ARGUMENT) 'WL-FFI::I))))))
 0)

(DEFVAR *DISPATCHER* (CALLBACK DISPATCHER-FFI))

(DEFMETHOD SHARED-INITIALIZE :AFTER ((INSTANCE DISPATCH) SLOT-NAMES &KEY ID)
  (DECLARE (IGNORE SLOT-NAMES))
  (UNLESS (WL_REGION-ID INSTANCE)
    (LET* ((RESOURCE
            (CL-WL::CREATE-RESOURCE (CL-WL:PTR (CL-WL:CLIENT INSTANCE))
                                    *INTERFACE* (VERSION INSTANCE)
                                    (OR ID 0))))
      (SETF (CL-WL::TRANSIENT-ID INSTANCE)
              (SETF (WL_REGION-ID INSTANCE)
                      (OR ID (CL-WL.FFI:RESOURCE-GET-ID RESOURCE))))
      (SETF (WL_REGION-PTR INSTANCE) RESOURCE)
      (SETF (GETHASH (POINTER-ADDRESS RESOURCE) CL-WL::*RESOURCE-TRACKER*)
              INSTANCE)
      (RESOURCE-SET-DISPATCHER RESOURCE *DISPATCHER* (NULL-POINTER)
       (NULL-POINTER) (NULL-POINTER)))))

(DEFCLASS GLOBAL (CL-WL:GLOBAL) NIL
          (:DEFAULT-INITARGS :VERSION 1 :DISPATCH-IMPL 'DISPATCH)
          (:DOCUMENTATION "region interface

A region object describes an area.

      Region objects are used to describe the opaque and input
      regions of a surface.
"))

(DEFMETHOD DISPATCH-BIND ((GLOBAL GLOBAL) CLIENT DATA VERSION ID)
  "Default bind implementation for the wl_region global object.
This can be overriden by inheritance in case if custom behaviour is required."
  (CL-WL::DEBUG-LOG! "Binding ~a~%" "wl_region")
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
  (CL-WL::DEBUG-LOG! "Initializing global object: ~a~%" "wl_region")
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

(IN-PACKAGE :WL_SUBCOMPOSITOR)

(DEFCLASS DISPATCH (CL-WL:OBJECT)
          ((WL_SUBCOMPOSITOR-ID :INITFORM NIL :ACCESSOR
            WL_SUBCOMPOSITOR-ID)
           (WL_SUBCOMPOSITOR-PTR :INITFORM NIL :ACCESSOR
            WL_SUBCOMPOSITOR-PTR))
          (:DEFAULT-INITARGS :VERSION 1)
          (:DOCUMENTATION "sub-surface compositing

The global interface exposing sub-surface compositing capabilities.
      A wl_surface, that has sub-surfaces associated, is called the
      parent surface. Sub-surfaces can be arbitrarily nested and create
      a tree of sub-surfaces.

      The root surface in a tree of sub-surfaces is the main
      surface. The main surface cannot be a sub-surface, because
      sub-surfaces must always have a parent.

      A main surface with its sub-surfaces forms a (compound) window.
      For window management purposes, this set of wl_surface objects is
      to be considered as a single window, and it should also behave as
      such.

      The aim of sub-surfaces is to offload some of the compositing work
      within a window from clients to the compositor. A prime example is
      a video player with decorations and video in separate wl_surface
      objects. This should allow the compositor to pass YUV video buffer
      processing to dedicated overlay hardware when possible.
"))

(DEFGENERIC DESTROY
    (RESOURCE))

(DEFGENERIC GET-SUBSURFACE
    (RESOURCE ID SURFACE PARENT))

(DEFMETHOD CL-WL:DESTROY ((DISPATCH DISPATCH))
  (CL-WL::DEBUG-LOG! "Destroying dispatch object: ~a~%" "wl_subcompositor")
  (WHEN (CL-WL::DESTROY-CALLBACK DISPATCH)
    (LOOP FOR CALLBACK IN (CL-WL::DESTROY-CALLBACK DISPATCH)
          DO (FUNCALL CALLBACK DISPATCH)))
  (LET ((RESOURCE-PTR (WL_SUBCOMPOSITOR-PTR DISPATCH)))
    (CL-WL::REMOVE-RESOURCE (POINTER-ADDRESS RESOURCE-PTR)))
  (CL-WL::DN-IF DISPATCH))

(DEFMETHOD DESTROY ((DISPATCH DISPATCH)) (CL-WL:DESTROY DISPATCH))

(DEFMETHOD DESTROY (EMPTY) NIL)

(PUSHNEW
 (LIST "wl_subcompositor" (LIST "wl_subsurface" "wl_surface")
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
                                             :COUNT 3))
                        (MSG-PTR
                         (MEM-APTR MESSAGES '(:STRUCT CL-WL.FFI:WL_MESSAGE) 1)))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 0)
                            WL_SUBSURFACE::*INTERFACE*)
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 1)
                            WL_SURFACE::*INTERFACE*)
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 2)
                            WL_SURFACE::*INTERFACE*)
                    (WITH-FOREIGN-SLOTS
                     ((NAME SIGNATURE TYPES) MSG-PTR
                      (:STRUCT CL-WL.FFI:WL_MESSAGE))
                     (SETF NAME (FOREIGN-STRING-ALLOC "get_subsurface")
                           SIGNATURE "noo"
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
            (SETF NAME (FOREIGN-STRING-ALLOC "wl_subcompositor")
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
   (ECASE OPCODE
     (0
      (CL-WL::DEBUG-LOG! "Dispatching ~a:~a~%" "wl_subcompositor" "destroy")
      (FUNCALL 'DESTROY RESOURCE))
     (1
      (CL-WL::DEBUG-LOG! "Dispatching ~a:~a~%" "wl_subcompositor"
                         "get-subsurface")
      (FUNCALL 'GET-SUBSURFACE RESOURCE
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
                CL-WL::*RESOURCE-TRACKER*)))))
 0)

(DEFVAR *DISPATCHER* (CALLBACK DISPATCHER-FFI))

(DEFMETHOD SHARED-INITIALIZE :AFTER ((INSTANCE DISPATCH) SLOT-NAMES &KEY ID)
  (DECLARE (IGNORE SLOT-NAMES))
  (UNLESS (WL_SUBCOMPOSITOR-ID INSTANCE)
    (LET* ((RESOURCE
            (CL-WL::CREATE-RESOURCE (CL-WL:PTR (CL-WL:CLIENT INSTANCE))
                                    *INTERFACE* (VERSION INSTANCE)
                                    (OR ID 0))))
      (SETF (CL-WL::TRANSIENT-ID INSTANCE)
              (SETF (WL_SUBCOMPOSITOR-ID INSTANCE)
                      (OR ID (CL-WL.FFI:RESOURCE-GET-ID RESOURCE))))
      (SETF (WL_SUBCOMPOSITOR-PTR INSTANCE) RESOURCE)
      (SETF (GETHASH (POINTER-ADDRESS RESOURCE) CL-WL::*RESOURCE-TRACKER*)
              INSTANCE)
      (RESOURCE-SET-DISPATCHER RESOURCE *DISPATCHER* (NULL-POINTER)
       (NULL-POINTER) (NULL-POINTER)))))

(DEFUN ERROR-FROM-VALUE (NUMBER)
  (LOOP FOR ENTRY IN '((0 :BAD-SURFACE)
                       (1 :BAD-PARENT))
        FOR VALUE = (CAR ENTRY)
        FOR KEYWORD = (CADR ENTRY)
        WHEN (EQ NUMBER VALUE) RETURN KEYWORD
        FINALLY (ERROR (FORMAT NIL "Unknown enum value: ~a" NUMBER))))

(DEFUN ERROR-TO-VALUE (KEY)
  (LOOP FOR ENTRY IN '((0 :BAD-SURFACE)
                       (1 :BAD-PARENT))
        FOR VALUE = (CAR ENTRY)
        FOR KEYWORD = (CADR ENTRY)
        WHEN (EQ KEY KEYWORD) RETURN VALUE
        FINALLY (ERROR (FORMAT NIL "Unknown enum keyword: ~a" KEY))))

(DEFCLASS GLOBAL (CL-WL:GLOBAL) NIL
          (:DEFAULT-INITARGS :VERSION 1 :DISPATCH-IMPL 'DISPATCH)
          (:DOCUMENTATION "sub-surface compositing

The global interface exposing sub-surface compositing capabilities.
      A wl_surface, that has sub-surfaces associated, is called the
      parent surface. Sub-surfaces can be arbitrarily nested and create
      a tree of sub-surfaces.

      The root surface in a tree of sub-surfaces is the main
      surface. The main surface cannot be a sub-surface, because
      sub-surfaces must always have a parent.

      A main surface with its sub-surfaces forms a (compound) window.
      For window management purposes, this set of wl_surface objects is
      to be considered as a single window, and it should also behave as
      such.

      The aim of sub-surfaces is to offload some of the compositing work
      within a window from clients to the compositor. A prime example is
      a video player with decorations and video in separate wl_surface
      objects. This should allow the compositor to pass YUV video buffer
      processing to dedicated overlay hardware when possible.
"))

(DEFMETHOD DISPATCH-BIND ((GLOBAL GLOBAL) CLIENT DATA VERSION ID)
  "Default bind implementation for the wl_subcompositor global object.
This can be overriden by inheritance in case if custom behaviour is required."
  (CL-WL::DEBUG-LOG! "Binding ~a~%" "wl_subcompositor")
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
  (CL-WL::DEBUG-LOG! "Initializing global object: ~a~%" "wl_subcompositor")
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

(IN-PACKAGE :WL_SUBSURFACE)

(DEFCLASS DISPATCH (CL-WL:OBJECT)
          ((WL_SUBSURFACE-ID :INITFORM NIL :ACCESSOR
            WL_SUBSURFACE-ID)
           (WL_SUBSURFACE-PTR :INITFORM NIL :ACCESSOR
            WL_SUBSURFACE-PTR))
          (:DEFAULT-INITARGS :VERSION 1)
          (:DOCUMENTATION "sub-surface interface to a wl_surface

An additional interface to a wl_surface object, which has been
      made a sub-surface. A sub-surface has one parent surface. A
      sub-surface's size and position are not limited to that of the parent.
      Particularly, a sub-surface is not automatically clipped to its
      parent's area.

      A sub-surface becomes mapped, when a non-NULL wl_buffer is applied
      and the parent surface is mapped. The order of which one happens
      first is irrelevant. A sub-surface is hidden if the parent becomes
      hidden, or if a NULL wl_buffer is applied. These rules apply
      recursively through the tree of surfaces.

      The behaviour of a wl_surface.commit request on a sub-surface
      depends on the sub-surface's mode. The possible modes are
      synchronized and desynchronized, see methods
      wl_subsurface.set_sync and wl_subsurface.set_desync. Synchronized
      mode caches the wl_surface state to be applied when the parent's
      state gets applied, and desynchronized mode applies the pending
      wl_surface state directly. A sub-surface is initially in the
      synchronized mode.

      Sub-surfaces also have another kind of state, which is managed by
      wl_subsurface requests, as opposed to wl_surface requests. This
      state includes the sub-surface position relative to the parent
      surface (wl_subsurface.set_position), and the stacking order of
      the parent and its sub-surfaces (wl_subsurface.place_above and
      .place_below). This state is applied when the parent surface's
      wl_surface state is applied, regardless of the sub-surface's mode.
      As the exception, set_sync and set_desync are effective immediately.

      The main surface can be thought to be always in desynchronized mode,
      since it does not have a parent in the sub-surfaces sense.

      Even if a sub-surface is in desynchronized mode, it will behave as
      in synchronized mode, if its parent surface behaves as in
      synchronized mode. This rule is applied recursively throughout the
      tree of surfaces. This means, that one can set a sub-surface into
      synchronized mode, and then assume that all its child and grand-child
      sub-surfaces are synchronized, too, without explicitly setting them.

      Destroying a sub-surface takes effect immediately. If you need to
      synchronize the removal of a sub-surface to the parent surface update,
      unmap the sub-surface first by attaching a NULL wl_buffer, update parent,
      and then destroy the sub-surface.

      If the parent wl_surface object is destroyed, the sub-surface is
      unmapped.
"))

(DEFGENERIC DESTROY
    (RESOURCE))

(DEFGENERIC SET-POSITION
    (RESOURCE X Y))

(DEFGENERIC PLACE-ABOVE
    (RESOURCE SIBLING))

(DEFGENERIC PLACE-BELOW
    (RESOURCE SIBLING))

(DEFGENERIC SET-SYNC
    (RESOURCE))

(DEFGENERIC SET-DESYNC
    (RESOURCE))

(DEFMETHOD CL-WL:DESTROY ((DISPATCH DISPATCH))
  (CL-WL::DEBUG-LOG! "Destroying dispatch object: ~a~%" "wl_subsurface")
  (WHEN (CL-WL::DESTROY-CALLBACK DISPATCH)
    (LOOP FOR CALLBACK IN (CL-WL::DESTROY-CALLBACK DISPATCH)
          DO (FUNCALL CALLBACK DISPATCH)))
  (LET ((RESOURCE-PTR (WL_SUBSURFACE-PTR DISPATCH)))
    (CL-WL::REMOVE-RESOURCE (POINTER-ADDRESS RESOURCE-PTR)))
  (CL-WL::DN-IF DISPATCH))

(DEFMETHOD DESTROY ((DISPATCH DISPATCH)) (CL-WL:DESTROY DISPATCH))

(DEFMETHOD DESTROY (EMPTY) NIL)

(PUSHNEW
 (LIST "wl_subsurface" (LIST "wl_surface")
       (LAMBDA ()
         (SETF *INTERFACE* (FOREIGN-ALLOC '(:STRUCT INTERFACE)))
         (LET ((REQUESTS-PTR
                (LET ((MESSAGES
                       (CFFI:FOREIGN-ALLOC '(:STRUCT CL-WL.FFI:WL_MESSAGE)
                                           :COUNT 6)))
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
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 0) (NULL-POINTER))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 1) (NULL-POINTER))
                    (WITH-FOREIGN-SLOTS
                     ((NAME SIGNATURE TYPES) MSG-PTR
                      (:STRUCT CL-WL.FFI:WL_MESSAGE))
                     (SETF NAME (FOREIGN-STRING-ALLOC "set_position")
                           SIGNATURE "ii"
                           TYPES INTERFACE-ARRAY)))
                  (LET ((INTERFACE-ARRAY
                         (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID))
                                             :COUNT 1))
                        (MSG-PTR
                         (MEM-APTR MESSAGES '(:STRUCT CL-WL.FFI:WL_MESSAGE) 2)))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 0)
                            WL_SURFACE::*INTERFACE*)
                    (WITH-FOREIGN-SLOTS
                     ((NAME SIGNATURE TYPES) MSG-PTR
                      (:STRUCT CL-WL.FFI:WL_MESSAGE))
                     (SETF NAME (FOREIGN-STRING-ALLOC "place_above")
                           SIGNATURE "o"
                           TYPES INTERFACE-ARRAY)))
                  (LET ((INTERFACE-ARRAY
                         (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID))
                                             :COUNT 1))
                        (MSG-PTR
                         (MEM-APTR MESSAGES '(:STRUCT CL-WL.FFI:WL_MESSAGE) 3)))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 0)
                            WL_SURFACE::*INTERFACE*)
                    (WITH-FOREIGN-SLOTS
                     ((NAME SIGNATURE TYPES) MSG-PTR
                      (:STRUCT CL-WL.FFI:WL_MESSAGE))
                     (SETF NAME (FOREIGN-STRING-ALLOC "place_below")
                           SIGNATURE "o"
                           TYPES INTERFACE-ARRAY)))
                  (LET ((INTERFACE-ARRAY
                         (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID))
                                             :COUNT 0))
                        (MSG-PTR
                         (MEM-APTR MESSAGES '(:STRUCT CL-WL.FFI:WL_MESSAGE) 4)))
                    (WITH-FOREIGN-SLOTS
                     ((NAME SIGNATURE TYPES) MSG-PTR
                      (:STRUCT CL-WL.FFI:WL_MESSAGE))
                     (SETF NAME (FOREIGN-STRING-ALLOC "set_sync")
                           SIGNATURE ""
                           TYPES INTERFACE-ARRAY)))
                  (LET ((INTERFACE-ARRAY
                         (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID))
                                             :COUNT 0))
                        (MSG-PTR
                         (MEM-APTR MESSAGES '(:STRUCT CL-WL.FFI:WL_MESSAGE) 5)))
                    (WITH-FOREIGN-SLOTS
                     ((NAME SIGNATURE TYPES) MSG-PTR
                      (:STRUCT CL-WL.FFI:WL_MESSAGE))
                     (SETF NAME (FOREIGN-STRING-ALLOC "set_desync")
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
            (SETF NAME (FOREIGN-STRING-ALLOC "wl_subsurface")
                  VERSION 1
                  METHOD_COUNT 6
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
      (CL-WL::DEBUG-LOG! "Dispatching ~a:~a~%" "wl_subsurface" "destroy")
      (FUNCALL 'DESTROY RESOURCE))
     (1
      (CL-WL::DEBUG-LOG! "Dispatching ~a:~a~%" "wl_subsurface" "set-position")
      (FUNCALL 'SET-POSITION RESOURCE
               (VALUES
                (FOREIGN-SLOT-VALUE
                 (MEM-APTR ARGS '(:UNION CL-WL.FFI:WL_ARGUMENT) 0)
                 '(:UNION CL-WL.FFI:WL_ARGUMENT) 'WL-FFI::I))
               (VALUES
                (FOREIGN-SLOT-VALUE
                 (MEM-APTR ARGS '(:UNION CL-WL.FFI:WL_ARGUMENT) 1)
                 '(:UNION CL-WL.FFI:WL_ARGUMENT) 'WL-FFI::I))))
     (2
      (CL-WL::DEBUG-LOG! "Dispatching ~a:~a~%" "wl_subsurface" "place-above")
      (FUNCALL 'PLACE-ABOVE RESOURCE
               (GETHASH
                (POINTER-ADDRESS
                 (FOREIGN-SLOT-VALUE
                  (MEM-APTR ARGS '(:UNION CL-WL.FFI:WL_ARGUMENT) 0)
                  '(:UNION CL-WL.FFI:WL_ARGUMENT) 'WL-FFI::O))
                CL-WL::*RESOURCE-TRACKER*)))
     (3
      (CL-WL::DEBUG-LOG! "Dispatching ~a:~a~%" "wl_subsurface" "place-below")
      (FUNCALL 'PLACE-BELOW RESOURCE
               (GETHASH
                (POINTER-ADDRESS
                 (FOREIGN-SLOT-VALUE
                  (MEM-APTR ARGS '(:UNION CL-WL.FFI:WL_ARGUMENT) 0)
                  '(:UNION CL-WL.FFI:WL_ARGUMENT) 'WL-FFI::O))
                CL-WL::*RESOURCE-TRACKER*)))
     (4
      (CL-WL::DEBUG-LOG! "Dispatching ~a:~a~%" "wl_subsurface" "set-sync")
      (FUNCALL 'SET-SYNC RESOURCE))
     (5
      (CL-WL::DEBUG-LOG! "Dispatching ~a:~a~%" "wl_subsurface" "set-desync")
      (FUNCALL 'SET-DESYNC RESOURCE))))
 0)

(DEFVAR *DISPATCHER* (CALLBACK DISPATCHER-FFI))

(DEFMETHOD SHARED-INITIALIZE :AFTER ((INSTANCE DISPATCH) SLOT-NAMES &KEY ID)
  (DECLARE (IGNORE SLOT-NAMES))
  (UNLESS (WL_SUBSURFACE-ID INSTANCE)
    (LET* ((RESOURCE
            (CL-WL::CREATE-RESOURCE (CL-WL:PTR (CL-WL:CLIENT INSTANCE))
                                    *INTERFACE* (VERSION INSTANCE)
                                    (OR ID 0))))
      (SETF (CL-WL::TRANSIENT-ID INSTANCE)
              (SETF (WL_SUBSURFACE-ID INSTANCE)
                      (OR ID (CL-WL.FFI:RESOURCE-GET-ID RESOURCE))))
      (SETF (WL_SUBSURFACE-PTR INSTANCE) RESOURCE)
      (SETF (GETHASH (POINTER-ADDRESS RESOURCE) CL-WL::*RESOURCE-TRACKER*)
              INSTANCE)
      (RESOURCE-SET-DISPATCHER RESOURCE *DISPATCHER* (NULL-POINTER)
       (NULL-POINTER) (NULL-POINTER)))))

(DEFUN ERROR-FROM-VALUE (NUMBER)
  (LOOP FOR ENTRY IN '((0 :BAD-SURFACE))
        FOR VALUE = (CAR ENTRY)
        FOR KEYWORD = (CADR ENTRY)
        WHEN (EQ NUMBER VALUE) RETURN KEYWORD
        FINALLY (ERROR (FORMAT NIL "Unknown enum value: ~a" NUMBER))))

(DEFUN ERROR-TO-VALUE (KEY)
  (LOOP FOR ENTRY IN '((0 :BAD-SURFACE))
        FOR VALUE = (CAR ENTRY)
        FOR KEYWORD = (CADR ENTRY)
        WHEN (EQ KEY KEYWORD) RETURN VALUE
        FINALLY (ERROR (FORMAT NIL "Unknown enum keyword: ~a" KEY))))

(DEFCLASS GLOBAL (CL-WL:GLOBAL) NIL
          (:DEFAULT-INITARGS :VERSION 1 :DISPATCH-IMPL 'DISPATCH)
          (:DOCUMENTATION "sub-surface interface to a wl_surface

An additional interface to a wl_surface object, which has been
      made a sub-surface. A sub-surface has one parent surface. A
      sub-surface's size and position are not limited to that of the parent.
      Particularly, a sub-surface is not automatically clipped to its
      parent's area.

      A sub-surface becomes mapped, when a non-NULL wl_buffer is applied
      and the parent surface is mapped. The order of which one happens
      first is irrelevant. A sub-surface is hidden if the parent becomes
      hidden, or if a NULL wl_buffer is applied. These rules apply
      recursively through the tree of surfaces.

      The behaviour of a wl_surface.commit request on a sub-surface
      depends on the sub-surface's mode. The possible modes are
      synchronized and desynchronized, see methods
      wl_subsurface.set_sync and wl_subsurface.set_desync. Synchronized
      mode caches the wl_surface state to be applied when the parent's
      state gets applied, and desynchronized mode applies the pending
      wl_surface state directly. A sub-surface is initially in the
      synchronized mode.

      Sub-surfaces also have another kind of state, which is managed by
      wl_subsurface requests, as opposed to wl_surface requests. This
      state includes the sub-surface position relative to the parent
      surface (wl_subsurface.set_position), and the stacking order of
      the parent and its sub-surfaces (wl_subsurface.place_above and
      .place_below). This state is applied when the parent surface's
      wl_surface state is applied, regardless of the sub-surface's mode.
      As the exception, set_sync and set_desync are effective immediately.

      The main surface can be thought to be always in desynchronized mode,
      since it does not have a parent in the sub-surfaces sense.

      Even if a sub-surface is in desynchronized mode, it will behave as
      in synchronized mode, if its parent surface behaves as in
      synchronized mode. This rule is applied recursively throughout the
      tree of surfaces. This means, that one can set a sub-surface into
      synchronized mode, and then assume that all its child and grand-child
      sub-surfaces are synchronized, too, without explicitly setting them.

      Destroying a sub-surface takes effect immediately. If you need to
      synchronize the removal of a sub-surface to the parent surface update,
      unmap the sub-surface first by attaching a NULL wl_buffer, update parent,
      and then destroy the sub-surface.

      If the parent wl_surface object is destroyed, the sub-surface is
      unmapped.
"))

(DEFMETHOD DISPATCH-BIND ((GLOBAL GLOBAL) CLIENT DATA VERSION ID)
  "Default bind implementation for the wl_subsurface global object.
This can be overriden by inheritance in case if custom behaviour is required."
  (CL-WL::DEBUG-LOG! "Binding ~a~%" "wl_subsurface")
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
  (CL-WL::DEBUG-LOG! "Initializing global object: ~a~%" "wl_subsurface")
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
