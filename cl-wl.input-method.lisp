(DEFPACKAGE :ZWP_INPUT_METHOD_V2
  (:USE :CL :CFFI :CL-WL.FFI)
  (:NICKNAMES :ZWP-INPUT-METHOD-V2)
  (:EXPORT DISPATCH
           GLOBAL
           DISPATCH-BIND
           COMMIT-STRING
           SET-PREEDIT-STRING
           DELETE-SURROUNDING-TEXT
           COMMIT
           GET-INPUT-POPUP-SURFACE
           GRAB-KEYBOARD
           DESTROY
           SEND-ACTIVATE
           SEND-DEACTIVATE
           SEND-SURROUNDING-TEXT
           SEND-TEXT-CHANGE-CAUSE
           SEND-CONTENT-TYPE
           SEND-DONE
           SEND-UNAVAILABLE))

(IN-PACKAGE :ZWP_INPUT_METHOD_V2)

(DEFCSTRUCT INTERFACE (NAME :STRING) (VERSION :INT) (METHOD_COUNT :INT)
 (METHODS (:POINTER (:STRUCT CL-WL.FFI:WL_MESSAGE))) (EVENT_COUNT :INT)
 (EVENTS (:POINTER (:STRUCT CL-WL.FFI:WL_MESSAGE))))

(DEFVAR *INTERFACE* NIL)

(DEFPACKAGE :ZWP_INPUT_POPUP_SURFACE_V2
  (:USE :CL :CFFI :CL-WL.FFI)
  (:NICKNAMES :ZWP-INPUT-POPUP-SURFACE-V2)
  (:EXPORT DISPATCH
           GLOBAL
           DISPATCH-BIND
           DESTROY
           SEND-TEXT-INPUT-RECTANGLE))

(IN-PACKAGE :ZWP_INPUT_POPUP_SURFACE_V2)

(DEFCSTRUCT INTERFACE (NAME :STRING) (VERSION :INT) (METHOD_COUNT :INT)
 (METHODS (:POINTER (:STRUCT CL-WL.FFI:WL_MESSAGE))) (EVENT_COUNT :INT)
 (EVENTS (:POINTER (:STRUCT CL-WL.FFI:WL_MESSAGE))))

(DEFVAR *INTERFACE* NIL)

(DEFPACKAGE :ZWP_INPUT_METHOD_KEYBOARD_GRAB_V2
  (:USE :CL :CFFI :CL-WL.FFI)
  (:NICKNAMES :ZWP-INPUT-METHOD-KEYBOARD-GRAB-V2)
  (:EXPORT DISPATCH
           GLOBAL
           DISPATCH-BIND
           RELEASE
           SEND-KEYMAP
           SEND-KEY
           SEND-MODIFIERS
           SEND-REPEAT-INFO))

(IN-PACKAGE :ZWP_INPUT_METHOD_KEYBOARD_GRAB_V2)

(DEFCSTRUCT INTERFACE (NAME :STRING) (VERSION :INT) (METHOD_COUNT :INT)
 (METHODS (:POINTER (:STRUCT CL-WL.FFI:WL_MESSAGE))) (EVENT_COUNT :INT)
 (EVENTS (:POINTER (:STRUCT CL-WL.FFI:WL_MESSAGE))))

(DEFVAR *INTERFACE* NIL)

(DEFPACKAGE :ZWP_INPUT_METHOD_MANAGER_V2
  (:USE :CL :CFFI :CL-WL.FFI)
  (:NICKNAMES :ZWP-INPUT-METHOD-MANAGER-V2)
  (:EXPORT DISPATCH
           GLOBAL
           DISPATCH-BIND
           GET-INPUT-METHOD
           DESTROY))

(IN-PACKAGE :ZWP_INPUT_METHOD_MANAGER_V2)

(DEFCSTRUCT INTERFACE (NAME :STRING) (VERSION :INT) (METHOD_COUNT :INT)
 (METHODS (:POINTER (:STRUCT CL-WL.FFI:WL_MESSAGE))) (EVENT_COUNT :INT)
 (EVENTS (:POINTER (:STRUCT CL-WL.FFI:WL_MESSAGE))))

(DEFVAR *INTERFACE* NIL)

(IN-PACKAGE :ZWP_INPUT_METHOD_V2)

(DEFCLASS DISPATCH (CL-WL:OBJECT)
          ((ZWP_INPUT_METHOD_V2-ID :INITFORM NIL :ACCESSOR
            ZWP_INPUT_METHOD_V2-ID)
           (ZWP_INPUT_METHOD_V2-PTR :INITFORM NIL :ACCESSOR
            ZWP_INPUT_METHOD_V2-PTR))
          (:DEFAULT-INITARGS :VERSION 1)
          (:DOCUMENTATION "input method

An input method object allows for clients to compose text.

      The objects connects the client to a text input in an application, and
      lets the client to serve as an input method for a seat.

      The zwp_input_method_v2 object can occupy two distinct states: active and
      inactive. In the active state, the object is associated to and
      communicates with a text input. In the inactive state, there is no
      associated text input, and the only communication is with the compositor.
      Initially, the input method is in the inactive state.

      Requests issued in the inactive state must be accepted by the compositor.
      Because of the serial mechanism, and the state reset on activate event,
      they will not have any effect on the state of the next text input.

      There must be no more than one input method object per seat.
"))

(DEFGENERIC COMMIT-STRING
    (RESOURCE TEXT))

(DEFGENERIC SET-PREEDIT-STRING
    (RESOURCE TEXT CURSOR_BEGIN CURSOR_END))

(DEFGENERIC DELETE-SURROUNDING-TEXT
    (RESOURCE BEFORE_LENGTH AFTER_LENGTH))

(DEFGENERIC COMMIT
    (RESOURCE SERIAL))

(DEFGENERIC GET-INPUT-POPUP-SURFACE
    (RESOURCE ID SURFACE))

(DEFGENERIC GRAB-KEYBOARD
    (RESOURCE KEYBOARD))

(DEFGENERIC DESTROY
    (RESOURCE))

(DEFMETHOD CL-WL:DESTROY ((DISPATCH DISPATCH))
  (CL-WL::DEBUG-LOG! "Destroying dispatch object: ~a~%" "zwp_input_method_v2")
  (WHEN (CL-WL::DESTROY-CALLBACK DISPATCH)
    (LOOP FOR CALLBACK IN (CL-WL::DESTROY-CALLBACK DISPATCH)
          DO (FUNCALL CALLBACK DISPATCH)))
  (LET ((RESOURCE-PTR (ZWP_INPUT_METHOD_V2-PTR DISPATCH)))
    (CL-WL::REMOVE-RESOURCE (POINTER-ADDRESS RESOURCE-PTR)))
  (CL-WL::DN-IF DISPATCH))

(DEFMETHOD DESTROY ((DISPATCH DISPATCH)) (CL-WL:DESTROY DISPATCH))

(DEFMETHOD DESTROY (EMPTY) NIL)

(PUSHNEW
 (LIST "zwp_input_method_v2"
       (LIST "zwp_input_popup_surface_v2" "wl_surface"
             "zwp_input_method_keyboard_grab_v2")
       (LAMBDA ()
         (SETF *INTERFACE* (FOREIGN-ALLOC '(:STRUCT INTERFACE)))
         (LET ((REQUESTS-PTR
                (LET ((MESSAGES
                       (CFFI:FOREIGN-ALLOC '(:STRUCT CL-WL.FFI:WL_MESSAGE)
                                           :COUNT 7)))
                  (LET ((INTERFACE-ARRAY
                         (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID))
                                             :COUNT 1))
                        (MSG-PTR
                         (MEM-APTR MESSAGES '(:STRUCT CL-WL.FFI:WL_MESSAGE) 0)))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 0) (NULL-POINTER))
                    (WITH-FOREIGN-SLOTS
                     ((NAME SIGNATURE TYPES) MSG-PTR
                      (:STRUCT CL-WL.FFI:WL_MESSAGE))
                     (SETF NAME (FOREIGN-STRING-ALLOC "commit_string")
                           SIGNATURE "s"
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
                     (SETF NAME (FOREIGN-STRING-ALLOC "set_preedit_string")
                           SIGNATURE "sii"
                           TYPES INTERFACE-ARRAY)))
                  (LET ((INTERFACE-ARRAY
                         (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID))
                                             :COUNT 2))
                        (MSG-PTR
                         (MEM-APTR MESSAGES '(:STRUCT CL-WL.FFI:WL_MESSAGE) 2)))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 0) (NULL-POINTER))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 1) (NULL-POINTER))
                    (WITH-FOREIGN-SLOTS
                     ((NAME SIGNATURE TYPES) MSG-PTR
                      (:STRUCT CL-WL.FFI:WL_MESSAGE))
                     (SETF NAME
                             (FOREIGN-STRING-ALLOC "delete_surrounding_text")
                           SIGNATURE "uu"
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
                     (SETF NAME (FOREIGN-STRING-ALLOC "commit")
                           SIGNATURE "u"
                           TYPES INTERFACE-ARRAY)))
                  (LET ((INTERFACE-ARRAY
                         (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID))
                                             :COUNT 2))
                        (MSG-PTR
                         (MEM-APTR MESSAGES '(:STRUCT CL-WL.FFI:WL_MESSAGE) 4)))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 0)
                            ZWP_INPUT_POPUP_SURFACE_V2::*INTERFACE*)
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 1)
                            WL_SURFACE::*INTERFACE*)
                    (WITH-FOREIGN-SLOTS
                     ((NAME SIGNATURE TYPES) MSG-PTR
                      (:STRUCT CL-WL.FFI:WL_MESSAGE))
                     (SETF NAME
                             (FOREIGN-STRING-ALLOC "get_input_popup_surface")
                           SIGNATURE "no"
                           TYPES INTERFACE-ARRAY)))
                  (LET ((INTERFACE-ARRAY
                         (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID))
                                             :COUNT 1))
                        (MSG-PTR
                         (MEM-APTR MESSAGES '(:STRUCT CL-WL.FFI:WL_MESSAGE) 5)))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 0)
                            ZWP_INPUT_METHOD_KEYBOARD_GRAB_V2::*INTERFACE*)
                    (WITH-FOREIGN-SLOTS
                     ((NAME SIGNATURE TYPES) MSG-PTR
                      (:STRUCT CL-WL.FFI:WL_MESSAGE))
                     (SETF NAME (FOREIGN-STRING-ALLOC "grab_keyboard")
                           SIGNATURE "n"
                           TYPES INTERFACE-ARRAY)))
                  (LET ((INTERFACE-ARRAY
                         (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID))
                                             :COUNT 0))
                        (MSG-PTR
                         (MEM-APTR MESSAGES '(:STRUCT CL-WL.FFI:WL_MESSAGE) 6)))
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
                                           :COUNT 7)))
                  (LET ((INTERFACE-ARRAY
                         (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID))
                                             :COUNT 0))
                        (MSG-PTR
                         (MEM-APTR MESSAGES '(:STRUCT CL-WL.FFI:WL_MESSAGE) 0)))
                    (WITH-FOREIGN-SLOTS
                     ((NAME SIGNATURE TYPES) MSG-PTR
                      (:STRUCT CL-WL.FFI:WL_MESSAGE))
                     (SETF NAME (FOREIGN-STRING-ALLOC "activate")
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
                     (SETF NAME (FOREIGN-STRING-ALLOC "deactivate")
                           SIGNATURE ""
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
                     (SETF NAME (FOREIGN-STRING-ALLOC "surrounding_text")
                           SIGNATURE "suu"
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
                     (SETF NAME (FOREIGN-STRING-ALLOC "text_change_cause")
                           SIGNATURE "u"
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
                     (SETF NAME (FOREIGN-STRING-ALLOC "content_type")
                           SIGNATURE "uu"
                           TYPES INTERFACE-ARRAY)))
                  (LET ((INTERFACE-ARRAY
                         (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID))
                                             :COUNT 0))
                        (MSG-PTR
                         (MEM-APTR MESSAGES '(:STRUCT CL-WL.FFI:WL_MESSAGE) 5)))
                    (WITH-FOREIGN-SLOTS
                     ((NAME SIGNATURE TYPES) MSG-PTR
                      (:STRUCT CL-WL.FFI:WL_MESSAGE))
                     (SETF NAME (FOREIGN-STRING-ALLOC "done")
                           SIGNATURE ""
                           TYPES INTERFACE-ARRAY)))
                  (LET ((INTERFACE-ARRAY
                         (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID))
                                             :COUNT 0))
                        (MSG-PTR
                         (MEM-APTR MESSAGES '(:STRUCT CL-WL.FFI:WL_MESSAGE) 6)))
                    (WITH-FOREIGN-SLOTS
                     ((NAME SIGNATURE TYPES) MSG-PTR
                      (:STRUCT CL-WL.FFI:WL_MESSAGE))
                     (SETF NAME (FOREIGN-STRING-ALLOC "unavailable")
                           SIGNATURE ""
                           TYPES INTERFACE-ARRAY)))
                  MESSAGES)))
           (WITH-FOREIGN-SLOTS
            ((NAME VERSION METHOD_COUNT METHODS EVENT_COUNT EVENTS) *INTERFACE*
             (:STRUCT INTERFACE))
            (SETF NAME (FOREIGN-STRING-ALLOC "zwp_input_method_v2")
                  VERSION 1
                  METHOD_COUNT 7
                  METHODS REQUESTS-PTR
                  EVENT_COUNT 7
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
                                       "zwp_input_method_v2" "commit-string")
                    (FUNCALL 'COMMIT-STRING RESOURCE
                             (VALUES
                              (FOREIGN-SLOT-VALUE
                               (MEM-APTR ARGS '(:UNION CL-WL.FFI:WL_ARGUMENT)
                                0)
                               '(:UNION CL-WL.FFI:WL_ARGUMENT)
                               'WL-FFI::S))))
                   (1
                    (CL-WL::DEBUG-LOG! "Dispatching ~a:~a~%"
                                       "zwp_input_method_v2"
                                       "set-preedit-string")
                    (FUNCALL 'SET-PREEDIT-STRING RESOURCE
                             (VALUES
                              (FOREIGN-SLOT-VALUE
                               (MEM-APTR ARGS '(:UNION CL-WL.FFI:WL_ARGUMENT)
                                0)
                               '(:UNION CL-WL.FFI:WL_ARGUMENT)
                               'WL-FFI::S))
                             (VALUES
                              (FOREIGN-SLOT-VALUE
                               (MEM-APTR ARGS '(:UNION CL-WL.FFI:WL_ARGUMENT)
                                1)
                               '(:UNION CL-WL.FFI:WL_ARGUMENT)
                               'WL-FFI::I))
                             (VALUES
                              (FOREIGN-SLOT-VALUE
                               (MEM-APTR ARGS '(:UNION CL-WL.FFI:WL_ARGUMENT)
                                2)
                               '(:UNION CL-WL.FFI:WL_ARGUMENT)
                               'WL-FFI::I))))
                   (2
                    (CL-WL::DEBUG-LOG! "Dispatching ~a:~a~%"
                                       "zwp_input_method_v2"
                                       "delete-surrounding-text")
                    (FUNCALL 'DELETE-SURROUNDING-TEXT RESOURCE
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
                   (3
                    (CL-WL::DEBUG-LOG! "Dispatching ~a:~a~%"
                                       "zwp_input_method_v2" "commit")
                    (FUNCALL 'COMMIT RESOURCE
                             (VALUES
                              (FOREIGN-SLOT-VALUE
                               (MEM-APTR ARGS '(:UNION CL-WL.FFI:WL_ARGUMENT)
                                0)
                               '(:UNION CL-WL.FFI:WL_ARGUMENT)
                               'WL-FFI::U))))
                   (4
                    (CL-WL::DEBUG-LOG! "Dispatching ~a:~a~%"
                                       "zwp_input_method_v2"
                                       "get-input-popup-surface")
                    (FUNCALL 'GET-INPUT-POPUP-SURFACE RESOURCE
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
                              CL-WL::*RESOURCE-TRACKER*)))
                   (5
                    (CL-WL::DEBUG-LOG! "Dispatching ~a:~a~%"
                                       "zwp_input_method_v2" "grab-keyboard")
                    (FUNCALL 'GRAB-KEYBOARD RESOURCE
                             (VALUES
                              (FOREIGN-SLOT-VALUE
                               (MEM-APTR ARGS '(:UNION CL-WL.FFI:WL_ARGUMENT)
                                0)
                               '(:UNION CL-WL.FFI:WL_ARGUMENT)
                               'WL-FFI::N))))
                   (6
                    (CL-WL::DEBUG-LOG! "Dispatching ~a:~a~%"
                                       "zwp_input_method_v2" "destroy")
                    (FUNCALL 'DESTROY RESOURCE)))
     (KILL-CLIENT NIL :REPORT "Kill the client causing errors"
      (CL-WL:DESTROY-CLIENT (CL-WL:CLIENT RESOURCE)) 0)))
 0)

(DEFVAR *DISPATCHER* (CALLBACK DISPATCHER-FFI))

(DEFMETHOD SHARED-INITIALIZE :AFTER ((INSTANCE DISPATCH) SLOT-NAMES &KEY ID)
  (DECLARE (IGNORE SLOT-NAMES))
  (UNLESS (ZWP_INPUT_METHOD_V2-ID INSTANCE)
    (LET* ((RESOURCE
            (CL-WL::CREATE-RESOURCE (CL-WL:PTR (CL-WL:CLIENT INSTANCE))
                                    *INTERFACE* (VERSION INSTANCE)
                                    (OR ID 0))))
      (SETF (CL-WL::TRANSIENT-ID INSTANCE)
              (SETF (ZWP_INPUT_METHOD_V2-ID INSTANCE)
                      (OR ID (CL-WL.FFI:RESOURCE-GET-ID RESOURCE))))
      (SETF (ZWP_INPUT_METHOD_V2-PTR INSTANCE) RESOURCE)
      (SETF (GETHASH (POINTER-ADDRESS RESOURCE) CL-WL::*RESOURCE-TRACKER*)
              INSTANCE)
      (RESOURCE-SET-DISPATCHER RESOURCE *DISPATCHER* (NULL-POINTER)
       (NULL-POINTER) (NULL-POINTER)))))

(DEFMETHOD SEND-ACTIVATE ((DISPATCH DISPATCH))
  (CL-WL::DEBUG-LOG! "Event: ~a~%" "activate")
  (LET ((ARG-LIST (FOREIGN-ALLOC '(:UNION WL_ARGUMENT) :COUNT 0)))
    (RESOURCE-POST-EVENT-ARRAY (ZWP_INPUT_METHOD_V2-PTR DISPATCH) 0
     ARG-LIST)))

(DEFMETHOD SEND-DEACTIVATE ((DISPATCH DISPATCH))
  (CL-WL::DEBUG-LOG! "Event: ~a~%" "deactivate")
  (LET ((ARG-LIST (FOREIGN-ALLOC '(:UNION WL_ARGUMENT) :COUNT 0)))
    (RESOURCE-POST-EVENT-ARRAY (ZWP_INPUT_METHOD_V2-PTR DISPATCH) 1
     ARG-LIST)))

(DEFMETHOD SEND-SURROUNDING-TEXT
           ((DISPATCH DISPATCH) TEXT CURSOR
            ANCHOR)
  (CL-WL::DEBUG-LOG! "Event: ~a~%" "surrounding_text")
  (LET ((ARG-LIST (FOREIGN-ALLOC '(:UNION WL_ARGUMENT) :COUNT 3)))
    (SETF (FOREIGN-SLOT-VALUE
           (MEM-AREF ARG-LIST '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 0)
           '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 'WL-FFI::S)
            TEXT)
    (SETF (FOREIGN-SLOT-VALUE
           (MEM-AREF ARG-LIST '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 1)
           '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 'WL-FFI::U)
            CURSOR)
    (SETF (FOREIGN-SLOT-VALUE
           (MEM-AREF ARG-LIST '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 2)
           '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 'WL-FFI::U)
            ANCHOR)
    (RESOURCE-POST-EVENT-ARRAY (ZWP_INPUT_METHOD_V2-PTR DISPATCH) 2
     ARG-LIST)))

(DEFMETHOD SEND-TEXT-CHANGE-CAUSE
           ((DISPATCH DISPATCH) CAUSE)
  (CL-WL::DEBUG-LOG! "Event: ~a~%" "text_change_cause")
  (LET ((ARG-LIST (FOREIGN-ALLOC '(:UNION WL_ARGUMENT) :COUNT 1)))
    (SETF (FOREIGN-SLOT-VALUE
           (MEM-AREF ARG-LIST '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 0)
           '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 'WL-FFI::U)
            (ZWP-TEXT-INPUT-V3::CHANGE-CAUSE-TO-VALUE
             CAUSE))
    (RESOURCE-POST-EVENT-ARRAY (ZWP_INPUT_METHOD_V2-PTR DISPATCH) 3
     ARG-LIST)))

(DEFMETHOD SEND-CONTENT-TYPE
           ((DISPATCH DISPATCH) HINT PURPOSE)
  (CL-WL::DEBUG-LOG! "Event: ~a~%" "content_type")
  (LET ((ARG-LIST (FOREIGN-ALLOC '(:UNION WL_ARGUMENT) :COUNT 2)))
    (SETF (FOREIGN-SLOT-VALUE
           (MEM-AREF ARG-LIST '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 0)
           '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 'WL-FFI::U)
            (ZWP-TEXT-INPUT-V3::CONTENT-HINT-TO-VALUE
             HINT))
    (SETF (FOREIGN-SLOT-VALUE
           (MEM-AREF ARG-LIST '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 1)
           '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 'WL-FFI::U)
            (ZWP-TEXT-INPUT-V3::CONTENT-PURPOSE-TO-VALUE
             PURPOSE))
    (RESOURCE-POST-EVENT-ARRAY (ZWP_INPUT_METHOD_V2-PTR DISPATCH) 4
     ARG-LIST)))

(DEFMETHOD SEND-DONE ((DISPATCH DISPATCH))
  (CL-WL::DEBUG-LOG! "Event: ~a~%" "done")
  (LET ((ARG-LIST (FOREIGN-ALLOC '(:UNION WL_ARGUMENT) :COUNT 0)))
    (RESOURCE-POST-EVENT-ARRAY (ZWP_INPUT_METHOD_V2-PTR DISPATCH) 5
     ARG-LIST)))

(DEFMETHOD SEND-UNAVAILABLE ((DISPATCH DISPATCH))
  (CL-WL::DEBUG-LOG! "Event: ~a~%" "unavailable")
  (LET ((ARG-LIST (FOREIGN-ALLOC '(:UNION WL_ARGUMENT) :COUNT 0)))
    (RESOURCE-POST-EVENT-ARRAY (ZWP_INPUT_METHOD_V2-PTR DISPATCH) 6
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
          (:DEFAULT-INITARGS :VERSION 1 :DISPATCH-IMPL 'DISPATCH)
          (:DOCUMENTATION "input method

An input method object allows for clients to compose text.

      The objects connects the client to a text input in an application, and
      lets the client to serve as an input method for a seat.

      The zwp_input_method_v2 object can occupy two distinct states: active and
      inactive. In the active state, the object is associated to and
      communicates with a text input. In the inactive state, there is no
      associated text input, and the only communication is with the compositor.
      Initially, the input method is in the inactive state.

      Requests issued in the inactive state must be accepted by the compositor.
      Because of the serial mechanism, and the state reset on activate event,
      they will not have any effect on the state of the next text input.

      There must be no more than one input method object per seat.
"))

(DEFMETHOD DISPATCH-BIND ((GLOBAL GLOBAL) CLIENT DATA VERSION ID)
  "Default bind implementation for the zwp_input_method_v2 global object.
This can be overriden by inheritance in case if custom behaviour is required."
  (CL-WL::DEBUG-LOG! "Binding ~a~%" "zwp_input_method_v2")
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
  (CL-WL::DEBUG-LOG! "Initializing global object: ~a~%" "zwp_input_method_v2")
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

(IN-PACKAGE :ZWP_INPUT_POPUP_SURFACE_V2)

(DEFCLASS DISPATCH (CL-WL:OBJECT)
          ((ZWP_INPUT_POPUP_SURFACE_V2-ID :INITFORM NIL :ACCESSOR
            ZWP_INPUT_POPUP_SURFACE_V2-ID)
           (ZWP_INPUT_POPUP_SURFACE_V2-PTR :INITFORM NIL :ACCESSOR
            ZWP_INPUT_POPUP_SURFACE_V2-PTR))
          (:DEFAULT-INITARGS :VERSION 1)
          (:DOCUMENTATION "popup surface

This interface marks a surface as a popup for interacting with an input
      method.

      The compositor should place it near the active text input area. It must
      be visible if and only if the input method is in the active state.

      The client must not destroy the underlying wl_surface while the
      zwp_input_popup_surface_v2 object exists.
"))

(DEFGENERIC DESTROY
    (RESOURCE))

(DEFMETHOD CL-WL:DESTROY ((DISPATCH DISPATCH))
  (CL-WL::DEBUG-LOG! "Destroying dispatch object: ~a~%"
                     "zwp_input_popup_surface_v2")
  (WHEN (CL-WL::DESTROY-CALLBACK DISPATCH)
    (LOOP FOR CALLBACK IN (CL-WL::DESTROY-CALLBACK DISPATCH)
          DO (FUNCALL CALLBACK DISPATCH)))
  (LET ((RESOURCE-PTR (ZWP_INPUT_POPUP_SURFACE_V2-PTR DISPATCH)))
    (CL-WL::REMOVE-RESOURCE (POINTER-ADDRESS RESOURCE-PTR)))
  (CL-WL::DN-IF DISPATCH))

(DEFMETHOD DESTROY ((DISPATCH DISPATCH)) (CL-WL:DESTROY DISPATCH))

(DEFMETHOD DESTROY (EMPTY) NIL)

(PUSHNEW
 (LIST "zwp_input_popup_surface_v2" (LIST)
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
                                             :COUNT 4))
                        (MSG-PTR
                         (MEM-APTR MESSAGES '(:STRUCT CL-WL.FFI:WL_MESSAGE) 0)))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 0) (NULL-POINTER))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 1) (NULL-POINTER))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 2) (NULL-POINTER))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 3) (NULL-POINTER))
                    (WITH-FOREIGN-SLOTS
                     ((NAME SIGNATURE TYPES) MSG-PTR
                      (:STRUCT CL-WL.FFI:WL_MESSAGE))
                     (SETF NAME (FOREIGN-STRING-ALLOC "text_input_rectangle")
                           SIGNATURE "iiii"
                           TYPES INTERFACE-ARRAY)))
                  MESSAGES)))
           (WITH-FOREIGN-SLOTS
            ((NAME VERSION METHOD_COUNT METHODS EVENT_COUNT EVENTS) *INTERFACE*
             (:STRUCT INTERFACE))
            (SETF NAME (FOREIGN-STRING-ALLOC "zwp_input_popup_surface_v2")
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
   (RESTART-CASE (ECASE OPCODE
                   (0
                    (CL-WL::DEBUG-LOG! "Dispatching ~a:~a~%"
                                       "zwp_input_popup_surface_v2" "destroy")
                    (FUNCALL 'DESTROY RESOURCE)))
     (KILL-CLIENT NIL :REPORT "Kill the client causing errors"
      (CL-WL:DESTROY-CLIENT (CL-WL:CLIENT RESOURCE)) 0)))
 0)

(DEFVAR *DISPATCHER* (CALLBACK DISPATCHER-FFI))

(DEFMETHOD SHARED-INITIALIZE :AFTER ((INSTANCE DISPATCH) SLOT-NAMES &KEY ID)
  (DECLARE (IGNORE SLOT-NAMES))
  (UNLESS (ZWP_INPUT_POPUP_SURFACE_V2-ID INSTANCE)
    (LET* ((RESOURCE
            (CL-WL::CREATE-RESOURCE (CL-WL:PTR (CL-WL:CLIENT INSTANCE))
                                    *INTERFACE* (VERSION INSTANCE)
                                    (OR ID 0))))
      (SETF (CL-WL::TRANSIENT-ID INSTANCE)
              (SETF (ZWP_INPUT_POPUP_SURFACE_V2-ID INSTANCE)
                      (OR ID (CL-WL.FFI:RESOURCE-GET-ID RESOURCE))))
      (SETF (ZWP_INPUT_POPUP_SURFACE_V2-PTR INSTANCE) RESOURCE)
      (SETF (GETHASH (POINTER-ADDRESS RESOURCE) CL-WL::*RESOURCE-TRACKER*)
              INSTANCE)
      (RESOURCE-SET-DISPATCHER RESOURCE *DISPATCHER* (NULL-POINTER)
       (NULL-POINTER) (NULL-POINTER)))))

(DEFMETHOD SEND-TEXT-INPUT-RECTANGLE
           ((DISPATCH DISPATCH) X Y WIDTH
            HEIGHT)
  (CL-WL::DEBUG-LOG! "Event: ~a~%" "text_input_rectangle")
  (LET ((ARG-LIST (FOREIGN-ALLOC '(:UNION WL_ARGUMENT) :COUNT 4)))
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
            WIDTH)
    (SETF (FOREIGN-SLOT-VALUE
           (MEM-AREF ARG-LIST '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 3)
           '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 'WL-FFI::I)
            HEIGHT)
    (RESOURCE-POST-EVENT-ARRAY
     (ZWP_INPUT_POPUP_SURFACE_V2-PTR DISPATCH) 0 ARG-LIST)))

(DEFCLASS GLOBAL (CL-WL:GLOBAL) NIL
          (:DEFAULT-INITARGS :VERSION 1 :DISPATCH-IMPL 'DISPATCH)
          (:DOCUMENTATION "popup surface

This interface marks a surface as a popup for interacting with an input
      method.

      The compositor should place it near the active text input area. It must
      be visible if and only if the input method is in the active state.

      The client must not destroy the underlying wl_surface while the
      zwp_input_popup_surface_v2 object exists.
"))

(DEFMETHOD DISPATCH-BIND ((GLOBAL GLOBAL) CLIENT DATA VERSION ID)
  "Default bind implementation for the zwp_input_popup_surface_v2 global object.
This can be overriden by inheritance in case if custom behaviour is required."
  (CL-WL::DEBUG-LOG! "Binding ~a~%" "zwp_input_popup_surface_v2")
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
                     "zwp_input_popup_surface_v2")
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

(IN-PACKAGE :ZWP_INPUT_METHOD_KEYBOARD_GRAB_V2)

(DEFCLASS DISPATCH (CL-WL:OBJECT)
          ((ZWP_INPUT_METHOD_KEYBOARD_GRAB_V2-ID :INITFORM NIL
            :ACCESSOR ZWP_INPUT_METHOD_KEYBOARD_GRAB_V2-ID)
           (ZWP_INPUT_METHOD_KEYBOARD_GRAB_V2-PTR :INITFORM NIL
            :ACCESSOR ZWP_INPUT_METHOD_KEYBOARD_GRAB_V2-PTR))
          (:DEFAULT-INITARGS :VERSION 1)
          (:DOCUMENTATION "keyboard grab

The zwp_input_method_keyboard_grab_v2 interface represents an exclusive
      grab of the wl_keyboard interface associated with the seat.
"))

(DEFGENERIC RELEASE
    (RESOURCE))

(DEFMETHOD CL-WL:DESTROY ((DISPATCH DISPATCH))
  (CL-WL::DEBUG-LOG! "Destroying dispatch object: ~a~%"
                     "zwp_input_method_keyboard_grab_v2")
  (WHEN (CL-WL::DESTROY-CALLBACK DISPATCH)
    (LOOP FOR CALLBACK IN (CL-WL::DESTROY-CALLBACK DISPATCH)
          DO (FUNCALL CALLBACK DISPATCH)))
  (LET ((RESOURCE-PTR
         (ZWP_INPUT_METHOD_KEYBOARD_GRAB_V2-PTR DISPATCH)))
    (CL-WL::REMOVE-RESOURCE (POINTER-ADDRESS RESOURCE-PTR)))
  (CL-WL::DN-IF DISPATCH))

(PUSHNEW
 (LIST "zwp_input_method_keyboard_grab_v2" (LIST)
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
                           SIGNATURE ""
                           TYPES INTERFACE-ARRAY)))
                  MESSAGES))
               (EVENTS-PTR
                (LET ((MESSAGES
                       (CFFI:FOREIGN-ALLOC '(:STRUCT CL-WL.FFI:WL_MESSAGE)
                                           :COUNT 4)))
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
                     (SETF NAME (FOREIGN-STRING-ALLOC "key")
                           SIGNATURE "uuuu"
                           TYPES INTERFACE-ARRAY)))
                  (LET ((INTERFACE-ARRAY
                         (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID))
                                             :COUNT 5))
                        (MSG-PTR
                         (MEM-APTR MESSAGES '(:STRUCT CL-WL.FFI:WL_MESSAGE) 2)))
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
                         (MEM-APTR MESSAGES '(:STRUCT CL-WL.FFI:WL_MESSAGE) 3)))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 0) (NULL-POINTER))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 1) (NULL-POINTER))
                    (WITH-FOREIGN-SLOTS
                     ((NAME SIGNATURE TYPES) MSG-PTR
                      (:STRUCT CL-WL.FFI:WL_MESSAGE))
                     (SETF NAME (FOREIGN-STRING-ALLOC "repeat_info")
                           SIGNATURE "ii"
                           TYPES INTERFACE-ARRAY)))
                  MESSAGES)))
           (WITH-FOREIGN-SLOTS
            ((NAME VERSION METHOD_COUNT METHODS EVENT_COUNT EVENTS) *INTERFACE*
             (:STRUCT INTERFACE))
            (SETF NAME
                    (FOREIGN-STRING-ALLOC "zwp_input_method_keyboard_grab_v2")
                  VERSION 1
                  METHOD_COUNT 1
                  METHODS REQUESTS-PTR
                  EVENT_COUNT 4
                  EVENTS EVENTS-PTR)))))
 CL-WL::*INTERFACE-INIT-LIST* :TEST #'CL-WL::INTERFACE-EXISTS-TEST)

(DEFCALLBACK DISPATCHER-FFI :INT
 ((DATA :POINTER) (TARGET :POINTER) (OPCODE :UINT) (MESSAGE :POINTER)
  (ARGS :POINTER))
 (DECLARE (IGNORE DATA MESSAGE ARGS))
 (LET ((RESOURCE (GETHASH (POINTER-ADDRESS TARGET) CL-WL::*RESOURCE-TRACKER*)))
   (RESTART-CASE (ECASE OPCODE
                   (0
                    (CL-WL::DEBUG-LOG! "Dispatching ~a:~a~%"
                                       "zwp_input_method_keyboard_grab_v2"
                                       "release")
                    (FUNCALL 'RELEASE RESOURCE)))
     (KILL-CLIENT NIL :REPORT "Kill the client causing errors"
      (CL-WL:DESTROY-CLIENT (CL-WL:CLIENT RESOURCE)) 0)))
 0)

(DEFVAR *DISPATCHER* (CALLBACK DISPATCHER-FFI))

(DEFMETHOD SHARED-INITIALIZE :AFTER ((INSTANCE DISPATCH) SLOT-NAMES &KEY ID)
  (DECLARE (IGNORE SLOT-NAMES))
  (UNLESS (ZWP_INPUT_METHOD_KEYBOARD_GRAB_V2-ID INSTANCE)
    (LET* ((RESOURCE
            (CL-WL::CREATE-RESOURCE (CL-WL:PTR (CL-WL:CLIENT INSTANCE))
                                    *INTERFACE* (VERSION INSTANCE)
                                    (OR ID 0))))
      (SETF (CL-WL::TRANSIENT-ID INSTANCE)
              (SETF (ZWP_INPUT_METHOD_KEYBOARD_GRAB_V2-ID INSTANCE)
                      (OR ID (CL-WL.FFI:RESOURCE-GET-ID RESOURCE))))
      (SETF (ZWP_INPUT_METHOD_KEYBOARD_GRAB_V2-PTR INSTANCE)
              RESOURCE)
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
            (WL-KEYBOARD::KEYMAP-FORMAT-TO-VALUE
             FORMAT))
    (SETF (FOREIGN-SLOT-VALUE
           (MEM-AREF ARG-LIST '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 1)
           '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 'WL-FFI::H)
            FD)
    (SETF (FOREIGN-SLOT-VALUE
           (MEM-AREF ARG-LIST '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 2)
           '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 'WL-FFI::U)
            SIZE)
    (RESOURCE-POST-EVENT-ARRAY
     (ZWP_INPUT_METHOD_KEYBOARD_GRAB_V2-PTR DISPATCH) 0 ARG-LIST)))

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
            (WL-KEYBOARD::KEY-STATE-TO-VALUE STATE))
    (RESOURCE-POST-EVENT-ARRAY
     (ZWP_INPUT_METHOD_KEYBOARD_GRAB_V2-PTR DISPATCH) 1 ARG-LIST)))

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
    (RESOURCE-POST-EVENT-ARRAY
     (ZWP_INPUT_METHOD_KEYBOARD_GRAB_V2-PTR DISPATCH) 2 ARG-LIST)))

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
    (RESOURCE-POST-EVENT-ARRAY
     (ZWP_INPUT_METHOD_KEYBOARD_GRAB_V2-PTR DISPATCH) 3 ARG-LIST)))

(DEFCLASS GLOBAL (CL-WL:GLOBAL) NIL
          (:DEFAULT-INITARGS :VERSION 1 :DISPATCH-IMPL 'DISPATCH)
          (:DOCUMENTATION "keyboard grab

The zwp_input_method_keyboard_grab_v2 interface represents an exclusive
      grab of the wl_keyboard interface associated with the seat.
"))

(DEFMETHOD DISPATCH-BIND ((GLOBAL GLOBAL) CLIENT DATA VERSION ID)
  "Default bind implementation for the zwp_input_method_keyboard_grab_v2 global object.
This can be overriden by inheritance in case if custom behaviour is required."
  (CL-WL::DEBUG-LOG! "Binding ~a~%" "zwp_input_method_keyboard_grab_v2")
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
                     "zwp_input_method_keyboard_grab_v2")
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

(IN-PACKAGE :ZWP_INPUT_METHOD_MANAGER_V2)

(DEFCLASS DISPATCH (CL-WL:OBJECT)
          ((ZWP_INPUT_METHOD_MANAGER_V2-ID :INITFORM NIL :ACCESSOR
            ZWP_INPUT_METHOD_MANAGER_V2-ID)
           (ZWP_INPUT_METHOD_MANAGER_V2-PTR :INITFORM NIL :ACCESSOR
            ZWP_INPUT_METHOD_MANAGER_V2-PTR))
          (:DEFAULT-INITARGS :VERSION 1)
          (:DOCUMENTATION "input method manager

The input method manager allows the client to become the input method on
      a chosen seat.

      No more than one input method must be associated with any seat at any
      given time.
"))

(DEFGENERIC GET-INPUT-METHOD
    (RESOURCE SEAT INPUT_METHOD))

(DEFGENERIC DESTROY
    (RESOURCE))

(DEFMETHOD CL-WL:DESTROY ((DISPATCH DISPATCH))
  (CL-WL::DEBUG-LOG! "Destroying dispatch object: ~a~%"
                     "zwp_input_method_manager_v2")
  (WHEN (CL-WL::DESTROY-CALLBACK DISPATCH)
    (LOOP FOR CALLBACK IN (CL-WL::DESTROY-CALLBACK DISPATCH)
          DO (FUNCALL CALLBACK DISPATCH)))
  (LET ((RESOURCE-PTR (ZWP_INPUT_METHOD_MANAGER_V2-PTR DISPATCH)))
    (CL-WL::REMOVE-RESOURCE (POINTER-ADDRESS RESOURCE-PTR)))
  (CL-WL::DN-IF DISPATCH))

(DEFMETHOD DESTROY ((DISPATCH DISPATCH)) (CL-WL:DESTROY DISPATCH))

(DEFMETHOD DESTROY (EMPTY) NIL)

(PUSHNEW
 (LIST "zwp_input_method_manager_v2" (LIST "wl_seat" "zwp_input_method_v2")
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
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 0)
                            WL_SEAT::*INTERFACE*)
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 1)
                            ZWP_INPUT_METHOD_V2::*INTERFACE*)
                    (WITH-FOREIGN-SLOTS
                     ((NAME SIGNATURE TYPES) MSG-PTR
                      (:STRUCT CL-WL.FFI:WL_MESSAGE))
                     (SETF NAME (FOREIGN-STRING-ALLOC "get_input_method")
                           SIGNATURE "on"
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
            (SETF NAME (FOREIGN-STRING-ALLOC "zwp_input_method_manager_v2")
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
                                       "zwp_input_method_manager_v2"
                                       "get-input-method")
                    (FUNCALL 'GET-INPUT-METHOD RESOURCE
                             (GETHASH
                              (POINTER-ADDRESS
                               (FOREIGN-SLOT-VALUE
                                (MEM-APTR ARGS '(:UNION CL-WL.FFI:WL_ARGUMENT)
                                 0)
                                '(:UNION CL-WL.FFI:WL_ARGUMENT)
                                'WL-FFI::O))
                              CL-WL::*RESOURCE-TRACKER*)
                             (VALUES
                              (FOREIGN-SLOT-VALUE
                               (MEM-APTR ARGS '(:UNION CL-WL.FFI:WL_ARGUMENT)
                                1)
                               '(:UNION CL-WL.FFI:WL_ARGUMENT)
                               'WL-FFI::N))))
                   (1
                    (CL-WL::DEBUG-LOG! "Dispatching ~a:~a~%"
                                       "zwp_input_method_manager_v2" "destroy")
                    (FUNCALL 'DESTROY RESOURCE)))
     (KILL-CLIENT NIL :REPORT "Kill the client causing errors"
      (CL-WL:DESTROY-CLIENT (CL-WL:CLIENT RESOURCE)) 0)))
 0)

(DEFVAR *DISPATCHER* (CALLBACK DISPATCHER-FFI))

(DEFMETHOD SHARED-INITIALIZE :AFTER ((INSTANCE DISPATCH) SLOT-NAMES &KEY ID)
  (DECLARE (IGNORE SLOT-NAMES))
  (UNLESS (ZWP_INPUT_METHOD_MANAGER_V2-ID INSTANCE)
    (LET* ((RESOURCE
            (CL-WL::CREATE-RESOURCE (CL-WL:PTR (CL-WL:CLIENT INSTANCE))
                                    *INTERFACE* (VERSION INSTANCE)
                                    (OR ID 0))))
      (SETF (CL-WL::TRANSIENT-ID INSTANCE)
              (SETF (ZWP_INPUT_METHOD_MANAGER_V2-ID INSTANCE)
                      (OR ID (CL-WL.FFI:RESOURCE-GET-ID RESOURCE))))
      (SETF (ZWP_INPUT_METHOD_MANAGER_V2-PTR INSTANCE) RESOURCE)
      (SETF (GETHASH (POINTER-ADDRESS RESOURCE) CL-WL::*RESOURCE-TRACKER*)
              INSTANCE)
      (RESOURCE-SET-DISPATCHER RESOURCE *DISPATCHER* (NULL-POINTER)
       (NULL-POINTER) (NULL-POINTER)))))

(DEFCLASS GLOBAL (CL-WL:GLOBAL) NIL
          (:DEFAULT-INITARGS :VERSION 1 :DISPATCH-IMPL 'DISPATCH)
          (:DOCUMENTATION "input method manager

The input method manager allows the client to become the input method on
      a chosen seat.

      No more than one input method must be associated with any seat at any
      given time.
"))

(DEFMETHOD DISPATCH-BIND ((GLOBAL GLOBAL) CLIENT DATA VERSION ID)
  "Default bind implementation for the zwp_input_method_manager_v2 global object.
This can be overriden by inheritance in case if custom behaviour is required."
  (CL-WL::DEBUG-LOG! "Binding ~a~%" "zwp_input_method_manager_v2")
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
                     "zwp_input_method_manager_v2")
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

