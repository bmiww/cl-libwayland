(DEFPACKAGE :ZWP_TEXT_INPUT_V3
  (:USE :CL :CFFI :CL-WL.FFI)
  (:NICKNAMES :ZWP-TEXT-INPUT-V3)
  (:EXPORT DISPATCH
           GLOBAL
           DISPATCH-BIND
           DESTROY
           ENABLE
           DISABLE
           SET-SURROUNDING-TEXT
           SET-TEXT-CHANGE-CAUSE
           SET-CONTENT-TYPE
           SET-CURSOR-RECTANGLE
           COMMIT
           SEND-ENTER
           SEND-LEAVE
           SEND-PREEDIT-STRING
           SEND-COMMIT-STRING
           SEND-DELETE-SURROUNDING-TEXT
           SEND-DONE))

(IN-PACKAGE :ZWP_TEXT_INPUT_V3)

(DEFCSTRUCT INTERFACE (NAME :STRING) (VERSION :INT) (METHOD_COUNT :INT)
 (METHODS (:POINTER (:STRUCT CL-WL.FFI:WL_MESSAGE))) (EVENT_COUNT :INT)
 (EVENTS (:POINTER (:STRUCT CL-WL.FFI:WL_MESSAGE))))

(DEFVAR *INTERFACE* NIL)

(DEFPACKAGE :ZWP_TEXT_INPUT_MANAGER_V3
  (:USE :CL :CFFI :CL-WL.FFI)
  (:NICKNAMES :ZWP-TEXT-INPUT-MANAGER-V3)
  (:EXPORT DISPATCH
           GLOBAL
           DISPATCH-BIND
           DESTROY
           GET-TEXT-INPUT))

(IN-PACKAGE :ZWP_TEXT_INPUT_MANAGER_V3)

(DEFCSTRUCT INTERFACE (NAME :STRING) (VERSION :INT) (METHOD_COUNT :INT)
 (METHODS (:POINTER (:STRUCT CL-WL.FFI:WL_MESSAGE))) (EVENT_COUNT :INT)
 (EVENTS (:POINTER (:STRUCT CL-WL.FFI:WL_MESSAGE))))

(DEFVAR *INTERFACE* NIL)

(IN-PACKAGE :ZWP_TEXT_INPUT_V3)

(DEFCLASS DISPATCH (CL-WL:OBJECT)
          ((ZWP_TEXT_INPUT_V3-ID :INITFORM NIL :ACCESSOR
            ZWP_TEXT_INPUT_V3-ID)
           (ZWP_TEXT_INPUT_V3-PTR :INITFORM NIL :ACCESSOR
            ZWP_TEXT_INPUT_V3-PTR))
          (:DEFAULT-INITARGS :VERSION 1)
          (:DOCUMENTATION "text input

The zwp_text_input_v3 interface represents text input and input methods
      associated with a seat. It provides enter/leave events to follow the
      text input focus for a seat.

      Requests are used to enable/disable the text-input object and set
      state information like surrounding and selected text or the content type.
      The information about the entered text is sent to the text-input object
      via the preedit_string and commit_string events.

      Text is valid UTF-8 encoded, indices and lengths are in bytes. Indices
      must not point to middle bytes inside a code point: they must either
      point to the first byte of a code point or to the end of the buffer.
      Lengths must be measured between two valid indices.

      Focus moving throughout surfaces will result in the emission of
      zwp_text_input_v3.enter and zwp_text_input_v3.leave events. The focused
      surface must commit zwp_text_input_v3.enable and
      zwp_text_input_v3.disable requests as the keyboard focus moves across
      editable and non-editable elements of the UI. Those two requests are not
      expected to be paired with each other, the compositor must be able to
      handle consecutive series of the same request.

      State is sent by the state requests (set_surrounding_text,
      set_content_type and set_cursor_rectangle) and a commit request. After an
      enter event or disable request all state information is invalidated and
      needs to be resent by the client.
"))

(DEFGENERIC DESTROY
    (RESOURCE))

(DEFGENERIC ENABLE
    (RESOURCE))

(DEFGENERIC DISABLE
    (RESOURCE))

(DEFGENERIC SET-SURROUNDING-TEXT
    (RESOURCE TEXT CURSOR ANCHOR))

(DEFGENERIC SET-TEXT-CHANGE-CAUSE
    (RESOURCE CAUSE))

(DEFGENERIC SET-CONTENT-TYPE
    (RESOURCE HINT PURPOSE))

(DEFGENERIC SET-CURSOR-RECTANGLE
    (RESOURCE X Y WIDTH HEIGHT))

(DEFGENERIC COMMIT
    (RESOURCE))

(DEFMETHOD CL-WL:DESTROY ((DISPATCH DISPATCH))
  (CL-WL::DEBUG-LOG! "Destroying dispatch object: ~a~%" "zwp_text_input_v3")
  (WHEN (CL-WL::DESTROY-CALLBACK DISPATCH)
    (LOOP FOR CALLBACK IN (CL-WL::DESTROY-CALLBACK DISPATCH)
          DO (FUNCALL CALLBACK DISPATCH)))
  (LET ((RESOURCE-PTR (ZWP_TEXT_INPUT_V3-PTR DISPATCH)))
    (CL-WL::REMOVE-RESOURCE (POINTER-ADDRESS RESOURCE-PTR)))
  (CL-WL::DN-IF DISPATCH))

(DEFMETHOD DESTROY ((DISPATCH DISPATCH)) (CL-WL:DESTROY DISPATCH))

(DEFMETHOD DESTROY (EMPTY) NIL)

(PUSHNEW
 (LIST "zwp_text_input_v3" (LIST "wl_surface")
       (LAMBDA ()
         (SETF *INTERFACE* (FOREIGN-ALLOC '(:STRUCT INTERFACE)))
         (LET ((REQUESTS-PTR
                (LET ((MESSAGES
                       (CFFI:FOREIGN-ALLOC '(:STRUCT CL-WL.FFI:WL_MESSAGE)
                                           :COUNT 8)))
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
                                             :COUNT 0))
                        (MSG-PTR
                         (MEM-APTR MESSAGES '(:STRUCT CL-WL.FFI:WL_MESSAGE) 1)))
                    (WITH-FOREIGN-SLOTS
                     ((NAME SIGNATURE TYPES) MSG-PTR
                      (:STRUCT CL-WL.FFI:WL_MESSAGE))
                     (SETF NAME (FOREIGN-STRING-ALLOC "enable")
                           SIGNATURE ""
                           TYPES INTERFACE-ARRAY)))
                  (LET ((INTERFACE-ARRAY
                         (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID))
                                             :COUNT 0))
                        (MSG-PTR
                         (MEM-APTR MESSAGES '(:STRUCT CL-WL.FFI:WL_MESSAGE) 2)))
                    (WITH-FOREIGN-SLOTS
                     ((NAME SIGNATURE TYPES) MSG-PTR
                      (:STRUCT CL-WL.FFI:WL_MESSAGE))
                     (SETF NAME (FOREIGN-STRING-ALLOC "disable")
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
                     (SETF NAME (FOREIGN-STRING-ALLOC "set_surrounding_text")
                           SIGNATURE "sii"
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
                     (SETF NAME (FOREIGN-STRING-ALLOC "set_text_change_cause")
                           SIGNATURE "u"
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
                     (SETF NAME (FOREIGN-STRING-ALLOC "set_content_type")
                           SIGNATURE "uu"
                           TYPES INTERFACE-ARRAY)))
                  (LET ((INTERFACE-ARRAY
                         (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID))
                                             :COUNT 4))
                        (MSG-PTR
                         (MEM-APTR MESSAGES '(:STRUCT CL-WL.FFI:WL_MESSAGE) 6)))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 0) (NULL-POINTER))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 1) (NULL-POINTER))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 2) (NULL-POINTER))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 3) (NULL-POINTER))
                    (WITH-FOREIGN-SLOTS
                     ((NAME SIGNATURE TYPES) MSG-PTR
                      (:STRUCT CL-WL.FFI:WL_MESSAGE))
                     (SETF NAME (FOREIGN-STRING-ALLOC "set_cursor_rectangle")
                           SIGNATURE "iiii"
                           TYPES INTERFACE-ARRAY)))
                  (LET ((INTERFACE-ARRAY
                         (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID))
                                             :COUNT 0))
                        (MSG-PTR
                         (MEM-APTR MESSAGES '(:STRUCT CL-WL.FFI:WL_MESSAGE) 7)))
                    (WITH-FOREIGN-SLOTS
                     ((NAME SIGNATURE TYPES) MSG-PTR
                      (:STRUCT CL-WL.FFI:WL_MESSAGE))
                     (SETF NAME (FOREIGN-STRING-ALLOC "commit")
                           SIGNATURE ""
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
                            WL_SURFACE::*INTERFACE*)
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
                            WL_SURFACE::*INTERFACE*)
                    (WITH-FOREIGN-SLOTS
                     ((NAME SIGNATURE TYPES) MSG-PTR
                      (:STRUCT CL-WL.FFI:WL_MESSAGE))
                     (SETF NAME (FOREIGN-STRING-ALLOC "leave")
                           SIGNATURE "o"
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
                     (SETF NAME (FOREIGN-STRING-ALLOC "preedit_string")
                           SIGNATURE "?sii"
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
                     (SETF NAME (FOREIGN-STRING-ALLOC "commit_string")
                           SIGNATURE "?s"
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
                     (SETF NAME
                             (FOREIGN-STRING-ALLOC "delete_surrounding_text")
                           SIGNATURE "uu"
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
                     (SETF NAME (FOREIGN-STRING-ALLOC "done")
                           SIGNATURE "u"
                           TYPES INTERFACE-ARRAY)))
                  MESSAGES)))
           (WITH-FOREIGN-SLOTS
            ((NAME VERSION METHOD_COUNT METHODS EVENT_COUNT EVENTS) *INTERFACE*
             (:STRUCT INTERFACE))
            (SETF NAME (FOREIGN-STRING-ALLOC "zwp_text_input_v3")
                  VERSION 1
                  METHOD_COUNT 8
                  METHODS REQUESTS-PTR
                  EVENT_COUNT 6
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
                                       "zwp_text_input_v3" "destroy")
                    (FUNCALL 'DESTROY RESOURCE))
                   (1
                    (CL-WL::DEBUG-LOG! "Dispatching ~a:~a~%"
                                       "zwp_text_input_v3" "enable")
                    (FUNCALL 'ENABLE RESOURCE))
                   (2
                    (CL-WL::DEBUG-LOG! "Dispatching ~a:~a~%"
                                       "zwp_text_input_v3" "disable")
                    (FUNCALL 'DISABLE RESOURCE))
                   (3
                    (CL-WL::DEBUG-LOG! "Dispatching ~a:~a~%"
                                       "zwp_text_input_v3"
                                       "set-surrounding-text")
                    (FUNCALL 'SET-SURROUNDING-TEXT RESOURCE
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
                   (4
                    (CL-WL::DEBUG-LOG! "Dispatching ~a:~a~%"
                                       "zwp_text_input_v3"
                                       "set-text-change-cause")
                    (FUNCALL 'SET-TEXT-CHANGE-CAUSE RESOURCE
                             (CHANGE-CAUSE-FROM-VALUE
                              (FOREIGN-SLOT-VALUE
                               (MEM-APTR ARGS '(:UNION CL-WL.FFI:WL_ARGUMENT)
                                0)
                               '(:UNION CL-WL.FFI:WL_ARGUMENT)
                               'WL-FFI::U))))
                   (5
                    (CL-WL::DEBUG-LOG! "Dispatching ~a:~a~%"
                                       "zwp_text_input_v3" "set-content-type")
                    (FUNCALL 'SET-CONTENT-TYPE RESOURCE
                             (CONTENT-HINT-FROM-VALUE
                              (FOREIGN-SLOT-VALUE
                               (MEM-APTR ARGS '(:UNION CL-WL.FFI:WL_ARGUMENT)
                                0)
                               '(:UNION CL-WL.FFI:WL_ARGUMENT)
                               'WL-FFI::U))
                             (CONTENT-PURPOSE-FROM-VALUE
                              (FOREIGN-SLOT-VALUE
                               (MEM-APTR ARGS '(:UNION CL-WL.FFI:WL_ARGUMENT)
                                1)
                               '(:UNION CL-WL.FFI:WL_ARGUMENT)
                               'WL-FFI::U))))
                   (6
                    (CL-WL::DEBUG-LOG! "Dispatching ~a:~a~%"
                                       "zwp_text_input_v3"
                                       "set-cursor-rectangle")
                    (FUNCALL 'SET-CURSOR-RECTANGLE RESOURCE
                             (VALUES
                              (FOREIGN-SLOT-VALUE
                               (MEM-APTR ARGS '(:UNION CL-WL.FFI:WL_ARGUMENT)
                                0)
                               '(:UNION CL-WL.FFI:WL_ARGUMENT)
                               'WL-FFI::I))
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
                               'WL-FFI::I))
                             (VALUES
                              (FOREIGN-SLOT-VALUE
                               (MEM-APTR ARGS '(:UNION CL-WL.FFI:WL_ARGUMENT)
                                3)
                               '(:UNION CL-WL.FFI:WL_ARGUMENT)
                               'WL-FFI::I))))
                   (7
                    (CL-WL::DEBUG-LOG! "Dispatching ~a:~a~%"
                                       "zwp_text_input_v3" "commit")
                    (FUNCALL 'COMMIT RESOURCE)))
     (KILL-CLIENT NIL :REPORT "Kill the client causing errors"
      (CL-WL:DESTROY-CLIENT (CL-WL:CLIENT RESOURCE)) 0)))
 0)

(DEFVAR *DISPATCHER* (CALLBACK DISPATCHER-FFI))

(DEFMETHOD SHARED-INITIALIZE :AFTER ((INSTANCE DISPATCH) SLOT-NAMES &KEY ID)
  (DECLARE (IGNORE SLOT-NAMES))
  (UNLESS (ZWP_TEXT_INPUT_V3-ID INSTANCE)
    (LET* ((RESOURCE
            (CL-WL::CREATE-RESOURCE (CL-WL:PTR (CL-WL:CLIENT INSTANCE))
                                    *INTERFACE* (VERSION INSTANCE)
                                    (OR ID 0))))
      (SETF (CL-WL::TRANSIENT-ID INSTANCE)
              (SETF (ZWP_TEXT_INPUT_V3-ID INSTANCE)
                      (OR ID (CL-WL.FFI:RESOURCE-GET-ID RESOURCE))))
      (SETF (ZWP_TEXT_INPUT_V3-PTR INSTANCE) RESOURCE)
      (SETF (GETHASH (POINTER-ADDRESS RESOURCE) CL-WL::*RESOURCE-TRACKER*)
              INSTANCE)
      (RESOURCE-SET-DISPATCHER RESOURCE *DISPATCHER* (NULL-POINTER)
       (NULL-POINTER) (NULL-POINTER)))))

(DEFMETHOD SEND-ENTER ((DISPATCH DISPATCH) SURFACE)
  (CL-WL::DEBUG-LOG! "Event: ~a~%" "enter")
  (LET ((ARG-LIST (FOREIGN-ALLOC '(:UNION WL_ARGUMENT) :COUNT 1)))
    (SETF (FOREIGN-SLOT-VALUE
           (MEM-AREF ARG-LIST '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 0)
           '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 'WL-FFI::O)
            (WL_SURFACE::WL_SURFACE-PTR SURFACE))
    (RESOURCE-POST-EVENT-ARRAY (ZWP_TEXT_INPUT_V3-PTR DISPATCH) 0
     ARG-LIST)))

(DEFMETHOD SEND-LEAVE ((DISPATCH DISPATCH) SURFACE)
  (CL-WL::DEBUG-LOG! "Event: ~a~%" "leave")
  (LET ((ARG-LIST (FOREIGN-ALLOC '(:UNION WL_ARGUMENT) :COUNT 1)))
    (SETF (FOREIGN-SLOT-VALUE
           (MEM-AREF ARG-LIST '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 0)
           '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 'WL-FFI::O)
            (WL_SURFACE::WL_SURFACE-PTR SURFACE))
    (RESOURCE-POST-EVENT-ARRAY (ZWP_TEXT_INPUT_V3-PTR DISPATCH) 1
     ARG-LIST)))

(DEFMETHOD SEND-PREEDIT-STRING
           ((DISPATCH DISPATCH) TEXT CURSOR_BEGIN
            CURSOR_END)
  (CL-WL::DEBUG-LOG! "Event: ~a~%" "preedit_string")
  (LET ((ARG-LIST (FOREIGN-ALLOC '(:UNION WL_ARGUMENT) :COUNT 3)))
    (SETF (FOREIGN-SLOT-VALUE
           (MEM-AREF ARG-LIST '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 0)
           '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 'WL-FFI::S)
            TEXT)
    (SETF (FOREIGN-SLOT-VALUE
           (MEM-AREF ARG-LIST '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 1)
           '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 'WL-FFI::I)
            CURSOR_BEGIN)
    (SETF (FOREIGN-SLOT-VALUE
           (MEM-AREF ARG-LIST '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 2)
           '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 'WL-FFI::I)
            CURSOR_END)
    (RESOURCE-POST-EVENT-ARRAY (ZWP_TEXT_INPUT_V3-PTR DISPATCH) 2
     ARG-LIST)))

(DEFMETHOD SEND-COMMIT-STRING
           ((DISPATCH DISPATCH) TEXT)
  (CL-WL::DEBUG-LOG! "Event: ~a~%" "commit_string")
  (LET ((ARG-LIST (FOREIGN-ALLOC '(:UNION WL_ARGUMENT) :COUNT 1)))
    (SETF (FOREIGN-SLOT-VALUE
           (MEM-AREF ARG-LIST '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 0)
           '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 'WL-FFI::S)
            TEXT)
    (RESOURCE-POST-EVENT-ARRAY (ZWP_TEXT_INPUT_V3-PTR DISPATCH) 3
     ARG-LIST)))

(DEFMETHOD SEND-DELETE-SURROUNDING-TEXT
           ((DISPATCH DISPATCH) BEFORE_LENGTH
            AFTER_LENGTH)
  (CL-WL::DEBUG-LOG! "Event: ~a~%" "delete_surrounding_text")
  (LET ((ARG-LIST (FOREIGN-ALLOC '(:UNION WL_ARGUMENT) :COUNT 2)))
    (SETF (FOREIGN-SLOT-VALUE
           (MEM-AREF ARG-LIST '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 0)
           '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 'WL-FFI::U)
            BEFORE_LENGTH)
    (SETF (FOREIGN-SLOT-VALUE
           (MEM-AREF ARG-LIST '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 1)
           '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 'WL-FFI::U)
            AFTER_LENGTH)
    (RESOURCE-POST-EVENT-ARRAY (ZWP_TEXT_INPUT_V3-PTR DISPATCH) 4
     ARG-LIST)))

(DEFMETHOD SEND-DONE ((DISPATCH DISPATCH) SERIAL)
  (CL-WL::DEBUG-LOG! "Event: ~a~%" "done")
  (LET ((ARG-LIST (FOREIGN-ALLOC '(:UNION WL_ARGUMENT) :COUNT 1)))
    (SETF (FOREIGN-SLOT-VALUE
           (MEM-AREF ARG-LIST '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 0)
           '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 'WL-FFI::U)
            SERIAL)
    (RESOURCE-POST-EVENT-ARRAY (ZWP_TEXT_INPUT_V3-PTR DISPATCH) 5
     ARG-LIST)))

(DEFUN CHANGE-CAUSE-FROM-VALUE (NUMBER)
  (LOOP FOR ENTRY IN '((0 :INPUT-METHOD) (1 :OTHER))
        FOR VALUE = (CAR ENTRY)
        FOR KEYWORD = (CADR ENTRY)
        WHEN (EQ NUMBER VALUE) RETURN KEYWORD
        FINALLY (ERROR (FORMAT NIL "Unknown enum value: ~a" NUMBER))))

(DEFUN CHANGE-CAUSE-TO-VALUE (KEY)
  (LOOP FOR ENTRY IN '((0 :INPUT-METHOD) (1 :OTHER))
        FOR VALUE = (CAR ENTRY)
        FOR KEYWORD = (CADR ENTRY)
        WHEN (EQ KEY KEYWORD) RETURN VALUE
        FINALLY (ERROR (FORMAT NIL "Unknown enum keyword: ~a" KEY))))

(DEFUN CONTENT-HINT-FROM-VALUE (BITS)
  (LOOP FOR ENTRY IN '((0 :NONE) (1 :COMPLETION)
                       (2 :SPELLCHECK)
                       (4 :AUTO-CAPITALIZATION)
                       (8 :LOWERCASE) (16 :UPPERCASE)
                       (32 :TITLECASE)
                       (64 :HIDDEN-TEXT)
                       (128 :SENSITIVE-DATA)
                       (256 :LATIN) (512 :MULTILINE))
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

(DEFUN CONTENT-HINT-TO-VALUE (KEYWORDS)
  (REDUCE #'+ KEYWORDS :KEY
          (LAMBDA (KEYWORD)
            (OR
             (CADR
              (ASSOC KEYWORD
                     '((:NONE 0) (:COMPLETION 1)
                       (:SPELLCHECK 2)
                       (:AUTO-CAPITALIZATION 4)
                       (:LOWERCASE 8) (:UPPERCASE 16)
                       (:TITLECASE 32)
                       (:HIDDEN-TEXT 64)
                       (:SENSITIVE-DATA 128)
                       (:LATIN 256) (:MULTILINE 512))))
             0))))

(DEFUN CONTENT-PURPOSE-FROM-VALUE (NUMBER)
  (LOOP FOR ENTRY IN '((0 :NORMAL) (1 :ALPHA)
                       (2 :DIGITS) (3 :NUMBER)
                       (4 :PHONE) (5 :URL)
                       (6 :EMAIL) (7 :NAME)
                       (8 :PASSWORD) (9 :PIN)
                       (10 :DATE) (11 :TIME)
                       (12 :DATETIME) (13 :TERMINAL))
        FOR VALUE = (CAR ENTRY)
        FOR KEYWORD = (CADR ENTRY)
        WHEN (EQ NUMBER VALUE) RETURN KEYWORD
        FINALLY (ERROR (FORMAT NIL "Unknown enum value: ~a" NUMBER))))

(DEFUN CONTENT-PURPOSE-TO-VALUE (KEY)
  (LOOP FOR ENTRY IN '((0 :NORMAL) (1 :ALPHA)
                       (2 :DIGITS) (3 :NUMBER)
                       (4 :PHONE) (5 :URL)
                       (6 :EMAIL) (7 :NAME)
                       (8 :PASSWORD) (9 :PIN)
                       (10 :DATE) (11 :TIME)
                       (12 :DATETIME) (13 :TERMINAL))
        FOR VALUE = (CAR ENTRY)
        FOR KEYWORD = (CADR ENTRY)
        WHEN (EQ KEY KEYWORD) RETURN VALUE
        FINALLY (ERROR (FORMAT NIL "Unknown enum keyword: ~a" KEY))))

(DEFCLASS GLOBAL (CL-WL:GLOBAL) NIL
          (:DEFAULT-INITARGS :VERSION 1 :DISPATCH-IMPL 'DISPATCH)
          (:DOCUMENTATION "text input

The zwp_text_input_v3 interface represents text input and input methods
      associated with a seat. It provides enter/leave events to follow the
      text input focus for a seat.

      Requests are used to enable/disable the text-input object and set
      state information like surrounding and selected text or the content type.
      The information about the entered text is sent to the text-input object
      via the preedit_string and commit_string events.

      Text is valid UTF-8 encoded, indices and lengths are in bytes. Indices
      must not point to middle bytes inside a code point: they must either
      point to the first byte of a code point or to the end of the buffer.
      Lengths must be measured between two valid indices.

      Focus moving throughout surfaces will result in the emission of
      zwp_text_input_v3.enter and zwp_text_input_v3.leave events. The focused
      surface must commit zwp_text_input_v3.enable and
      zwp_text_input_v3.disable requests as the keyboard focus moves across
      editable and non-editable elements of the UI. Those two requests are not
      expected to be paired with each other, the compositor must be able to
      handle consecutive series of the same request.

      State is sent by the state requests (set_surrounding_text,
      set_content_type and set_cursor_rectangle) and a commit request. After an
      enter event or disable request all state information is invalidated and
      needs to be resent by the client.
"))

(DEFMETHOD DISPATCH-BIND ((GLOBAL GLOBAL) CLIENT DATA VERSION ID)
  "Default bind implementation for the zwp_text_input_v3 global object.
This can be overriden by inheritance in case if custom behaviour is required."
  (CL-WL::DEBUG-LOG! "Binding ~a~%" "zwp_text_input_v3")
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
  (CL-WL::DEBUG-LOG! "Initializing global object: ~a~%" "zwp_text_input_v3")
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

(IN-PACKAGE :ZWP_TEXT_INPUT_MANAGER_V3)

(DEFCLASS DISPATCH (CL-WL:OBJECT)
          ((ZWP_TEXT_INPUT_MANAGER_V3-ID :INITFORM NIL :ACCESSOR
            ZWP_TEXT_INPUT_MANAGER_V3-ID)
           (ZWP_TEXT_INPUT_MANAGER_V3-PTR :INITFORM NIL :ACCESSOR
            ZWP_TEXT_INPUT_MANAGER_V3-PTR))
          (:DEFAULT-INITARGS :VERSION 1)
          (:DOCUMENTATION "text input manager

A factory for text-input objects. This object is a global singleton.
"))

(DEFGENERIC DESTROY
    (RESOURCE))

(DEFGENERIC GET-TEXT-INPUT
    (RESOURCE ID SEAT))

(DEFMETHOD CL-WL:DESTROY ((DISPATCH DISPATCH))
  (CL-WL::DEBUG-LOG! "Destroying dispatch object: ~a~%"
                     "zwp_text_input_manager_v3")
  (WHEN (CL-WL::DESTROY-CALLBACK DISPATCH)
    (LOOP FOR CALLBACK IN (CL-WL::DESTROY-CALLBACK DISPATCH)
          DO (FUNCALL CALLBACK DISPATCH)))
  (LET ((RESOURCE-PTR (ZWP_TEXT_INPUT_MANAGER_V3-PTR DISPATCH)))
    (CL-WL::REMOVE-RESOURCE (POINTER-ADDRESS RESOURCE-PTR)))
  (CL-WL::DN-IF DISPATCH))

(DEFMETHOD DESTROY ((DISPATCH DISPATCH)) (CL-WL:DESTROY DISPATCH))

(DEFMETHOD DESTROY (EMPTY) NIL)

(PUSHNEW
 (LIST "zwp_text_input_manager_v3" (LIST "zwp_text_input_v3" "wl_seat")
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
                            ZWP_TEXT_INPUT_V3::*INTERFACE*)
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 1)
                            WL_SEAT::*INTERFACE*)
                    (WITH-FOREIGN-SLOTS
                     ((NAME SIGNATURE TYPES) MSG-PTR
                      (:STRUCT CL-WL.FFI:WL_MESSAGE))
                     (SETF NAME (FOREIGN-STRING-ALLOC "get_text_input")
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
            (SETF NAME (FOREIGN-STRING-ALLOC "zwp_text_input_manager_v3")
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
                                       "zwp_text_input_manager_v3" "destroy")
                    (FUNCALL 'DESTROY RESOURCE))
                   (1
                    (CL-WL::DEBUG-LOG! "Dispatching ~a:~a~%"
                                       "zwp_text_input_manager_v3"
                                       "get-text-input")
                    (FUNCALL 'GET-TEXT-INPUT RESOURCE
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
  (UNLESS (ZWP_TEXT_INPUT_MANAGER_V3-ID INSTANCE)
    (LET* ((RESOURCE
            (CL-WL::CREATE-RESOURCE (CL-WL:PTR (CL-WL:CLIENT INSTANCE))
                                    *INTERFACE* (VERSION INSTANCE)
                                    (OR ID 0))))
      (SETF (CL-WL::TRANSIENT-ID INSTANCE)
              (SETF (ZWP_TEXT_INPUT_MANAGER_V3-ID INSTANCE)
                      (OR ID (CL-WL.FFI:RESOURCE-GET-ID RESOURCE))))
      (SETF (ZWP_TEXT_INPUT_MANAGER_V3-PTR INSTANCE) RESOURCE)
      (SETF (GETHASH (POINTER-ADDRESS RESOURCE) CL-WL::*RESOURCE-TRACKER*)
              INSTANCE)
      (RESOURCE-SET-DISPATCHER RESOURCE *DISPATCHER* (NULL-POINTER)
       (NULL-POINTER) (NULL-POINTER)))))

(DEFCLASS GLOBAL (CL-WL:GLOBAL) NIL
          (:DEFAULT-INITARGS :VERSION 1 :DISPATCH-IMPL 'DISPATCH)
          (:DOCUMENTATION "text input manager

A factory for text-input objects. This object is a global singleton.
"))

(DEFMETHOD DISPATCH-BIND ((GLOBAL GLOBAL) CLIENT DATA VERSION ID)
  "Default bind implementation for the zwp_text_input_manager_v3 global object.
This can be overriden by inheritance in case if custom behaviour is required."
  (CL-WL::DEBUG-LOG! "Binding ~a~%" "zwp_text_input_manager_v3")
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
                     "zwp_text_input_manager_v3")
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

