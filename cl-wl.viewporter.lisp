(DEFPACKAGE :WP_VIEWPORTER
  (:USE :CL :CFFI :CL-WL.FFI)
  (:NICKNAMES :WP-VIEWPORTER)
  (:EXPORT DISPATCH
           GLOBAL
           DISPATCH-BIND
           DESTROY
           GET-VIEWPORT))

(IN-PACKAGE :WP_VIEWPORTER)

(DEFCSTRUCT INTERFACE (NAME :STRING) (VERSION :INT) (METHOD_COUNT :INT)
 (METHODS (:POINTER (:STRUCT CL-WL.FFI:WL_MESSAGE))) (EVENT_COUNT :INT)
 (EVENTS (:POINTER (:STRUCT CL-WL.FFI:WL_MESSAGE))))

(DEFVAR *INTERFACE* NIL)

(DEFPACKAGE :WP_VIEWPORT
  (:USE :CL :CFFI :CL-WL.FFI)
  (:NICKNAMES :WP-VIEWPORT)
  (:EXPORT DISPATCH
           GLOBAL
           DISPATCH-BIND
           DESTROY
           SET-SOURCE
           SET-DESTINATION))

(IN-PACKAGE :WP_VIEWPORT)

(DEFCSTRUCT INTERFACE (NAME :STRING) (VERSION :INT) (METHOD_COUNT :INT)
 (METHODS (:POINTER (:STRUCT CL-WL.FFI:WL_MESSAGE))) (EVENT_COUNT :INT)
 (EVENTS (:POINTER (:STRUCT CL-WL.FFI:WL_MESSAGE))))

(DEFVAR *INTERFACE* NIL)

(IN-PACKAGE :WP_VIEWPORTER)

(DEFCLASS DISPATCH (CL-WL:OBJECT)
          ((WP_VIEWPORTER-ID :INITFORM NIL :ACCESSOR
            WP_VIEWPORTER-ID)
           (WP_VIEWPORTER-PTR :INITFORM NIL :ACCESSOR
            WP_VIEWPORTER-PTR))
          (:DEFAULT-INITARGS :VERSION 1)
          (:DOCUMENTATION "surface cropping and scaling

The global interface exposing surface cropping and scaling
      capabilities is used to instantiate an interface extension for a
      wl_surface object. This extended interface will then allow
      cropping and scaling the surface contents, effectively
      disconnecting the direct relationship between the buffer and the
      surface size.
"))

(DEFGENERIC DESTROY
    (RESOURCE))

(DEFGENERIC GET-VIEWPORT
    (RESOURCE ID SURFACE))

(DEFCONTINUE:DEFCONTINUE CL-WL:DESTROY ((DISPATCH DISPATCH))
                         (CL-WL::DEBUG-LOG! "Destroying dispatch object: ~a~%"
                                            "wp_viewporter")
                         (LET ((CLIENT (CL-WL:CLIENT DISPATCH))
                               (RESOURCE-PTR
                                (WP_VIEWPORTER-PTR DISPATCH)))
                           (CL-WL:REMOVE-CLIENT-OBJECT CLIENT
                                                       (WP_VIEWPORTER-ID
                                                        DISPATCH))
                           (CL-WL::REMOVE-RESOURCE
                            (POINTER-ADDRESS RESOURCE-PTR)))
                         (CL-WL::DN-IF DISPATCH))

(DEFMETHOD DESTROY ((DISPATCH DISPATCH)) (CL-WL:DESTROY DISPATCH))

(DEFMETHOD DESTROY (EMPTY) NIL)

(PUSHNEW
 (LIST "wp_viewporter" (LIST "wp_viewport" "wl_surface")
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
                            WP_VIEWPORT::*INTERFACE*)
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 1)
                            WL_SURFACE::*INTERFACE*)
                    (WITH-FOREIGN-SLOTS
                     ((NAME SIGNATURE TYPES) MSG-PTR
                      (:STRUCT CL-WL.FFI:WL_MESSAGE))
                     (SETF NAME (FOREIGN-STRING-ALLOC "get_viewport")
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
            (SETF NAME (FOREIGN-STRING-ALLOC "wp_viewporter")
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
                    (CL-WL::DEBUG-LOG! "Dispatching ~a:~a~%" "wp_viewporter"
                                       "destroy")
                    (FUNCALL 'DESTROY RESOURCE))
                   (1
                    (CL-WL::DEBUG-LOG! "Dispatching ~a:~a~%" "wp_viewporter"
                                       "get-viewport")
                    (FUNCALL 'GET-VIEWPORT RESOURCE
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
  (UNLESS (WP_VIEWPORTER-ID INSTANCE)
    (LET* ((RESOURCE
            (CL-WL::CREATE-RESOURCE (CL-WL:PTR (CL-WL:CLIENT INSTANCE))
                                    *INTERFACE* (VERSION INSTANCE)
                                    (OR ID 0))))
      (SETF (CL-WL::TRANSIENT-ID INSTANCE)
              (SETF (WP_VIEWPORTER-ID INSTANCE)
                      (OR ID (CL-WL.FFI:RESOURCE-GET-ID RESOURCE))))
      (SETF (WP_VIEWPORTER-PTR INSTANCE) RESOURCE)
      (SETF (GETHASH (POINTER-ADDRESS RESOURCE) CL-WL::*RESOURCE-TRACKER*)
              INSTANCE)
      (RESOURCE-SET-DISPATCHER RESOURCE *DISPATCHER* (NULL-POINTER)
       (NULL-POINTER) (NULL-POINTER)))))

(DEFUN ERROR-FROM-VALUE (NUMBER)
  (LOOP FOR ENTRY IN '((0 :VIEWPORT-EXISTS))
        FOR VALUE = (CAR ENTRY)
        FOR KEYWORD = (CADR ENTRY)
        WHEN (EQ NUMBER VALUE) RETURN KEYWORD
        FINALLY (ERROR (FORMAT NIL "Unknown enum value: ~a" NUMBER))))

(DEFUN ERROR-TO-VALUE (KEY)
  (LOOP FOR ENTRY IN '((0 :VIEWPORT-EXISTS))
        FOR VALUE = (CAR ENTRY)
        FOR KEYWORD = (CADR ENTRY)
        WHEN (EQ KEY KEYWORD) RETURN VALUE
        FINALLY (ERROR (FORMAT NIL "Unknown enum keyword: ~a" KEY))))

(DEFCLASS GLOBAL (CL-WL:GLOBAL) NIL
          (:DEFAULT-INITARGS :VERSION 1 :DISPATCH-IMPL 'DISPATCH)
          (:DOCUMENTATION "surface cropping and scaling

The global interface exposing surface cropping and scaling
      capabilities is used to instantiate an interface extension for a
      wl_surface object. This extended interface will then allow
      cropping and scaling the surface contents, effectively
      disconnecting the direct relationship between the buffer and the
      surface size.
"))

(DEFMETHOD DISPATCH-BIND ((GLOBAL GLOBAL) CLIENT DATA VERSION ID)
  "Default bind implementation for the wp_viewporter global object.
This can be overriden by inheritance in case if custom behaviour is required."
  (CL-WL::DEBUG-LOG! "Binding ~a~%" "wp_viewporter")
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
  (CL-WL::DEBUG-LOG! "Initializing global object: ~a~%" "wp_viewporter")
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

(IN-PACKAGE :WP_VIEWPORT)

(DEFCLASS DISPATCH (CL-WL:OBJECT)
          ((WP_VIEWPORT-ID :INITFORM NIL :ACCESSOR
            WP_VIEWPORT-ID)
           (WP_VIEWPORT-PTR :INITFORM NIL :ACCESSOR
            WP_VIEWPORT-PTR))
          (:DEFAULT-INITARGS :VERSION 1)
          (:DOCUMENTATION "crop and scale interface to a wl_surface

An additional interface to a wl_surface object, which allows the
      client to specify the cropping and scaling of the surface
      contents.

      This interface works with two concepts: the source rectangle (src_x,
      src_y, src_width, src_height), and the destination size (dst_width,
      dst_height). The contents of the source rectangle are scaled to the
      destination size, and content outside the source rectangle is ignored.
      This state is double-buffered, see wl_surface.commit.

      The two parts of crop and scale state are independent: the source
      rectangle, and the destination size. Initially both are unset, that
      is, no scaling is applied. The whole of the current wl_buffer is
      used as the source, and the surface size is as defined in
      wl_surface.attach.

      If the destination size is set, it causes the surface size to become
      dst_width, dst_height. The source (rectangle) is scaled to exactly
      this size. This overrides whatever the attached wl_buffer size is,
      unless the wl_buffer is NULL. If the wl_buffer is NULL, the surface
      has no content and therefore no size. Otherwise, the size is always
      at least 1x1 in surface local coordinates.

      If the source rectangle is set, it defines what area of the wl_buffer is
      taken as the source. If the source rectangle is set and the destination
      size is not set, then src_width and src_height must be integers, and the
      surface size becomes the source rectangle size. This results in cropping
      without scaling. If src_width or src_height are not integers and
      destination size is not set, the bad_size protocol error is raised when
      the surface state is applied.

      The coordinate transformations from buffer pixel coordinates up to
      the surface-local coordinates happen in the following order:
        1. buffer_transform (wl_surface.set_buffer_transform)
        2. buffer_scale (wl_surface.set_buffer_scale)
        3. crop and scale (wp_viewport.set*)
      This means, that the source rectangle coordinates of crop and scale
      are given in the coordinates after the buffer transform and scale,
      i.e. in the coordinates that would be the surface-local coordinates
      if the crop and scale was not applied.

      If src_x or src_y are negative, the bad_value protocol error is raised.
      Otherwise, if the source rectangle is partially or completely outside of
      the non-NULL wl_buffer, then the out_of_buffer protocol error is raised
      when the surface state is applied. A NULL wl_buffer does not raise the
      out_of_buffer error.

      If the wl_surface associated with the wp_viewport is destroyed,
      all wp_viewport requests except 'destroy' raise the protocol error
      no_surface.

      If the wp_viewport object is destroyed, the crop and scale
      state is removed from the wl_surface. The change will be applied
      on the next wl_surface.commit.
"))

(DEFGENERIC DESTROY
    (RESOURCE))

(DEFGENERIC SET-SOURCE
    (RESOURCE X Y WIDTH HEIGHT))

(DEFGENERIC SET-DESTINATION
    (RESOURCE WIDTH HEIGHT))

(DEFCONTINUE:DEFCONTINUE CL-WL:DESTROY ((DISPATCH DISPATCH))
                         (CL-WL::DEBUG-LOG! "Destroying dispatch object: ~a~%"
                                            "wp_viewport")
                         (LET ((CLIENT (CL-WL:CLIENT DISPATCH))
                               (RESOURCE-PTR
                                (WP_VIEWPORT-PTR DISPATCH)))
                           (CL-WL:REMOVE-CLIENT-OBJECT CLIENT
                                                       (WP_VIEWPORT-ID
                                                        DISPATCH))
                           (CL-WL::REMOVE-RESOURCE
                            (POINTER-ADDRESS RESOURCE-PTR)))
                         (CL-WL::DN-IF DISPATCH))

(DEFMETHOD DESTROY ((DISPATCH DISPATCH)) (CL-WL:DESTROY DISPATCH))

(DEFMETHOD DESTROY (EMPTY) NIL)

(PUSHNEW
 (LIST "wp_viewport" (LIST)
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
                     (SETF NAME (FOREIGN-STRING-ALLOC "set_source")
                           SIGNATURE "ffff"
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
                     (SETF NAME (FOREIGN-STRING-ALLOC "set_destination")
                           SIGNATURE "ii"
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
            (SETF NAME (FOREIGN-STRING-ALLOC "wp_viewport")
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
   (RESTART-CASE (ECASE OPCODE
                   (0
                    (CL-WL::DEBUG-LOG! "Dispatching ~a:~a~%" "wp_viewport"
                                       "destroy")
                    (FUNCALL 'DESTROY RESOURCE))
                   (1
                    (CL-WL::DEBUG-LOG! "Dispatching ~a:~a~%" "wp_viewport"
                                       "set-source")
                    (FUNCALL 'SET-SOURCE RESOURCE
                             (VALUES
                              (FOREIGN-SLOT-VALUE
                               (MEM-APTR ARGS '(:UNION CL-WL.FFI:WL_ARGUMENT)
                                0)
                               '(:UNION CL-WL.FFI:WL_ARGUMENT)
                               'WL-FFI::F))
                             (VALUES
                              (FOREIGN-SLOT-VALUE
                               (MEM-APTR ARGS '(:UNION CL-WL.FFI:WL_ARGUMENT)
                                1)
                               '(:UNION CL-WL.FFI:WL_ARGUMENT)
                               'WL-FFI::F))
                             (VALUES
                              (FOREIGN-SLOT-VALUE
                               (MEM-APTR ARGS '(:UNION CL-WL.FFI:WL_ARGUMENT)
                                2)
                               '(:UNION CL-WL.FFI:WL_ARGUMENT)
                               'WL-FFI::F))
                             (VALUES
                              (FOREIGN-SLOT-VALUE
                               (MEM-APTR ARGS '(:UNION CL-WL.FFI:WL_ARGUMENT)
                                3)
                               '(:UNION CL-WL.FFI:WL_ARGUMENT)
                               'WL-FFI::F))))
                   (2
                    (CL-WL::DEBUG-LOG! "Dispatching ~a:~a~%" "wp_viewport"
                                       "set-destination")
                    (FUNCALL 'SET-DESTINATION RESOURCE
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
                               'WL-FFI::I)))))
     (KILL-CLIENT NIL :REPORT "Kill the client causing errors"
      (CL-WL:DESTROY-CLIENT (CL-WL:CLIENT RESOURCE)) 0)))
 0)

(DEFVAR *DISPATCHER* (CALLBACK DISPATCHER-FFI))

(DEFMETHOD SHARED-INITIALIZE :AFTER ((INSTANCE DISPATCH) SLOT-NAMES &KEY ID)
  (DECLARE (IGNORE SLOT-NAMES))
  (UNLESS (WP_VIEWPORT-ID INSTANCE)
    (LET* ((RESOURCE
            (CL-WL::CREATE-RESOURCE (CL-WL:PTR (CL-WL:CLIENT INSTANCE))
                                    *INTERFACE* (VERSION INSTANCE)
                                    (OR ID 0))))
      (SETF (CL-WL::TRANSIENT-ID INSTANCE)
              (SETF (WP_VIEWPORT-ID INSTANCE)
                      (OR ID (CL-WL.FFI:RESOURCE-GET-ID RESOURCE))))
      (SETF (WP_VIEWPORT-PTR INSTANCE) RESOURCE)
      (SETF (GETHASH (POINTER-ADDRESS RESOURCE) CL-WL::*RESOURCE-TRACKER*)
              INSTANCE)
      (RESOURCE-SET-DISPATCHER RESOURCE *DISPATCHER* (NULL-POINTER)
       (NULL-POINTER) (NULL-POINTER)))))

(DEFUN ERROR-FROM-VALUE (NUMBER)
  (LOOP FOR ENTRY IN '((0 :BAD-VALUE) (1 :BAD-SIZE)
                       (2 :OUT-OF-BUFFER)
                       (3 :NO-SURFACE))
        FOR VALUE = (CAR ENTRY)
        FOR KEYWORD = (CADR ENTRY)
        WHEN (EQ NUMBER VALUE) RETURN KEYWORD
        FINALLY (ERROR (FORMAT NIL "Unknown enum value: ~a" NUMBER))))

(DEFUN ERROR-TO-VALUE (KEY)
  (LOOP FOR ENTRY IN '((0 :BAD-VALUE) (1 :BAD-SIZE)
                       (2 :OUT-OF-BUFFER)
                       (3 :NO-SURFACE))
        FOR VALUE = (CAR ENTRY)
        FOR KEYWORD = (CADR ENTRY)
        WHEN (EQ KEY KEYWORD) RETURN VALUE
        FINALLY (ERROR (FORMAT NIL "Unknown enum keyword: ~a" KEY))))

(DEFCLASS GLOBAL (CL-WL:GLOBAL) NIL
          (:DEFAULT-INITARGS :VERSION 1 :DISPATCH-IMPL 'DISPATCH)
          (:DOCUMENTATION "crop and scale interface to a wl_surface

An additional interface to a wl_surface object, which allows the
      client to specify the cropping and scaling of the surface
      contents.

      This interface works with two concepts: the source rectangle (src_x,
      src_y, src_width, src_height), and the destination size (dst_width,
      dst_height). The contents of the source rectangle are scaled to the
      destination size, and content outside the source rectangle is ignored.
      This state is double-buffered, see wl_surface.commit.

      The two parts of crop and scale state are independent: the source
      rectangle, and the destination size. Initially both are unset, that
      is, no scaling is applied. The whole of the current wl_buffer is
      used as the source, and the surface size is as defined in
      wl_surface.attach.

      If the destination size is set, it causes the surface size to become
      dst_width, dst_height. The source (rectangle) is scaled to exactly
      this size. This overrides whatever the attached wl_buffer size is,
      unless the wl_buffer is NULL. If the wl_buffer is NULL, the surface
      has no content and therefore no size. Otherwise, the size is always
      at least 1x1 in surface local coordinates.

      If the source rectangle is set, it defines what area of the wl_buffer is
      taken as the source. If the source rectangle is set and the destination
      size is not set, then src_width and src_height must be integers, and the
      surface size becomes the source rectangle size. This results in cropping
      without scaling. If src_width or src_height are not integers and
      destination size is not set, the bad_size protocol error is raised when
      the surface state is applied.

      The coordinate transformations from buffer pixel coordinates up to
      the surface-local coordinates happen in the following order:
        1. buffer_transform (wl_surface.set_buffer_transform)
        2. buffer_scale (wl_surface.set_buffer_scale)
        3. crop and scale (wp_viewport.set*)
      This means, that the source rectangle coordinates of crop and scale
      are given in the coordinates after the buffer transform and scale,
      i.e. in the coordinates that would be the surface-local coordinates
      if the crop and scale was not applied.

      If src_x or src_y are negative, the bad_value protocol error is raised.
      Otherwise, if the source rectangle is partially or completely outside of
      the non-NULL wl_buffer, then the out_of_buffer protocol error is raised
      when the surface state is applied. A NULL wl_buffer does not raise the
      out_of_buffer error.

      If the wl_surface associated with the wp_viewport is destroyed,
      all wp_viewport requests except 'destroy' raise the protocol error
      no_surface.

      If the wp_viewport object is destroyed, the crop and scale
      state is removed from the wl_surface. The change will be applied
      on the next wl_surface.commit.
"))

(DEFMETHOD DISPATCH-BIND ((GLOBAL GLOBAL) CLIENT DATA VERSION ID)
  "Default bind implementation for the wp_viewport global object.
This can be overriden by inheritance in case if custom behaviour is required."
  (CL-WL::DEBUG-LOG! "Binding ~a~%" "wp_viewport")
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
  (CL-WL::DEBUG-LOG! "Initializing global object: ~a~%" "wp_viewport")
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

