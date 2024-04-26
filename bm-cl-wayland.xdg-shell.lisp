(DEFPACKAGE :XDG_WM_BASE
  (:USE :CL :WL :WL-FFI :CFFI)
  (:NICKNAMES :XDG-WM-BASE)
  (:EXPORT DISPATCH
           GLOBAL
           DISPATCH-BIND
           DESTROY
           CREATE-POSITIONER
           GET-XDG-SURFACE
           PONG
           SEND-PING))

(IN-PACKAGE :XDG_WM_BASE)

(DEFCSTRUCT INTERFACE (NAME :STRING) (VERSION :INT) (METHOD_COUNT :INT)
 (METHODS (:POINTER (:STRUCT WL_MESSAGE))) (EVENT_COUNT :INT)
 (EVENTS (:POINTER (:STRUCT WL_MESSAGE))))

(DEFVAR *INTERFACE* NIL)

(DEFPACKAGE :XDG_POSITIONER
  (:USE :CL :WL :WL-FFI :CFFI)
  (:NICKNAMES :XDG-POSITIONER)
  (:EXPORT DISPATCH
           GLOBAL
           DISPATCH-BIND
           DESTROY
           SET-SIZE
           SET-ANCHOR-RECT
           SET-ANCHOR
           SET-GRAVITY
           SET-CONSTRAINT-ADJUSTMENT
           SET-OFFSET
           SET-REACTIVE
           SET-PARENT-SIZE
           SET-PARENT-CONFIGURE))

(IN-PACKAGE :XDG_POSITIONER)

(DEFCSTRUCT INTERFACE (NAME :STRING) (VERSION :INT) (METHOD_COUNT :INT)
 (METHODS (:POINTER (:STRUCT WL_MESSAGE))) (EVENT_COUNT :INT)
 (EVENTS (:POINTER (:STRUCT WL_MESSAGE))))

(DEFVAR *INTERFACE* NIL)

(DEFPACKAGE :XDG_SURFACE
  (:USE :CL :WL :WL-FFI :CFFI)
  (:NICKNAMES :XDG-SURFACE)
  (:EXPORT DISPATCH
           GLOBAL
           DISPATCH-BIND
           DESTROY
           GET-TOPLEVEL
           GET-POPUP
           SET-WINDOW-GEOMETRY
           ACK-CONFIGURE
           SEND-CONFIGURE))

(IN-PACKAGE :XDG_SURFACE)

(DEFCSTRUCT INTERFACE (NAME :STRING) (VERSION :INT) (METHOD_COUNT :INT)
 (METHODS (:POINTER (:STRUCT WL_MESSAGE))) (EVENT_COUNT :INT)
 (EVENTS (:POINTER (:STRUCT WL_MESSAGE))))

(DEFVAR *INTERFACE* NIL)

(DEFPACKAGE :XDG_TOPLEVEL
  (:USE :CL :WL :WL-FFI :CFFI)
  (:NICKNAMES :XDG-TOPLEVEL)
  (:EXPORT DISPATCH
           GLOBAL
           DISPATCH-BIND
           DESTROY
           SET-PARENT
           SET-TITLE
           SET-APP-ID
           SHOW-WINDOW-MENU
           MOVE
           RESIZE
           SET-MAX-SIZE
           SET-MIN-SIZE
           SET-MAXIMIZED
           UNSET-MAXIMIZED
           SET-FULLSCREEN
           UNSET-FULLSCREEN
           SET-MINIMIZED
           SEND-CONFIGURE
           SEND-CLOSE
           SEND-CONFIGURE-BOUNDS
           SEND-WM-CAPABILITIES))

(IN-PACKAGE :XDG_TOPLEVEL)

(DEFCSTRUCT INTERFACE (NAME :STRING) (VERSION :INT) (METHOD_COUNT :INT)
 (METHODS (:POINTER (:STRUCT WL_MESSAGE))) (EVENT_COUNT :INT)
 (EVENTS (:POINTER (:STRUCT WL_MESSAGE))))

(DEFVAR *INTERFACE* NIL)

(DEFPACKAGE :XDG_POPUP
  (:USE :CL :WL :WL-FFI :CFFI)
  (:NICKNAMES :XDG-POPUP)
  (:EXPORT DISPATCH
           GLOBAL
           DISPATCH-BIND
           DESTROY
           GRAB
           REPOSITION
           SEND-CONFIGURE
           SEND-POPUP-DONE
           SEND-REPOSITIONED))

(IN-PACKAGE :XDG_POPUP)

(DEFCSTRUCT INTERFACE (NAME :STRING) (VERSION :INT) (METHOD_COUNT :INT)
 (METHODS (:POINTER (:STRUCT WL_MESSAGE))) (EVENT_COUNT :INT)
 (EVENTS (:POINTER (:STRUCT WL_MESSAGE))))

(DEFVAR *INTERFACE* NIL)

(IN-PACKAGE :XDG_WM_BASE)

(DEFCLASS DISPATCH (BM-CL-WAYLAND:OBJECT) NIL (:DEFAULT-INITARGS :VERSION 6)
          (:DOCUMENTATION "create desktop-style surfaces

The xdg_wm_base interface is exposed as a global object enabling clients
      to turn their wl_surfaces into windows in a desktop environment. It
      defines the basic functionality needed for clients and the compositor to
      create windows that can be dragged, resized, maximized, etc, as well as
      creating transient windows such as popup menus.
"))

(DEFGENERIC DESTROY
    (RESOURCE))

(DEFGENERIC CREATE-POSITIONER
    (RESOURCE ID))

(DEFGENERIC GET-XDG-SURFACE
    (RESOURCE ID SURFACE))

(DEFGENERIC PONG
    (RESOURCE SERIAL))

(PUSHNEW
 (LIST "xdg_wm_base" (LIST "xdg_positioner" "xdg_surface" "wl_surface")
       (LAMBDA ()
         (SETF *INTERFACE* (FOREIGN-ALLOC '(:STRUCT INTERFACE)))
         (LET ((REQUESTS-PTR
                (LET ((MESSAGES
                       (CFFI:FOREIGN-ALLOC '(:STRUCT WL_MESSAGE) :COUNT 4)))
                  (LET ((INTERFACE-ARRAY
                         (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID))
                                             :COUNT 0))
                        (MSG-PTR (MEM-APTR MESSAGES '(:STRUCT WL_MESSAGE) 0)))
                    (WITH-FOREIGN-SLOTS
                     ((NAME SIGNATURE TYPES) MSG-PTR (:STRUCT WL_MESSAGE))
                     (SETF NAME (FOREIGN-STRING-ALLOC "destroy")
                           SIGNATURE ""
                           TYPES INTERFACE-ARRAY)))
                  (LET ((INTERFACE-ARRAY
                         (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID))
                                             :COUNT 1))
                        (MSG-PTR (MEM-APTR MESSAGES '(:STRUCT WL_MESSAGE) 1)))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 0)
                            XDG_POSITIONER::*INTERFACE*)
                    (WITH-FOREIGN-SLOTS
                     ((NAME SIGNATURE TYPES) MSG-PTR (:STRUCT WL_MESSAGE))
                     (SETF NAME (FOREIGN-STRING-ALLOC "create_positioner")
                           SIGNATURE "n"
                           TYPES INTERFACE-ARRAY)))
                  (LET ((INTERFACE-ARRAY
                         (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID))
                                             :COUNT 2))
                        (MSG-PTR (MEM-APTR MESSAGES '(:STRUCT WL_MESSAGE) 2)))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 0)
                            XDG_SURFACE::*INTERFACE*)
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 1)
                            WL_SURFACE::*INTERFACE*)
                    (WITH-FOREIGN-SLOTS
                     ((NAME SIGNATURE TYPES) MSG-PTR (:STRUCT WL_MESSAGE))
                     (SETF NAME (FOREIGN-STRING-ALLOC "get_xdg_surface")
                           SIGNATURE "no"
                           TYPES INTERFACE-ARRAY)))
                  (LET ((INTERFACE-ARRAY
                         (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID))
                                             :COUNT 1))
                        (MSG-PTR (MEM-APTR MESSAGES '(:STRUCT WL_MESSAGE) 3)))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 0) (NULL-POINTER))
                    (WITH-FOREIGN-SLOTS
                     ((NAME SIGNATURE TYPES) MSG-PTR (:STRUCT WL_MESSAGE))
                     (SETF NAME (FOREIGN-STRING-ALLOC "pong")
                           SIGNATURE "u"
                           TYPES INTERFACE-ARRAY)))
                  MESSAGES))
               (EVENTS-PTR
                (LET ((MESSAGES
                       (CFFI:FOREIGN-ALLOC '(:STRUCT WL_MESSAGE) :COUNT 1)))
                  (LET ((INTERFACE-ARRAY
                         (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID))
                                             :COUNT 1))
                        (MSG-PTR (MEM-APTR MESSAGES '(:STRUCT WL_MESSAGE) 0)))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 0) (NULL-POINTER))
                    (WITH-FOREIGN-SLOTS
                     ((NAME SIGNATURE TYPES) MSG-PTR (:STRUCT WL_MESSAGE))
                     (SETF NAME (FOREIGN-STRING-ALLOC "ping")
                           SIGNATURE "u"
                           TYPES INTERFACE-ARRAY)))
                  MESSAGES)))
           (DEBUG-LOG! "IF ptr range: ~a --- ~a~%" *INTERFACE*
            (MEM-APTR *INTERFACE* '(:STRUCT INTERFACE) 1))
           (WITH-FOREIGN-SLOTS
            ((NAME VERSION METHOD_COUNT METHODS EVENT_COUNT EVENTS) *INTERFACE*
             (:STRUCT INTERFACE))
            (SETF NAME (FOREIGN-STRING-ALLOC "xdg_wm_base")
                  VERSION 6
                  METHOD_COUNT 4
                  METHODS REQUESTS-PTR
                  EVENT_COUNT 1
                  EVENTS EVENTS-PTR)))))
 BM-CL-WAYLAND::*INTERFACE-INIT-LIST* :TEST #'INTERFACE-EXISTS-TEST)

(CL-ASYNC-UTIL:DEFINE-C-CALLBACK DISPATCHER-FFI
    :INT
    ((DATA :POINTER) (TARGET :POINTER) (OPCODE :UINT) (MESSAGE :POINTER)
     (ARGS :POINTER))
  (DECLARE (IGNORE DATA MESSAGE))
  (DEBUG-LOG! "Dispatcher invoked: ~a~%" "xdg_wm_base")
  (LET ((RESOURCE (GETHASH (POINTER-ADDRESS TARGET) *RESOURCE-TRACKER*)))
    (ECASE OPCODE
      (0 (FUNCALL 'DESTROY RESOURCE))
      (1
       (FUNCALL 'CREATE-POSITIONER RESOURCE
                (VALUES
                 (FOREIGN-SLOT-VALUE
                  (MEM-APTR ARGS '(:UNION BM-CL-LIBWAYLAND:WL_ARGUMENT) 0)
                  '(:UNION BM-CL-LIBWAYLAND:WL_ARGUMENT)
                  'WL-FFI::N))))
      (2
       (FUNCALL 'GET-XDG-SURFACE RESOURCE
                (VALUES
                 (FOREIGN-SLOT-VALUE
                  (MEM-APTR ARGS '(:UNION BM-CL-LIBWAYLAND:WL_ARGUMENT) 0)
                  '(:UNION BM-CL-LIBWAYLAND:WL_ARGUMENT)
                  'WL-FFI::N))
                (GETHASH
                 (POINTER-ADDRESS
                  (FOREIGN-SLOT-VALUE
                   (MEM-APTR ARGS '(:UNION BM-CL-LIBWAYLAND:WL_ARGUMENT) 1)
                   '(:UNION BM-CL-LIBWAYLAND:WL_ARGUMENT)
                   'WL-FFI::O))
                 *RESOURCE-TRACKER*)))
      (3
       (FUNCALL 'PONG RESOURCE
                (VALUES
                 (FOREIGN-SLOT-VALUE
                  (MEM-APTR ARGS '(:UNION BM-CL-LIBWAYLAND:WL_ARGUMENT) 0)
                  '(:UNION BM-CL-LIBWAYLAND:WL_ARGUMENT)
                  'WL-FFI::U))))))
  0)

(DEFVAR *DISPATCHER* (CALLBACK DISPATCHER-FFI))

(DEFMETHOD INITIALIZE-INSTANCE :AFTER ((INSTANCE DISPATCH) &KEY)
  (LET* ((RESOURCE
          (CREATE-RESOURCE (PTR (CLIENT INSTANCE)) *INTERFACE*
           (VERSION INSTANCE) (ID INSTANCE))))
    (SETF (GETHASH (POINTER-ADDRESS RESOURCE) *RESOURCE-TRACKER*) INSTANCE)
    (SETF (PTR INSTANCE) RESOURCE)
    (RESOURCE-SET-DISPATCHER RESOURCE *DISPATCHER* (NULL-POINTER)
     (NULL-POINTER) (NULL-POINTER))))

(DEFMETHOD SEND-PING ((DISPATCH DISPATCH) SERIAL)
  (DEBUG-LOG! "Event: ~a~%" "ping")
  (LET ((ARG-LIST (FOREIGN-ALLOC '(:UNION WL_ARGUMENT) :COUNT 1)))
    (SETF (FOREIGN-SLOT-VALUE
           (MEM-AREF ARG-LIST '(:UNION BM-CL-LIBWAYLAND:WL_ARGUMENT) 0)
           '(:UNION BM-CL-LIBWAYLAND:WL_ARGUMENT) 'WL-FFI::U)
            SERIAL)
    (RESOURCE-POST-EVENT-ARRAY (PTR DISPATCH) 0 ARG-LIST)))

(DEFCLASS GLOBAL (BM-CL-WAYLAND::GLOBAL) NIL
          (:DEFAULT-INITARGS :VERSION 6 :DISPATCH-IMPL 'DISPATCH)
          (:DOCUMENTATION "create desktop-style surfaces

The xdg_wm_base interface is exposed as a global object enabling clients
      to turn their wl_surfaces into windows in a desktop environment. It
      defines the basic functionality needed for clients and the compositor to
      create windows that can be dragged, resized, maximized, etc, as well as
      creating transient windows such as popup menus.
"))

(DEFMETHOD DISPATCH-BIND ((GLOBAL GLOBAL) CLIENT DATA VERSION ID)
  "Default bind implementation for the xdg_wm_base global object.
This can be overriden by inheritance in case if custom behaviour is required."
  (DEBUG-LOG! "Binding ~a~%" "xdg_wm_base")
  (LET ((BOUND
         (MAKE-INSTANCE (DISPATCH-IMPL GLOBAL) :DISPLAY (DISPLAY CLIENT)
                        :CLIENT CLIENT :ID ID)))
    (SETF (IFACE CLIENT ID) BOUND)))

(CL-ASYNC-UTIL:DEFINE-C-CALLBACK DISPATCH-BIND-FFI
    :VOID
    ((CLIENT :POINTER) (DATA :POINTER) (VERSION :UINT) (ID :UINT))
  (DEBUG-LOG! "C-Binding ~a~%" "xdg_wm_base")
  (LET* ((CLIENT (GET-CLIENT CLIENT)) (GLOBAL (GET-DATA DATA)))
    (FUNCALL 'DISPATCH-BIND GLOBAL CLIENT (NULL-POINTER) VERSION ID)))

(DEFVAR *DISPATCH-BIND* (CALLBACK DISPATCH-BIND-FFI))

(DEFMETHOD INITIALIZE-INSTANCE :AFTER ((GLOBAL GLOBAL) &KEY)
  (DEBUG-LOG! "Initializing global object: ~a~%" "xdg_wm_base")
  (LET* ((NEXT-DATA-ID (RESERVE-DATA))
         (GLOBAL-PTR
          (GLOBAL-CREATE (DISPLAY GLOBAL) *INTERFACE* (VERSION GLOBAL)
           (DATA-PTR NEXT-DATA-ID) *DISPATCH-BIND*)))
    (SETF (BM-CL-WAYLAND:PTR GLOBAL) GLOBAL-PTR)
    (SET-DATA NEXT-DATA-ID
     (SETF (GETHASH (POINTER-ADDRESS GLOBAL-PTR) *GLOBAL-TRACKER*) GLOBAL))))

(IN-PACKAGE :XDG_POSITIONER)

(DEFCLASS DISPATCH (BM-CL-WAYLAND:OBJECT) NIL (:DEFAULT-INITARGS :VERSION 6)
          (:DOCUMENTATION "child surface positioner

The xdg_positioner provides a collection of rules for the placement of a
      child surface relative to a parent surface. Rules can be defined to ensure
      the child surface remains within the visible area's borders, and to
      specify how the child surface changes its position, such as sliding along
      an axis, or flipping around a rectangle. These positioner-created rules are
      constrained by the requirement that a child surface must intersect with or
      be at least partially adjacent to its parent surface.

      See the various requests for details about possible rules.

      At the time of the request, the compositor makes a copy of the rules
      specified by the xdg_positioner. Thus, after the request is complete the
      xdg_positioner object can be destroyed or reused; further changes to the
      object will have no effect on previous usages.

      For an xdg_positioner object to be considered complete, it must have a
      non-zero size set by set_size, and a non-zero anchor rectangle set by
      set_anchor_rect. Passing an incomplete xdg_positioner object when
      positioning a surface raises an invalid_positioner error.
"))

(DEFGENERIC DESTROY
    (RESOURCE))

(DEFGENERIC SET-SIZE
    (RESOURCE WIDTH HEIGHT))

(DEFGENERIC SET-ANCHOR-RECT
    (RESOURCE X Y WIDTH HEIGHT))

(DEFGENERIC SET-ANCHOR
    (RESOURCE ANCHOR))

(DEFGENERIC SET-GRAVITY
    (RESOURCE GRAVITY))

(DEFGENERIC SET-CONSTRAINT-ADJUSTMENT
    (RESOURCE CONSTRAINT_ADJUSTMENT))

(DEFGENERIC SET-OFFSET
    (RESOURCE X Y))

(DEFGENERIC SET-REACTIVE
    (RESOURCE))

(DEFGENERIC SET-PARENT-SIZE
    (RESOURCE PARENT_WIDTH PARENT_HEIGHT))

(DEFGENERIC SET-PARENT-CONFIGURE
    (RESOURCE SERIAL))

(PUSHNEW
 (LIST "xdg_positioner" (LIST)
       (LAMBDA ()
         (SETF *INTERFACE* (FOREIGN-ALLOC '(:STRUCT INTERFACE)))
         (LET ((REQUESTS-PTR
                (LET ((MESSAGES
                       (CFFI:FOREIGN-ALLOC '(:STRUCT WL_MESSAGE) :COUNT 10)))
                  (LET ((INTERFACE-ARRAY
                         (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID))
                                             :COUNT 0))
                        (MSG-PTR (MEM-APTR MESSAGES '(:STRUCT WL_MESSAGE) 0)))
                    (WITH-FOREIGN-SLOTS
                     ((NAME SIGNATURE TYPES) MSG-PTR (:STRUCT WL_MESSAGE))
                     (SETF NAME (FOREIGN-STRING-ALLOC "destroy")
                           SIGNATURE ""
                           TYPES INTERFACE-ARRAY)))
                  (LET ((INTERFACE-ARRAY
                         (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID))
                                             :COUNT 2))
                        (MSG-PTR (MEM-APTR MESSAGES '(:STRUCT WL_MESSAGE) 1)))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 0) (NULL-POINTER))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 1) (NULL-POINTER))
                    (WITH-FOREIGN-SLOTS
                     ((NAME SIGNATURE TYPES) MSG-PTR (:STRUCT WL_MESSAGE))
                     (SETF NAME (FOREIGN-STRING-ALLOC "set_size")
                           SIGNATURE "ii"
                           TYPES INTERFACE-ARRAY)))
                  (LET ((INTERFACE-ARRAY
                         (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID))
                                             :COUNT 4))
                        (MSG-PTR (MEM-APTR MESSAGES '(:STRUCT WL_MESSAGE) 2)))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 0) (NULL-POINTER))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 1) (NULL-POINTER))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 2) (NULL-POINTER))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 3) (NULL-POINTER))
                    (WITH-FOREIGN-SLOTS
                     ((NAME SIGNATURE TYPES) MSG-PTR (:STRUCT WL_MESSAGE))
                     (SETF NAME (FOREIGN-STRING-ALLOC "set_anchor_rect")
                           SIGNATURE "iiii"
                           TYPES INTERFACE-ARRAY)))
                  (LET ((INTERFACE-ARRAY
                         (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID))
                                             :COUNT 1))
                        (MSG-PTR (MEM-APTR MESSAGES '(:STRUCT WL_MESSAGE) 3)))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 0) (NULL-POINTER))
                    (WITH-FOREIGN-SLOTS
                     ((NAME SIGNATURE TYPES) MSG-PTR (:STRUCT WL_MESSAGE))
                     (SETF NAME (FOREIGN-STRING-ALLOC "set_anchor")
                           SIGNATURE "u"
                           TYPES INTERFACE-ARRAY)))
                  (LET ((INTERFACE-ARRAY
                         (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID))
                                             :COUNT 1))
                        (MSG-PTR (MEM-APTR MESSAGES '(:STRUCT WL_MESSAGE) 4)))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 0) (NULL-POINTER))
                    (WITH-FOREIGN-SLOTS
                     ((NAME SIGNATURE TYPES) MSG-PTR (:STRUCT WL_MESSAGE))
                     (SETF NAME (FOREIGN-STRING-ALLOC "set_gravity")
                           SIGNATURE "u"
                           TYPES INTERFACE-ARRAY)))
                  (LET ((INTERFACE-ARRAY
                         (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID))
                                             :COUNT 1))
                        (MSG-PTR (MEM-APTR MESSAGES '(:STRUCT WL_MESSAGE) 5)))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 0) (NULL-POINTER))
                    (WITH-FOREIGN-SLOTS
                     ((NAME SIGNATURE TYPES) MSG-PTR (:STRUCT WL_MESSAGE))
                     (SETF NAME
                             (FOREIGN-STRING-ALLOC "set_constraint_adjustment")
                           SIGNATURE "u"
                           TYPES INTERFACE-ARRAY)))
                  (LET ((INTERFACE-ARRAY
                         (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID))
                                             :COUNT 2))
                        (MSG-PTR (MEM-APTR MESSAGES '(:STRUCT WL_MESSAGE) 6)))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 0) (NULL-POINTER))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 1) (NULL-POINTER))
                    (WITH-FOREIGN-SLOTS
                     ((NAME SIGNATURE TYPES) MSG-PTR (:STRUCT WL_MESSAGE))
                     (SETF NAME (FOREIGN-STRING-ALLOC "set_offset")
                           SIGNATURE "ii"
                           TYPES INTERFACE-ARRAY)))
                  (LET ((INTERFACE-ARRAY
                         (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID))
                                             :COUNT 0))
                        (MSG-PTR (MEM-APTR MESSAGES '(:STRUCT WL_MESSAGE) 7)))
                    (WITH-FOREIGN-SLOTS
                     ((NAME SIGNATURE TYPES) MSG-PTR (:STRUCT WL_MESSAGE))
                     (SETF NAME (FOREIGN-STRING-ALLOC "set_reactive")
                           SIGNATURE "3"
                           TYPES INTERFACE-ARRAY)))
                  (LET ((INTERFACE-ARRAY
                         (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID))
                                             :COUNT 2))
                        (MSG-PTR (MEM-APTR MESSAGES '(:STRUCT WL_MESSAGE) 8)))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 0) (NULL-POINTER))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 1) (NULL-POINTER))
                    (WITH-FOREIGN-SLOTS
                     ((NAME SIGNATURE TYPES) MSG-PTR (:STRUCT WL_MESSAGE))
                     (SETF NAME (FOREIGN-STRING-ALLOC "set_parent_size")
                           SIGNATURE "3ii"
                           TYPES INTERFACE-ARRAY)))
                  (LET ((INTERFACE-ARRAY
                         (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID))
                                             :COUNT 1))
                        (MSG-PTR (MEM-APTR MESSAGES '(:STRUCT WL_MESSAGE) 9)))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 0) (NULL-POINTER))
                    (WITH-FOREIGN-SLOTS
                     ((NAME SIGNATURE TYPES) MSG-PTR (:STRUCT WL_MESSAGE))
                     (SETF NAME (FOREIGN-STRING-ALLOC "set_parent_configure")
                           SIGNATURE "3u"
                           TYPES INTERFACE-ARRAY)))
                  MESSAGES))
               (EVENTS-PTR
                (LET ((MESSAGES
                       (CFFI:FOREIGN-ALLOC '(:STRUCT WL_MESSAGE) :COUNT 0)))
                  MESSAGES)))
           (DEBUG-LOG! "IF ptr range: ~a --- ~a~%" *INTERFACE*
            (MEM-APTR *INTERFACE* '(:STRUCT INTERFACE) 1))
           (WITH-FOREIGN-SLOTS
            ((NAME VERSION METHOD_COUNT METHODS EVENT_COUNT EVENTS) *INTERFACE*
             (:STRUCT INTERFACE))
            (SETF NAME (FOREIGN-STRING-ALLOC "xdg_positioner")
                  VERSION 6
                  METHOD_COUNT 10
                  METHODS REQUESTS-PTR
                  EVENT_COUNT 0
                  EVENTS EVENTS-PTR)))))
 BM-CL-WAYLAND::*INTERFACE-INIT-LIST* :TEST #'INTERFACE-EXISTS-TEST)

(CL-ASYNC-UTIL:DEFINE-C-CALLBACK DISPATCHER-FFI
    :INT
    ((DATA :POINTER) (TARGET :POINTER) (OPCODE :UINT) (MESSAGE :POINTER)
     (ARGS :POINTER))
  (DECLARE (IGNORE DATA MESSAGE))
  (DEBUG-LOG! "Dispatcher invoked: ~a~%" "xdg_positioner")
  (LET ((RESOURCE (GETHASH (POINTER-ADDRESS TARGET) *RESOURCE-TRACKER*)))
    (ECASE OPCODE
      (0 (FUNCALL 'DESTROY RESOURCE))
      (1
       (FUNCALL 'SET-SIZE RESOURCE
                (VALUES
                 (FOREIGN-SLOT-VALUE
                  (MEM-APTR ARGS '(:UNION BM-CL-LIBWAYLAND:WL_ARGUMENT) 0)
                  '(:UNION BM-CL-LIBWAYLAND:WL_ARGUMENT)
                  'WL-FFI::I))
                (VALUES
                 (FOREIGN-SLOT-VALUE
                  (MEM-APTR ARGS '(:UNION BM-CL-LIBWAYLAND:WL_ARGUMENT) 1)
                  '(:UNION BM-CL-LIBWAYLAND:WL_ARGUMENT)
                  'WL-FFI::I))))
      (2
       (FUNCALL 'SET-ANCHOR-RECT RESOURCE
                (VALUES
                 (FOREIGN-SLOT-VALUE
                  (MEM-APTR ARGS '(:UNION BM-CL-LIBWAYLAND:WL_ARGUMENT) 0)
                  '(:UNION BM-CL-LIBWAYLAND:WL_ARGUMENT)
                  'WL-FFI::I))
                (VALUES
                 (FOREIGN-SLOT-VALUE
                  (MEM-APTR ARGS '(:UNION BM-CL-LIBWAYLAND:WL_ARGUMENT) 1)
                  '(:UNION BM-CL-LIBWAYLAND:WL_ARGUMENT)
                  'WL-FFI::I))
                (VALUES
                 (FOREIGN-SLOT-VALUE
                  (MEM-APTR ARGS '(:UNION BM-CL-LIBWAYLAND:WL_ARGUMENT) 2)
                  '(:UNION BM-CL-LIBWAYLAND:WL_ARGUMENT)
                  'WL-FFI::I))
                (VALUES
                 (FOREIGN-SLOT-VALUE
                  (MEM-APTR ARGS '(:UNION BM-CL-LIBWAYLAND:WL_ARGUMENT) 3)
                  '(:UNION BM-CL-LIBWAYLAND:WL_ARGUMENT)
                  'WL-FFI::I))))
      (3
       (FUNCALL 'SET-ANCHOR RESOURCE
                (ERROR
                 "WL C enum not yet implemented. You wanted to create a lisp list with keywords")))
      (4
       (FUNCALL 'SET-GRAVITY RESOURCE
                (ERROR
                 "WL C enum not yet implemented. You wanted to create a lisp list with keywords")))
      (5
       (FUNCALL 'SET-CONSTRAINT-ADJUSTMENT RESOURCE
                (ERROR
                 "WL C enum not yet implemented. You wanted to create a lisp list with keywords")))
      (6
       (FUNCALL 'SET-OFFSET RESOURCE
                (VALUES
                 (FOREIGN-SLOT-VALUE
                  (MEM-APTR ARGS '(:UNION BM-CL-LIBWAYLAND:WL_ARGUMENT) 0)
                  '(:UNION BM-CL-LIBWAYLAND:WL_ARGUMENT)
                  'WL-FFI::I))
                (VALUES
                 (FOREIGN-SLOT-VALUE
                  (MEM-APTR ARGS '(:UNION BM-CL-LIBWAYLAND:WL_ARGUMENT) 1)
                  '(:UNION BM-CL-LIBWAYLAND:WL_ARGUMENT)
                  'WL-FFI::I))))
      (7 (FUNCALL 'SET-REACTIVE RESOURCE))
      (8
       (FUNCALL 'SET-PARENT-SIZE RESOURCE
                (VALUES
                 (FOREIGN-SLOT-VALUE
                  (MEM-APTR ARGS '(:UNION BM-CL-LIBWAYLAND:WL_ARGUMENT) 0)
                  '(:UNION BM-CL-LIBWAYLAND:WL_ARGUMENT)
                  'WL-FFI::I))
                (VALUES
                 (FOREIGN-SLOT-VALUE
                  (MEM-APTR ARGS '(:UNION BM-CL-LIBWAYLAND:WL_ARGUMENT) 1)
                  '(:UNION BM-CL-LIBWAYLAND:WL_ARGUMENT)
                  'WL-FFI::I))))
      (9
       (FUNCALL 'SET-PARENT-CONFIGURE RESOURCE
                (VALUES
                 (FOREIGN-SLOT-VALUE
                  (MEM-APTR ARGS '(:UNION BM-CL-LIBWAYLAND:WL_ARGUMENT) 0)
                  '(:UNION BM-CL-LIBWAYLAND:WL_ARGUMENT)
                  'WL-FFI::U))))))
  0)

(DEFVAR *DISPATCHER* (CALLBACK DISPATCHER-FFI))

(DEFMETHOD INITIALIZE-INSTANCE :AFTER ((INSTANCE DISPATCH) &KEY)
  (LET* ((RESOURCE
          (CREATE-RESOURCE (PTR (CLIENT INSTANCE)) *INTERFACE*
           (VERSION INSTANCE) (ID INSTANCE))))
    (SETF (GETHASH (POINTER-ADDRESS RESOURCE) *RESOURCE-TRACKER*) INSTANCE)
    (SETF (PTR INSTANCE) RESOURCE)
    (RESOURCE-SET-DISPATCHER RESOURCE *DISPATCHER* (NULL-POINTER)
     (NULL-POINTER) (NULL-POINTER))))

(DEFCLASS GLOBAL (BM-CL-WAYLAND::GLOBAL) NIL
          (:DEFAULT-INITARGS :VERSION 6 :DISPATCH-IMPL 'DISPATCH)
          (:DOCUMENTATION "child surface positioner

The xdg_positioner provides a collection of rules for the placement of a
      child surface relative to a parent surface. Rules can be defined to ensure
      the child surface remains within the visible area's borders, and to
      specify how the child surface changes its position, such as sliding along
      an axis, or flipping around a rectangle. These positioner-created rules are
      constrained by the requirement that a child surface must intersect with or
      be at least partially adjacent to its parent surface.

      See the various requests for details about possible rules.

      At the time of the request, the compositor makes a copy of the rules
      specified by the xdg_positioner. Thus, after the request is complete the
      xdg_positioner object can be destroyed or reused; further changes to the
      object will have no effect on previous usages.

      For an xdg_positioner object to be considered complete, it must have a
      non-zero size set by set_size, and a non-zero anchor rectangle set by
      set_anchor_rect. Passing an incomplete xdg_positioner object when
      positioning a surface raises an invalid_positioner error.
"))

(DEFMETHOD DISPATCH-BIND ((GLOBAL GLOBAL) CLIENT DATA VERSION ID)
  "Default bind implementation for the xdg_positioner global object.
This can be overriden by inheritance in case if custom behaviour is required."
  (DEBUG-LOG! "Binding ~a~%" "xdg_positioner")
  (LET ((BOUND
         (MAKE-INSTANCE (DISPATCH-IMPL GLOBAL) :DISPLAY (DISPLAY CLIENT)
                        :CLIENT CLIENT :ID ID)))
    (SETF (IFACE CLIENT ID) BOUND)))

(CL-ASYNC-UTIL:DEFINE-C-CALLBACK DISPATCH-BIND-FFI
    :VOID
    ((CLIENT :POINTER) (DATA :POINTER) (VERSION :UINT) (ID :UINT))
  (DEBUG-LOG! "C-Binding ~a~%" "xdg_positioner")
  (LET* ((CLIENT (GET-CLIENT CLIENT)) (GLOBAL (GET-DATA DATA)))
    (FUNCALL 'DISPATCH-BIND GLOBAL CLIENT (NULL-POINTER) VERSION ID)))

(DEFVAR *DISPATCH-BIND* (CALLBACK DISPATCH-BIND-FFI))

(DEFMETHOD INITIALIZE-INSTANCE :AFTER ((GLOBAL GLOBAL) &KEY)
  (DEBUG-LOG! "Initializing global object: ~a~%" "xdg_positioner")
  (LET* ((NEXT-DATA-ID (RESERVE-DATA))
         (GLOBAL-PTR
          (GLOBAL-CREATE (DISPLAY GLOBAL) *INTERFACE* (VERSION GLOBAL)
           (DATA-PTR NEXT-DATA-ID) *DISPATCH-BIND*)))
    (SETF (BM-CL-WAYLAND:PTR GLOBAL) GLOBAL-PTR)
    (SET-DATA NEXT-DATA-ID
     (SETF (GETHASH (POINTER-ADDRESS GLOBAL-PTR) *GLOBAL-TRACKER*) GLOBAL))))

(IN-PACKAGE :XDG_SURFACE)

(DEFCLASS DISPATCH (BM-CL-WAYLAND:OBJECT) NIL (:DEFAULT-INITARGS :VERSION 6)
          (:DOCUMENTATION "desktop user interface surface base interface

An interface that may be implemented by a wl_surface, for
      implementations that provide a desktop-style user interface.

      It provides a base set of functionality required to construct user
      interface elements requiring management by the compositor, such as
      toplevel windows, menus, etc. The types of functionality are split into
      xdg_surface roles.

      Creating an xdg_surface does not set the role for a wl_surface. In order
      to map an xdg_surface, the client must create a role-specific object
      using, e.g., get_toplevel, get_popup. The wl_surface for any given
      xdg_surface can have at most one role, and may not be assigned any role
      not based on xdg_surface.

      A role must be assigned before any other requests are made to the
      xdg_surface object.

      The client must call wl_surface.commit on the corresponding wl_surface
      for the xdg_surface state to take effect.

      Creating an xdg_surface from a wl_surface which has a buffer attached or
      committed is a client error, and any attempts by a client to attach or
      manipulate a buffer prior to the first xdg_surface.configure call must
      also be treated as errors.

      After creating a role-specific object and setting it up, the client must
      perform an initial commit without any buffer attached. The compositor
      will reply with initial wl_surface state such as
      wl_surface.preferred_buffer_scale followed by an xdg_surface.configure
      event. The client must acknowledge it and is then allowed to attach a
      buffer to map the surface.

      Mapping an xdg_surface-based role surface is defined as making it
      possible for the surface to be shown by the compositor. Note that
      a mapped surface is not guaranteed to be visible once it is mapped.

      For an xdg_surface to be mapped by the compositor, the following
      conditions must be met:
      (1) the client has assigned an xdg_surface-based role to the surface
      (2) the client has set and committed the xdg_surface state and the
	  role-dependent state to the surface
      (3) the client has committed a buffer to the surface

      A newly-unmapped surface is considered to have met condition (1) out
      of the 3 required conditions for mapping a surface if its role surface
      has not been destroyed, i.e. the client must perform the initial commit
      again before attaching a buffer.
"))

(DEFGENERIC DESTROY
    (RESOURCE))

(DEFGENERIC GET-TOPLEVEL
    (RESOURCE ID))

(DEFGENERIC GET-POPUP
    (RESOURCE ID PARENT POSITIONER))

(DEFGENERIC SET-WINDOW-GEOMETRY
    (RESOURCE X Y WIDTH HEIGHT))

(DEFGENERIC ACK-CONFIGURE
    (RESOURCE SERIAL))

(PUSHNEW
 (LIST "xdg_surface"
       (LIST "xdg_toplevel" "xdg_popup" "xdg_surface" "xdg_positioner")
       (LAMBDA ()
         (SETF *INTERFACE* (FOREIGN-ALLOC '(:STRUCT INTERFACE)))
         (LET ((REQUESTS-PTR
                (LET ((MESSAGES
                       (CFFI:FOREIGN-ALLOC '(:STRUCT WL_MESSAGE) :COUNT 5)))
                  (LET ((INTERFACE-ARRAY
                         (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID))
                                             :COUNT 0))
                        (MSG-PTR (MEM-APTR MESSAGES '(:STRUCT WL_MESSAGE) 0)))
                    (WITH-FOREIGN-SLOTS
                     ((NAME SIGNATURE TYPES) MSG-PTR (:STRUCT WL_MESSAGE))
                     (SETF NAME (FOREIGN-STRING-ALLOC "destroy")
                           SIGNATURE ""
                           TYPES INTERFACE-ARRAY)))
                  (LET ((INTERFACE-ARRAY
                         (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID))
                                             :COUNT 1))
                        (MSG-PTR (MEM-APTR MESSAGES '(:STRUCT WL_MESSAGE) 1)))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 0)
                            XDG_TOPLEVEL::*INTERFACE*)
                    (WITH-FOREIGN-SLOTS
                     ((NAME SIGNATURE TYPES) MSG-PTR (:STRUCT WL_MESSAGE))
                     (SETF NAME (FOREIGN-STRING-ALLOC "get_toplevel")
                           SIGNATURE "n"
                           TYPES INTERFACE-ARRAY)))
                  (LET ((INTERFACE-ARRAY
                         (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID))
                                             :COUNT 3))
                        (MSG-PTR (MEM-APTR MESSAGES '(:STRUCT WL_MESSAGE) 2)))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 0)
                            XDG_POPUP::*INTERFACE*)
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 1)
                            XDG_SURFACE::*INTERFACE*)
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 2)
                            XDG_POSITIONER::*INTERFACE*)
                    (WITH-FOREIGN-SLOTS
                     ((NAME SIGNATURE TYPES) MSG-PTR (:STRUCT WL_MESSAGE))
                     (SETF NAME (FOREIGN-STRING-ALLOC "get_popup")
                           SIGNATURE "n?oo"
                           TYPES INTERFACE-ARRAY)))
                  (LET ((INTERFACE-ARRAY
                         (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID))
                                             :COUNT 4))
                        (MSG-PTR (MEM-APTR MESSAGES '(:STRUCT WL_MESSAGE) 3)))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 0) (NULL-POINTER))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 1) (NULL-POINTER))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 2) (NULL-POINTER))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 3) (NULL-POINTER))
                    (WITH-FOREIGN-SLOTS
                     ((NAME SIGNATURE TYPES) MSG-PTR (:STRUCT WL_MESSAGE))
                     (SETF NAME (FOREIGN-STRING-ALLOC "set_window_geometry")
                           SIGNATURE "iiii"
                           TYPES INTERFACE-ARRAY)))
                  (LET ((INTERFACE-ARRAY
                         (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID))
                                             :COUNT 1))
                        (MSG-PTR (MEM-APTR MESSAGES '(:STRUCT WL_MESSAGE) 4)))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 0) (NULL-POINTER))
                    (WITH-FOREIGN-SLOTS
                     ((NAME SIGNATURE TYPES) MSG-PTR (:STRUCT WL_MESSAGE))
                     (SETF NAME (FOREIGN-STRING-ALLOC "ack_configure")
                           SIGNATURE "u"
                           TYPES INTERFACE-ARRAY)))
                  MESSAGES))
               (EVENTS-PTR
                (LET ((MESSAGES
                       (CFFI:FOREIGN-ALLOC '(:STRUCT WL_MESSAGE) :COUNT 1)))
                  (LET ((INTERFACE-ARRAY
                         (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID))
                                             :COUNT 1))
                        (MSG-PTR (MEM-APTR MESSAGES '(:STRUCT WL_MESSAGE) 0)))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 0) (NULL-POINTER))
                    (WITH-FOREIGN-SLOTS
                     ((NAME SIGNATURE TYPES) MSG-PTR (:STRUCT WL_MESSAGE))
                     (SETF NAME (FOREIGN-STRING-ALLOC "configure")
                           SIGNATURE "u"
                           TYPES INTERFACE-ARRAY)))
                  MESSAGES)))
           (DEBUG-LOG! "IF ptr range: ~a --- ~a~%" *INTERFACE*
            (MEM-APTR *INTERFACE* '(:STRUCT INTERFACE) 1))
           (WITH-FOREIGN-SLOTS
            ((NAME VERSION METHOD_COUNT METHODS EVENT_COUNT EVENTS) *INTERFACE*
             (:STRUCT INTERFACE))
            (SETF NAME (FOREIGN-STRING-ALLOC "xdg_surface")
                  VERSION 6
                  METHOD_COUNT 5
                  METHODS REQUESTS-PTR
                  EVENT_COUNT 1
                  EVENTS EVENTS-PTR)))))
 BM-CL-WAYLAND::*INTERFACE-INIT-LIST* :TEST #'INTERFACE-EXISTS-TEST)

(CL-ASYNC-UTIL:DEFINE-C-CALLBACK DISPATCHER-FFI
    :INT
    ((DATA :POINTER) (TARGET :POINTER) (OPCODE :UINT) (MESSAGE :POINTER)
     (ARGS :POINTER))
  (DECLARE (IGNORE DATA MESSAGE))
  (DEBUG-LOG! "Dispatcher invoked: ~a~%" "xdg_surface")
  (LET ((RESOURCE (GETHASH (POINTER-ADDRESS TARGET) *RESOURCE-TRACKER*)))
    (ECASE OPCODE
      (0 (FUNCALL 'DESTROY RESOURCE))
      (1
       (FUNCALL 'GET-TOPLEVEL RESOURCE
                (VALUES
                 (FOREIGN-SLOT-VALUE
                  (MEM-APTR ARGS '(:UNION BM-CL-LIBWAYLAND:WL_ARGUMENT) 0)
                  '(:UNION BM-CL-LIBWAYLAND:WL_ARGUMENT)
                  'WL-FFI::N))))
      (2
       (FUNCALL 'GET-POPUP RESOURCE
                (VALUES
                 (FOREIGN-SLOT-VALUE
                  (MEM-APTR ARGS '(:UNION BM-CL-LIBWAYLAND:WL_ARGUMENT) 0)
                  '(:UNION BM-CL-LIBWAYLAND:WL_ARGUMENT)
                  'WL-FFI::N))
                (GETHASH
                 (POINTER-ADDRESS
                  (FOREIGN-SLOT-VALUE
                   (MEM-APTR ARGS '(:UNION BM-CL-LIBWAYLAND:WL_ARGUMENT) 1)
                   '(:UNION BM-CL-LIBWAYLAND:WL_ARGUMENT)
                   'WL-FFI::O))
                 *RESOURCE-TRACKER*)
                (GETHASH
                 (POINTER-ADDRESS
                  (FOREIGN-SLOT-VALUE
                   (MEM-APTR ARGS '(:UNION BM-CL-LIBWAYLAND:WL_ARGUMENT) 2)
                   '(:UNION BM-CL-LIBWAYLAND:WL_ARGUMENT)
                   'WL-FFI::O))
                 *RESOURCE-TRACKER*)))
      (3
       (FUNCALL 'SET-WINDOW-GEOMETRY RESOURCE
                (VALUES
                 (FOREIGN-SLOT-VALUE
                  (MEM-APTR ARGS '(:UNION BM-CL-LIBWAYLAND:WL_ARGUMENT) 0)
                  '(:UNION BM-CL-LIBWAYLAND:WL_ARGUMENT)
                  'WL-FFI::I))
                (VALUES
                 (FOREIGN-SLOT-VALUE
                  (MEM-APTR ARGS '(:UNION BM-CL-LIBWAYLAND:WL_ARGUMENT) 1)
                  '(:UNION BM-CL-LIBWAYLAND:WL_ARGUMENT)
                  'WL-FFI::I))
                (VALUES
                 (FOREIGN-SLOT-VALUE
                  (MEM-APTR ARGS '(:UNION BM-CL-LIBWAYLAND:WL_ARGUMENT) 2)
                  '(:UNION BM-CL-LIBWAYLAND:WL_ARGUMENT)
                  'WL-FFI::I))
                (VALUES
                 (FOREIGN-SLOT-VALUE
                  (MEM-APTR ARGS '(:UNION BM-CL-LIBWAYLAND:WL_ARGUMENT) 3)
                  '(:UNION BM-CL-LIBWAYLAND:WL_ARGUMENT)
                  'WL-FFI::I))))
      (4
       (FUNCALL 'ACK-CONFIGURE RESOURCE
                (VALUES
                 (FOREIGN-SLOT-VALUE
                  (MEM-APTR ARGS '(:UNION BM-CL-LIBWAYLAND:WL_ARGUMENT) 0)
                  '(:UNION BM-CL-LIBWAYLAND:WL_ARGUMENT)
                  'WL-FFI::U))))))
  0)

(DEFVAR *DISPATCHER* (CALLBACK DISPATCHER-FFI))

(DEFMETHOD INITIALIZE-INSTANCE :AFTER ((INSTANCE DISPATCH) &KEY)
  (LET* ((RESOURCE
          (CREATE-RESOURCE (PTR (CLIENT INSTANCE)) *INTERFACE*
           (VERSION INSTANCE) (ID INSTANCE))))
    (SETF (GETHASH (POINTER-ADDRESS RESOURCE) *RESOURCE-TRACKER*) INSTANCE)
    (SETF (PTR INSTANCE) RESOURCE)
    (RESOURCE-SET-DISPATCHER RESOURCE *DISPATCHER* (NULL-POINTER)
     (NULL-POINTER) (NULL-POINTER))))

(DEFMETHOD SEND-CONFIGURE ((DISPATCH DISPATCH) SERIAL)
  (DEBUG-LOG! "Event: ~a~%" "configure")
  (LET ((ARG-LIST (FOREIGN-ALLOC '(:UNION WL_ARGUMENT) :COUNT 1)))
    (SETF (FOREIGN-SLOT-VALUE
           (MEM-AREF ARG-LIST '(:UNION BM-CL-LIBWAYLAND:WL_ARGUMENT) 0)
           '(:UNION BM-CL-LIBWAYLAND:WL_ARGUMENT) 'WL-FFI::U)
            SERIAL)
    (RESOURCE-POST-EVENT-ARRAY (PTR DISPATCH) 0 ARG-LIST)))

(DEFCLASS GLOBAL (BM-CL-WAYLAND::GLOBAL) NIL
          (:DEFAULT-INITARGS :VERSION 6 :DISPATCH-IMPL 'DISPATCH)
          (:DOCUMENTATION "desktop user interface surface base interface

An interface that may be implemented by a wl_surface, for
      implementations that provide a desktop-style user interface.

      It provides a base set of functionality required to construct user
      interface elements requiring management by the compositor, such as
      toplevel windows, menus, etc. The types of functionality are split into
      xdg_surface roles.

      Creating an xdg_surface does not set the role for a wl_surface. In order
      to map an xdg_surface, the client must create a role-specific object
      using, e.g., get_toplevel, get_popup. The wl_surface for any given
      xdg_surface can have at most one role, and may not be assigned any role
      not based on xdg_surface.

      A role must be assigned before any other requests are made to the
      xdg_surface object.

      The client must call wl_surface.commit on the corresponding wl_surface
      for the xdg_surface state to take effect.

      Creating an xdg_surface from a wl_surface which has a buffer attached or
      committed is a client error, and any attempts by a client to attach or
      manipulate a buffer prior to the first xdg_surface.configure call must
      also be treated as errors.

      After creating a role-specific object and setting it up, the client must
      perform an initial commit without any buffer attached. The compositor
      will reply with initial wl_surface state such as
      wl_surface.preferred_buffer_scale followed by an xdg_surface.configure
      event. The client must acknowledge it and is then allowed to attach a
      buffer to map the surface.

      Mapping an xdg_surface-based role surface is defined as making it
      possible for the surface to be shown by the compositor. Note that
      a mapped surface is not guaranteed to be visible once it is mapped.

      For an xdg_surface to be mapped by the compositor, the following
      conditions must be met:
      (1) the client has assigned an xdg_surface-based role to the surface
      (2) the client has set and committed the xdg_surface state and the
	  role-dependent state to the surface
      (3) the client has committed a buffer to the surface

      A newly-unmapped surface is considered to have met condition (1) out
      of the 3 required conditions for mapping a surface if its role surface
      has not been destroyed, i.e. the client must perform the initial commit
      again before attaching a buffer.
"))

(DEFMETHOD DISPATCH-BIND ((GLOBAL GLOBAL) CLIENT DATA VERSION ID)
  "Default bind implementation for the xdg_surface global object.
This can be overriden by inheritance in case if custom behaviour is required."
  (DEBUG-LOG! "Binding ~a~%" "xdg_surface")
  (LET ((BOUND
         (MAKE-INSTANCE (DISPATCH-IMPL GLOBAL) :DISPLAY (DISPLAY CLIENT)
                        :CLIENT CLIENT :ID ID)))
    (SETF (IFACE CLIENT ID) BOUND)))

(CL-ASYNC-UTIL:DEFINE-C-CALLBACK DISPATCH-BIND-FFI
    :VOID
    ((CLIENT :POINTER) (DATA :POINTER) (VERSION :UINT) (ID :UINT))
  (DEBUG-LOG! "C-Binding ~a~%" "xdg_surface")
  (LET* ((CLIENT (GET-CLIENT CLIENT)) (GLOBAL (GET-DATA DATA)))
    (FUNCALL 'DISPATCH-BIND GLOBAL CLIENT (NULL-POINTER) VERSION ID)))

(DEFVAR *DISPATCH-BIND* (CALLBACK DISPATCH-BIND-FFI))

(DEFMETHOD INITIALIZE-INSTANCE :AFTER ((GLOBAL GLOBAL) &KEY)
  (DEBUG-LOG! "Initializing global object: ~a~%" "xdg_surface")
  (LET* ((NEXT-DATA-ID (RESERVE-DATA))
         (GLOBAL-PTR
          (GLOBAL-CREATE (DISPLAY GLOBAL) *INTERFACE* (VERSION GLOBAL)
           (DATA-PTR NEXT-DATA-ID) *DISPATCH-BIND*)))
    (SETF (BM-CL-WAYLAND:PTR GLOBAL) GLOBAL-PTR)
    (SET-DATA NEXT-DATA-ID
     (SETF (GETHASH (POINTER-ADDRESS GLOBAL-PTR) *GLOBAL-TRACKER*) GLOBAL))))

(IN-PACKAGE :XDG_TOPLEVEL)

(DEFCLASS DISPATCH (BM-CL-WAYLAND:OBJECT) NIL (:DEFAULT-INITARGS :VERSION 6)
          (:DOCUMENTATION "toplevel surface

This interface defines an xdg_surface role which allows a surface to,
      among other things, set window-like properties such as maximize,
      fullscreen, and minimize, set application-specific metadata like title and
      id, and well as trigger user interactive operations such as interactive
      resize and move.

      A xdg_toplevel by default is responsible for providing the full intended
      visual representation of the toplevel, which depending on the window
      state, may mean things like a title bar, window controls and drop shadow.

      Unmapping an xdg_toplevel means that the surface cannot be shown
      by the compositor until it is explicitly mapped again.
      All active operations (e.g., move, resize) are canceled and all
      attributes (e.g. title, state, stacking, ...) are discarded for
      an xdg_toplevel surface when it is unmapped. The xdg_toplevel returns to
      the state it had right after xdg_surface.get_toplevel. The client
      can re-map the toplevel by perfoming a commit without any buffer
      attached, waiting for a configure event and handling it as usual (see
      xdg_surface description).

      Attaching a null buffer to a toplevel unmaps the surface.
"))

(DEFGENERIC DESTROY
    (RESOURCE))

(DEFGENERIC SET-PARENT
    (RESOURCE PARENT))

(DEFGENERIC SET-TITLE
    (RESOURCE TITLE))

(DEFGENERIC SET-APP-ID
    (RESOURCE APP_ID))

(DEFGENERIC SHOW-WINDOW-MENU
    (RESOURCE SEAT SERIAL X Y))

(DEFGENERIC MOVE
    (RESOURCE SEAT SERIAL))

(DEFGENERIC RESIZE
    (RESOURCE SEAT SERIAL EDGES))

(DEFGENERIC SET-MAX-SIZE
    (RESOURCE WIDTH HEIGHT))

(DEFGENERIC SET-MIN-SIZE
    (RESOURCE WIDTH HEIGHT))

(DEFGENERIC SET-MAXIMIZED
    (RESOURCE))

(DEFGENERIC UNSET-MAXIMIZED
    (RESOURCE))

(DEFGENERIC SET-FULLSCREEN
    (RESOURCE OUTPUT))

(DEFGENERIC UNSET-FULLSCREEN
    (RESOURCE))

(DEFGENERIC SET-MINIMIZED
    (RESOURCE))

(PUSHNEW
 (LIST "xdg_toplevel" (LIST "xdg_toplevel" "wl_seat" "wl_output")
       (LAMBDA ()
         (SETF *INTERFACE* (FOREIGN-ALLOC '(:STRUCT INTERFACE)))
         (LET ((REQUESTS-PTR
                (LET ((MESSAGES
                       (CFFI:FOREIGN-ALLOC '(:STRUCT WL_MESSAGE) :COUNT 14)))
                  (LET ((INTERFACE-ARRAY
                         (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID))
                                             :COUNT 0))
                        (MSG-PTR (MEM-APTR MESSAGES '(:STRUCT WL_MESSAGE) 0)))
                    (WITH-FOREIGN-SLOTS
                     ((NAME SIGNATURE TYPES) MSG-PTR (:STRUCT WL_MESSAGE))
                     (SETF NAME (FOREIGN-STRING-ALLOC "destroy")
                           SIGNATURE ""
                           TYPES INTERFACE-ARRAY)))
                  (LET ((INTERFACE-ARRAY
                         (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID))
                                             :COUNT 1))
                        (MSG-PTR (MEM-APTR MESSAGES '(:STRUCT WL_MESSAGE) 1)))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 0)
                            XDG_TOPLEVEL::*INTERFACE*)
                    (WITH-FOREIGN-SLOTS
                     ((NAME SIGNATURE TYPES) MSG-PTR (:STRUCT WL_MESSAGE))
                     (SETF NAME (FOREIGN-STRING-ALLOC "set_parent")
                           SIGNATURE "?o"
                           TYPES INTERFACE-ARRAY)))
                  (LET ((INTERFACE-ARRAY
                         (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID))
                                             :COUNT 1))
                        (MSG-PTR (MEM-APTR MESSAGES '(:STRUCT WL_MESSAGE) 2)))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 0) (NULL-POINTER))
                    (WITH-FOREIGN-SLOTS
                     ((NAME SIGNATURE TYPES) MSG-PTR (:STRUCT WL_MESSAGE))
                     (SETF NAME (FOREIGN-STRING-ALLOC "set_title")
                           SIGNATURE "s"
                           TYPES INTERFACE-ARRAY)))
                  (LET ((INTERFACE-ARRAY
                         (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID))
                                             :COUNT 1))
                        (MSG-PTR (MEM-APTR MESSAGES '(:STRUCT WL_MESSAGE) 3)))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 0) (NULL-POINTER))
                    (WITH-FOREIGN-SLOTS
                     ((NAME SIGNATURE TYPES) MSG-PTR (:STRUCT WL_MESSAGE))
                     (SETF NAME (FOREIGN-STRING-ALLOC "set_app_id")
                           SIGNATURE "s"
                           TYPES INTERFACE-ARRAY)))
                  (LET ((INTERFACE-ARRAY
                         (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID))
                                             :COUNT 4))
                        (MSG-PTR (MEM-APTR MESSAGES '(:STRUCT WL_MESSAGE) 4)))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 0)
                            WL_SEAT::*INTERFACE*)
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 1) (NULL-POINTER))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 2) (NULL-POINTER))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 3) (NULL-POINTER))
                    (WITH-FOREIGN-SLOTS
                     ((NAME SIGNATURE TYPES) MSG-PTR (:STRUCT WL_MESSAGE))
                     (SETF NAME (FOREIGN-STRING-ALLOC "show_window_menu")
                           SIGNATURE "ouii"
                           TYPES INTERFACE-ARRAY)))
                  (LET ((INTERFACE-ARRAY
                         (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID))
                                             :COUNT 2))
                        (MSG-PTR (MEM-APTR MESSAGES '(:STRUCT WL_MESSAGE) 5)))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 0)
                            WL_SEAT::*INTERFACE*)
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 1) (NULL-POINTER))
                    (WITH-FOREIGN-SLOTS
                     ((NAME SIGNATURE TYPES) MSG-PTR (:STRUCT WL_MESSAGE))
                     (SETF NAME (FOREIGN-STRING-ALLOC "move")
                           SIGNATURE "ou"
                           TYPES INTERFACE-ARRAY)))
                  (LET ((INTERFACE-ARRAY
                         (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID))
                                             :COUNT 3))
                        (MSG-PTR (MEM-APTR MESSAGES '(:STRUCT WL_MESSAGE) 6)))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 0)
                            WL_SEAT::*INTERFACE*)
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 1) (NULL-POINTER))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 2) (NULL-POINTER))
                    (WITH-FOREIGN-SLOTS
                     ((NAME SIGNATURE TYPES) MSG-PTR (:STRUCT WL_MESSAGE))
                     (SETF NAME (FOREIGN-STRING-ALLOC "resize")
                           SIGNATURE "ouu"
                           TYPES INTERFACE-ARRAY)))
                  (LET ((INTERFACE-ARRAY
                         (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID))
                                             :COUNT 2))
                        (MSG-PTR (MEM-APTR MESSAGES '(:STRUCT WL_MESSAGE) 7)))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 0) (NULL-POINTER))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 1) (NULL-POINTER))
                    (WITH-FOREIGN-SLOTS
                     ((NAME SIGNATURE TYPES) MSG-PTR (:STRUCT WL_MESSAGE))
                     (SETF NAME (FOREIGN-STRING-ALLOC "set_max_size")
                           SIGNATURE "ii"
                           TYPES INTERFACE-ARRAY)))
                  (LET ((INTERFACE-ARRAY
                         (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID))
                                             :COUNT 2))
                        (MSG-PTR (MEM-APTR MESSAGES '(:STRUCT WL_MESSAGE) 8)))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 0) (NULL-POINTER))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 1) (NULL-POINTER))
                    (WITH-FOREIGN-SLOTS
                     ((NAME SIGNATURE TYPES) MSG-PTR (:STRUCT WL_MESSAGE))
                     (SETF NAME (FOREIGN-STRING-ALLOC "set_min_size")
                           SIGNATURE "ii"
                           TYPES INTERFACE-ARRAY)))
                  (LET ((INTERFACE-ARRAY
                         (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID))
                                             :COUNT 0))
                        (MSG-PTR (MEM-APTR MESSAGES '(:STRUCT WL_MESSAGE) 9)))
                    (WITH-FOREIGN-SLOTS
                     ((NAME SIGNATURE TYPES) MSG-PTR (:STRUCT WL_MESSAGE))
                     (SETF NAME (FOREIGN-STRING-ALLOC "set_maximized")
                           SIGNATURE ""
                           TYPES INTERFACE-ARRAY)))
                  (LET ((INTERFACE-ARRAY
                         (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID))
                                             :COUNT 0))
                        (MSG-PTR (MEM-APTR MESSAGES '(:STRUCT WL_MESSAGE) 10)))
                    (WITH-FOREIGN-SLOTS
                     ((NAME SIGNATURE TYPES) MSG-PTR (:STRUCT WL_MESSAGE))
                     (SETF NAME (FOREIGN-STRING-ALLOC "unset_maximized")
                           SIGNATURE ""
                           TYPES INTERFACE-ARRAY)))
                  (LET ((INTERFACE-ARRAY
                         (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID))
                                             :COUNT 1))
                        (MSG-PTR (MEM-APTR MESSAGES '(:STRUCT WL_MESSAGE) 11)))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 0)
                            WL_OUTPUT::*INTERFACE*)
                    (WITH-FOREIGN-SLOTS
                     ((NAME SIGNATURE TYPES) MSG-PTR (:STRUCT WL_MESSAGE))
                     (SETF NAME (FOREIGN-STRING-ALLOC "set_fullscreen")
                           SIGNATURE "?o"
                           TYPES INTERFACE-ARRAY)))
                  (LET ((INTERFACE-ARRAY
                         (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID))
                                             :COUNT 0))
                        (MSG-PTR (MEM-APTR MESSAGES '(:STRUCT WL_MESSAGE) 12)))
                    (WITH-FOREIGN-SLOTS
                     ((NAME SIGNATURE TYPES) MSG-PTR (:STRUCT WL_MESSAGE))
                     (SETF NAME (FOREIGN-STRING-ALLOC "unset_fullscreen")
                           SIGNATURE ""
                           TYPES INTERFACE-ARRAY)))
                  (LET ((INTERFACE-ARRAY
                         (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID))
                                             :COUNT 0))
                        (MSG-PTR (MEM-APTR MESSAGES '(:STRUCT WL_MESSAGE) 13)))
                    (WITH-FOREIGN-SLOTS
                     ((NAME SIGNATURE TYPES) MSG-PTR (:STRUCT WL_MESSAGE))
                     (SETF NAME (FOREIGN-STRING-ALLOC "set_minimized")
                           SIGNATURE ""
                           TYPES INTERFACE-ARRAY)))
                  MESSAGES))
               (EVENTS-PTR
                (LET ((MESSAGES
                       (CFFI:FOREIGN-ALLOC '(:STRUCT WL_MESSAGE) :COUNT 4)))
                  (LET ((INTERFACE-ARRAY
                         (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID))
                                             :COUNT 3))
                        (MSG-PTR (MEM-APTR MESSAGES '(:STRUCT WL_MESSAGE) 0)))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 0) (NULL-POINTER))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 1) (NULL-POINTER))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 2) (NULL-POINTER))
                    (WITH-FOREIGN-SLOTS
                     ((NAME SIGNATURE TYPES) MSG-PTR (:STRUCT WL_MESSAGE))
                     (SETF NAME (FOREIGN-STRING-ALLOC "configure")
                           SIGNATURE "iia"
                           TYPES INTERFACE-ARRAY)))
                  (LET ((INTERFACE-ARRAY
                         (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID))
                                             :COUNT 0))
                        (MSG-PTR (MEM-APTR MESSAGES '(:STRUCT WL_MESSAGE) 1)))
                    (WITH-FOREIGN-SLOTS
                     ((NAME SIGNATURE TYPES) MSG-PTR (:STRUCT WL_MESSAGE))
                     (SETF NAME (FOREIGN-STRING-ALLOC "close")
                           SIGNATURE ""
                           TYPES INTERFACE-ARRAY)))
                  (LET ((INTERFACE-ARRAY
                         (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID))
                                             :COUNT 2))
                        (MSG-PTR (MEM-APTR MESSAGES '(:STRUCT WL_MESSAGE) 2)))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 0) (NULL-POINTER))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 1) (NULL-POINTER))
                    (WITH-FOREIGN-SLOTS
                     ((NAME SIGNATURE TYPES) MSG-PTR (:STRUCT WL_MESSAGE))
                     (SETF NAME (FOREIGN-STRING-ALLOC "configure_bounds")
                           SIGNATURE "4ii"
                           TYPES INTERFACE-ARRAY)))
                  (LET ((INTERFACE-ARRAY
                         (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID))
                                             :COUNT 1))
                        (MSG-PTR (MEM-APTR MESSAGES '(:STRUCT WL_MESSAGE) 3)))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 0) (NULL-POINTER))
                    (WITH-FOREIGN-SLOTS
                     ((NAME SIGNATURE TYPES) MSG-PTR (:STRUCT WL_MESSAGE))
                     (SETF NAME (FOREIGN-STRING-ALLOC "wm_capabilities")
                           SIGNATURE "5a"
                           TYPES INTERFACE-ARRAY)))
                  MESSAGES)))
           (DEBUG-LOG! "IF ptr range: ~a --- ~a~%" *INTERFACE*
            (MEM-APTR *INTERFACE* '(:STRUCT INTERFACE) 1))
           (WITH-FOREIGN-SLOTS
            ((NAME VERSION METHOD_COUNT METHODS EVENT_COUNT EVENTS) *INTERFACE*
             (:STRUCT INTERFACE))
            (SETF NAME (FOREIGN-STRING-ALLOC "xdg_toplevel")
                  VERSION 6
                  METHOD_COUNT 14
                  METHODS REQUESTS-PTR
                  EVENT_COUNT 4
                  EVENTS EVENTS-PTR)))))
 BM-CL-WAYLAND::*INTERFACE-INIT-LIST* :TEST #'INTERFACE-EXISTS-TEST)

(CL-ASYNC-UTIL:DEFINE-C-CALLBACK DISPATCHER-FFI
    :INT
    ((DATA :POINTER) (TARGET :POINTER) (OPCODE :UINT) (MESSAGE :POINTER)
     (ARGS :POINTER))
  (DECLARE (IGNORE DATA MESSAGE))
  (DEBUG-LOG! "Dispatcher invoked: ~a~%" "xdg_toplevel")
  (LET ((RESOURCE (GETHASH (POINTER-ADDRESS TARGET) *RESOURCE-TRACKER*)))
    (ECASE OPCODE
      (0 (FUNCALL 'DESTROY RESOURCE))
      (1
       (FUNCALL 'SET-PARENT RESOURCE
                (GETHASH
                 (POINTER-ADDRESS
                  (FOREIGN-SLOT-VALUE
                   (MEM-APTR ARGS '(:UNION BM-CL-LIBWAYLAND:WL_ARGUMENT) 0)
                   '(:UNION BM-CL-LIBWAYLAND:WL_ARGUMENT)
                   'WL-FFI::O))
                 *RESOURCE-TRACKER*)))
      (2
       (FUNCALL 'SET-TITLE RESOURCE
                (VALUES
                 (FOREIGN-SLOT-VALUE
                  (MEM-APTR ARGS '(:UNION BM-CL-LIBWAYLAND:WL_ARGUMENT) 0)
                  '(:UNION BM-CL-LIBWAYLAND:WL_ARGUMENT)
                  'WL-FFI::S))))
      (3
       (FUNCALL 'SET-APP-ID RESOURCE
                (VALUES
                 (FOREIGN-SLOT-VALUE
                  (MEM-APTR ARGS '(:UNION BM-CL-LIBWAYLAND:WL_ARGUMENT) 0)
                  '(:UNION BM-CL-LIBWAYLAND:WL_ARGUMENT)
                  'WL-FFI::S))))
      (4
       (FUNCALL 'SHOW-WINDOW-MENU RESOURCE
                (GETHASH
                 (POINTER-ADDRESS
                  (FOREIGN-SLOT-VALUE
                   (MEM-APTR ARGS '(:UNION BM-CL-LIBWAYLAND:WL_ARGUMENT) 0)
                   '(:UNION BM-CL-LIBWAYLAND:WL_ARGUMENT)
                   'WL-FFI::O))
                 *RESOURCE-TRACKER*)
                (VALUES
                 (FOREIGN-SLOT-VALUE
                  (MEM-APTR ARGS '(:UNION BM-CL-LIBWAYLAND:WL_ARGUMENT) 1)
                  '(:UNION BM-CL-LIBWAYLAND:WL_ARGUMENT)
                  'WL-FFI::U))
                (VALUES
                 (FOREIGN-SLOT-VALUE
                  (MEM-APTR ARGS '(:UNION BM-CL-LIBWAYLAND:WL_ARGUMENT) 2)
                  '(:UNION BM-CL-LIBWAYLAND:WL_ARGUMENT)
                  'WL-FFI::I))
                (VALUES
                 (FOREIGN-SLOT-VALUE
                  (MEM-APTR ARGS '(:UNION BM-CL-LIBWAYLAND:WL_ARGUMENT) 3)
                  '(:UNION BM-CL-LIBWAYLAND:WL_ARGUMENT)
                  'WL-FFI::I))))
      (5
       (FUNCALL 'MOVE RESOURCE
                (GETHASH
                 (POINTER-ADDRESS
                  (FOREIGN-SLOT-VALUE
                   (MEM-APTR ARGS '(:UNION BM-CL-LIBWAYLAND:WL_ARGUMENT) 0)
                   '(:UNION BM-CL-LIBWAYLAND:WL_ARGUMENT)
                   'WL-FFI::O))
                 *RESOURCE-TRACKER*)
                (VALUES
                 (FOREIGN-SLOT-VALUE
                  (MEM-APTR ARGS '(:UNION BM-CL-LIBWAYLAND:WL_ARGUMENT) 1)
                  '(:UNION BM-CL-LIBWAYLAND:WL_ARGUMENT)
                  'WL-FFI::U))))
      (6
       (FUNCALL 'RESIZE RESOURCE
                (GETHASH
                 (POINTER-ADDRESS
                  (FOREIGN-SLOT-VALUE
                   (MEM-APTR ARGS '(:UNION BM-CL-LIBWAYLAND:WL_ARGUMENT) 0)
                   '(:UNION BM-CL-LIBWAYLAND:WL_ARGUMENT)
                   'WL-FFI::O))
                 *RESOURCE-TRACKER*)
                (VALUES
                 (FOREIGN-SLOT-VALUE
                  (MEM-APTR ARGS '(:UNION BM-CL-LIBWAYLAND:WL_ARGUMENT) 1)
                  '(:UNION BM-CL-LIBWAYLAND:WL_ARGUMENT)
                  'WL-FFI::U))
                (ERROR
                 "WL C enum not yet implemented. You wanted to create a lisp list with keywords")))
      (7
       (FUNCALL 'SET-MAX-SIZE RESOURCE
                (VALUES
                 (FOREIGN-SLOT-VALUE
                  (MEM-APTR ARGS '(:UNION BM-CL-LIBWAYLAND:WL_ARGUMENT) 0)
                  '(:UNION BM-CL-LIBWAYLAND:WL_ARGUMENT)
                  'WL-FFI::I))
                (VALUES
                 (FOREIGN-SLOT-VALUE
                  (MEM-APTR ARGS '(:UNION BM-CL-LIBWAYLAND:WL_ARGUMENT) 1)
                  '(:UNION BM-CL-LIBWAYLAND:WL_ARGUMENT)
                  'WL-FFI::I))))
      (8
       (FUNCALL 'SET-MIN-SIZE RESOURCE
                (VALUES
                 (FOREIGN-SLOT-VALUE
                  (MEM-APTR ARGS '(:UNION BM-CL-LIBWAYLAND:WL_ARGUMENT) 0)
                  '(:UNION BM-CL-LIBWAYLAND:WL_ARGUMENT)
                  'WL-FFI::I))
                (VALUES
                 (FOREIGN-SLOT-VALUE
                  (MEM-APTR ARGS '(:UNION BM-CL-LIBWAYLAND:WL_ARGUMENT) 1)
                  '(:UNION BM-CL-LIBWAYLAND:WL_ARGUMENT)
                  'WL-FFI::I))))
      (9 (FUNCALL 'SET-MAXIMIZED RESOURCE))
      (10 (FUNCALL 'UNSET-MAXIMIZED RESOURCE))
      (11
       (FUNCALL 'SET-FULLSCREEN RESOURCE
                (GETHASH
                 (POINTER-ADDRESS
                  (FOREIGN-SLOT-VALUE
                   (MEM-APTR ARGS '(:UNION BM-CL-LIBWAYLAND:WL_ARGUMENT) 0)
                   '(:UNION BM-CL-LIBWAYLAND:WL_ARGUMENT)
                   'WL-FFI::O))
                 *RESOURCE-TRACKER*)))
      (12 (FUNCALL 'UNSET-FULLSCREEN RESOURCE))
      (13 (FUNCALL 'SET-MINIMIZED RESOURCE))))
  0)

(DEFVAR *DISPATCHER* (CALLBACK DISPATCHER-FFI))

(DEFMETHOD INITIALIZE-INSTANCE :AFTER ((INSTANCE DISPATCH) &KEY)
  (LET* ((RESOURCE
          (CREATE-RESOURCE (PTR (CLIENT INSTANCE)) *INTERFACE*
           (VERSION INSTANCE) (ID INSTANCE))))
    (SETF (GETHASH (POINTER-ADDRESS RESOURCE) *RESOURCE-TRACKER*) INSTANCE)
    (SETF (PTR INSTANCE) RESOURCE)
    (RESOURCE-SET-DISPATCHER RESOURCE *DISPATCHER* (NULL-POINTER)
     (NULL-POINTER) (NULL-POINTER))))

(DEFMETHOD SEND-CONFIGURE
           ((DISPATCH DISPATCH) WIDTH HEIGHT
            STATES)
  (DEBUG-LOG! "Event: ~a~%" "configure")
  (LET ((ARG-LIST (FOREIGN-ALLOC '(:UNION WL_ARGUMENT) :COUNT 3)))
    (SETF (FOREIGN-SLOT-VALUE
           (MEM-AREF ARG-LIST '(:UNION BM-CL-LIBWAYLAND:WL_ARGUMENT) 0)
           '(:UNION BM-CL-LIBWAYLAND:WL_ARGUMENT) 'WL-FFI::I)
            WIDTH)
    (SETF (FOREIGN-SLOT-VALUE
           (MEM-AREF ARG-LIST '(:UNION BM-CL-LIBWAYLAND:WL_ARGUMENT) 1)
           '(:UNION BM-CL-LIBWAYLAND:WL_ARGUMENT) 'WL-FFI::I)
            HEIGHT)
    (SETF (FOREIGN-SLOT-VALUE
           (MEM-AREF ARG-LIST '(:UNION BM-CL-LIBWAYLAND:WL_ARGUMENT) 2)
           '(:UNION BM-CL-LIBWAYLAND:WL_ARGUMENT) 'WL-FFI::A)
            (ERROR "WL C ARRAY PARSING NOT IMPLEMENTED"))
    (RESOURCE-POST-EVENT-ARRAY (PTR DISPATCH) 0 ARG-LIST)))

(DEFMETHOD SEND-CLOSE ((DISPATCH DISPATCH))
  (DEBUG-LOG! "Event: ~a~%" "close")
  (LET ((ARG-LIST (FOREIGN-ALLOC '(:UNION WL_ARGUMENT) :COUNT 0)))
    (RESOURCE-POST-EVENT-ARRAY (PTR DISPATCH) 1 ARG-LIST)))

(DEFMETHOD SEND-CONFIGURE-BOUNDS
           ((DISPATCH DISPATCH) WIDTH HEIGHT)
  (DEBUG-LOG! "Event: ~a~%" "configure_bounds")
  (LET ((ARG-LIST (FOREIGN-ALLOC '(:UNION WL_ARGUMENT) :COUNT 2)))
    (SETF (FOREIGN-SLOT-VALUE
           (MEM-AREF ARG-LIST '(:UNION BM-CL-LIBWAYLAND:WL_ARGUMENT) 0)
           '(:UNION BM-CL-LIBWAYLAND:WL_ARGUMENT) 'WL-FFI::I)
            WIDTH)
    (SETF (FOREIGN-SLOT-VALUE
           (MEM-AREF ARG-LIST '(:UNION BM-CL-LIBWAYLAND:WL_ARGUMENT) 1)
           '(:UNION BM-CL-LIBWAYLAND:WL_ARGUMENT) 'WL-FFI::I)
            HEIGHT)
    (RESOURCE-POST-EVENT-ARRAY (PTR DISPATCH) 2 ARG-LIST)))

(DEFMETHOD SEND-WM-CAPABILITIES
           ((DISPATCH DISPATCH) CAPABILITIES)
  (DEBUG-LOG! "Event: ~a~%" "wm_capabilities")
  (LET ((ARG-LIST (FOREIGN-ALLOC '(:UNION WL_ARGUMENT) :COUNT 1)))
    (SETF (FOREIGN-SLOT-VALUE
           (MEM-AREF ARG-LIST '(:UNION BM-CL-LIBWAYLAND:WL_ARGUMENT) 0)
           '(:UNION BM-CL-LIBWAYLAND:WL_ARGUMENT) 'WL-FFI::A)
            (ERROR "WL C ARRAY PARSING NOT IMPLEMENTED"))
    (RESOURCE-POST-EVENT-ARRAY (PTR DISPATCH) 3 ARG-LIST)))

(DEFCLASS GLOBAL (BM-CL-WAYLAND::GLOBAL) NIL
          (:DEFAULT-INITARGS :VERSION 6 :DISPATCH-IMPL 'DISPATCH)
          (:DOCUMENTATION "toplevel surface

This interface defines an xdg_surface role which allows a surface to,
      among other things, set window-like properties such as maximize,
      fullscreen, and minimize, set application-specific metadata like title and
      id, and well as trigger user interactive operations such as interactive
      resize and move.

      A xdg_toplevel by default is responsible for providing the full intended
      visual representation of the toplevel, which depending on the window
      state, may mean things like a title bar, window controls and drop shadow.

      Unmapping an xdg_toplevel means that the surface cannot be shown
      by the compositor until it is explicitly mapped again.
      All active operations (e.g., move, resize) are canceled and all
      attributes (e.g. title, state, stacking, ...) are discarded for
      an xdg_toplevel surface when it is unmapped. The xdg_toplevel returns to
      the state it had right after xdg_surface.get_toplevel. The client
      can re-map the toplevel by perfoming a commit without any buffer
      attached, waiting for a configure event and handling it as usual (see
      xdg_surface description).

      Attaching a null buffer to a toplevel unmaps the surface.
"))

(DEFMETHOD DISPATCH-BIND ((GLOBAL GLOBAL) CLIENT DATA VERSION ID)
  "Default bind implementation for the xdg_toplevel global object.
This can be overriden by inheritance in case if custom behaviour is required."
  (DEBUG-LOG! "Binding ~a~%" "xdg_toplevel")
  (LET ((BOUND
         (MAKE-INSTANCE (DISPATCH-IMPL GLOBAL) :DISPLAY (DISPLAY CLIENT)
                        :CLIENT CLIENT :ID ID)))
    (SETF (IFACE CLIENT ID) BOUND)))

(CL-ASYNC-UTIL:DEFINE-C-CALLBACK DISPATCH-BIND-FFI
    :VOID
    ((CLIENT :POINTER) (DATA :POINTER) (VERSION :UINT) (ID :UINT))
  (DEBUG-LOG! "C-Binding ~a~%" "xdg_toplevel")
  (LET* ((CLIENT (GET-CLIENT CLIENT)) (GLOBAL (GET-DATA DATA)))
    (FUNCALL 'DISPATCH-BIND GLOBAL CLIENT (NULL-POINTER) VERSION ID)))

(DEFVAR *DISPATCH-BIND* (CALLBACK DISPATCH-BIND-FFI))

(DEFMETHOD INITIALIZE-INSTANCE :AFTER ((GLOBAL GLOBAL) &KEY)
  (DEBUG-LOG! "Initializing global object: ~a~%" "xdg_toplevel")
  (LET* ((NEXT-DATA-ID (RESERVE-DATA))
         (GLOBAL-PTR
          (GLOBAL-CREATE (DISPLAY GLOBAL) *INTERFACE* (VERSION GLOBAL)
           (DATA-PTR NEXT-DATA-ID) *DISPATCH-BIND*)))
    (SETF (BM-CL-WAYLAND:PTR GLOBAL) GLOBAL-PTR)
    (SET-DATA NEXT-DATA-ID
     (SETF (GETHASH (POINTER-ADDRESS GLOBAL-PTR) *GLOBAL-TRACKER*) GLOBAL))))

(IN-PACKAGE :XDG_POPUP)

(DEFCLASS DISPATCH (BM-CL-WAYLAND:OBJECT) NIL (:DEFAULT-INITARGS :VERSION 6)
          (:DOCUMENTATION "short-lived, popup surfaces for menus

A popup surface is a short-lived, temporary surface. It can be used to
      implement for example menus, popovers, tooltips and other similar user
      interface concepts.

      A popup can be made to take an explicit grab. See xdg_popup.grab for
      details.

      When the popup is dismissed, a popup_done event will be sent out, and at
      the same time the surface will be unmapped. See the xdg_popup.popup_done
      event for details.

      Explicitly destroying the xdg_popup object will also dismiss the popup and
      unmap the surface. Clients that want to dismiss the popup when another
      surface of their own is clicked should dismiss the popup using the destroy
      request.

      A newly created xdg_popup will be stacked on top of all previously created
      xdg_popup surfaces associated with the same xdg_toplevel.

      The parent of an xdg_popup must be mapped (see the xdg_surface
      description) before the xdg_popup itself.

      The client must call wl_surface.commit on the corresponding wl_surface
      for the xdg_popup state to take effect.
"))

(DEFGENERIC DESTROY
    (RESOURCE))

(DEFGENERIC GRAB
    (RESOURCE SEAT SERIAL))

(DEFGENERIC REPOSITION
    (RESOURCE POSITIONER TOKEN))

(PUSHNEW
 (LIST "xdg_popup" (LIST "wl_seat" "xdg_positioner")
       (LAMBDA ()
         (SETF *INTERFACE* (FOREIGN-ALLOC '(:STRUCT INTERFACE)))
         (LET ((REQUESTS-PTR
                (LET ((MESSAGES
                       (CFFI:FOREIGN-ALLOC '(:STRUCT WL_MESSAGE) :COUNT 3)))
                  (LET ((INTERFACE-ARRAY
                         (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID))
                                             :COUNT 0))
                        (MSG-PTR (MEM-APTR MESSAGES '(:STRUCT WL_MESSAGE) 0)))
                    (WITH-FOREIGN-SLOTS
                     ((NAME SIGNATURE TYPES) MSG-PTR (:STRUCT WL_MESSAGE))
                     (SETF NAME (FOREIGN-STRING-ALLOC "destroy")
                           SIGNATURE ""
                           TYPES INTERFACE-ARRAY)))
                  (LET ((INTERFACE-ARRAY
                         (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID))
                                             :COUNT 2))
                        (MSG-PTR (MEM-APTR MESSAGES '(:STRUCT WL_MESSAGE) 1)))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 0)
                            WL_SEAT::*INTERFACE*)
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 1) (NULL-POINTER))
                    (WITH-FOREIGN-SLOTS
                     ((NAME SIGNATURE TYPES) MSG-PTR (:STRUCT WL_MESSAGE))
                     (SETF NAME (FOREIGN-STRING-ALLOC "grab")
                           SIGNATURE "ou"
                           TYPES INTERFACE-ARRAY)))
                  (LET ((INTERFACE-ARRAY
                         (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID))
                                             :COUNT 2))
                        (MSG-PTR (MEM-APTR MESSAGES '(:STRUCT WL_MESSAGE) 2)))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 0)
                            XDG_POSITIONER::*INTERFACE*)
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 1) (NULL-POINTER))
                    (WITH-FOREIGN-SLOTS
                     ((NAME SIGNATURE TYPES) MSG-PTR (:STRUCT WL_MESSAGE))
                     (SETF NAME (FOREIGN-STRING-ALLOC "reposition")
                           SIGNATURE "3ou"
                           TYPES INTERFACE-ARRAY)))
                  MESSAGES))
               (EVENTS-PTR
                (LET ((MESSAGES
                       (CFFI:FOREIGN-ALLOC '(:STRUCT WL_MESSAGE) :COUNT 3)))
                  (LET ((INTERFACE-ARRAY
                         (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID))
                                             :COUNT 4))
                        (MSG-PTR (MEM-APTR MESSAGES '(:STRUCT WL_MESSAGE) 0)))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 0) (NULL-POINTER))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 1) (NULL-POINTER))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 2) (NULL-POINTER))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 3) (NULL-POINTER))
                    (WITH-FOREIGN-SLOTS
                     ((NAME SIGNATURE TYPES) MSG-PTR (:STRUCT WL_MESSAGE))
                     (SETF NAME (FOREIGN-STRING-ALLOC "configure")
                           SIGNATURE "iiii"
                           TYPES INTERFACE-ARRAY)))
                  (LET ((INTERFACE-ARRAY
                         (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID))
                                             :COUNT 0))
                        (MSG-PTR (MEM-APTR MESSAGES '(:STRUCT WL_MESSAGE) 1)))
                    (WITH-FOREIGN-SLOTS
                     ((NAME SIGNATURE TYPES) MSG-PTR (:STRUCT WL_MESSAGE))
                     (SETF NAME (FOREIGN-STRING-ALLOC "popup_done")
                           SIGNATURE ""
                           TYPES INTERFACE-ARRAY)))
                  (LET ((INTERFACE-ARRAY
                         (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID))
                                             :COUNT 1))
                        (MSG-PTR (MEM-APTR MESSAGES '(:STRUCT WL_MESSAGE) 2)))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 0) (NULL-POINTER))
                    (WITH-FOREIGN-SLOTS
                     ((NAME SIGNATURE TYPES) MSG-PTR (:STRUCT WL_MESSAGE))
                     (SETF NAME (FOREIGN-STRING-ALLOC "repositioned")
                           SIGNATURE "3u"
                           TYPES INTERFACE-ARRAY)))
                  MESSAGES)))
           (DEBUG-LOG! "IF ptr range: ~a --- ~a~%" *INTERFACE*
            (MEM-APTR *INTERFACE* '(:STRUCT INTERFACE) 1))
           (WITH-FOREIGN-SLOTS
            ((NAME VERSION METHOD_COUNT METHODS EVENT_COUNT EVENTS) *INTERFACE*
             (:STRUCT INTERFACE))
            (SETF NAME (FOREIGN-STRING-ALLOC "xdg_popup")
                  VERSION 6
                  METHOD_COUNT 3
                  METHODS REQUESTS-PTR
                  EVENT_COUNT 3
                  EVENTS EVENTS-PTR)))))
 BM-CL-WAYLAND::*INTERFACE-INIT-LIST* :TEST #'INTERFACE-EXISTS-TEST)

(CL-ASYNC-UTIL:DEFINE-C-CALLBACK DISPATCHER-FFI
    :INT
    ((DATA :POINTER) (TARGET :POINTER) (OPCODE :UINT) (MESSAGE :POINTER)
     (ARGS :POINTER))
  (DECLARE (IGNORE DATA MESSAGE))
  (DEBUG-LOG! "Dispatcher invoked: ~a~%" "xdg_popup")
  (LET ((RESOURCE (GETHASH (POINTER-ADDRESS TARGET) *RESOURCE-TRACKER*)))
    (ECASE OPCODE
      (0 (FUNCALL 'DESTROY RESOURCE))
      (1
       (FUNCALL 'GRAB RESOURCE
                (GETHASH
                 (POINTER-ADDRESS
                  (FOREIGN-SLOT-VALUE
                   (MEM-APTR ARGS '(:UNION BM-CL-LIBWAYLAND:WL_ARGUMENT) 0)
                   '(:UNION BM-CL-LIBWAYLAND:WL_ARGUMENT)
                   'WL-FFI::O))
                 *RESOURCE-TRACKER*)
                (VALUES
                 (FOREIGN-SLOT-VALUE
                  (MEM-APTR ARGS '(:UNION BM-CL-LIBWAYLAND:WL_ARGUMENT) 1)
                  '(:UNION BM-CL-LIBWAYLAND:WL_ARGUMENT)
                  'WL-FFI::U))))
      (2
       (FUNCALL 'REPOSITION RESOURCE
                (GETHASH
                 (POINTER-ADDRESS
                  (FOREIGN-SLOT-VALUE
                   (MEM-APTR ARGS '(:UNION BM-CL-LIBWAYLAND:WL_ARGUMENT) 0)
                   '(:UNION BM-CL-LIBWAYLAND:WL_ARGUMENT)
                   'WL-FFI::O))
                 *RESOURCE-TRACKER*)
                (VALUES
                 (FOREIGN-SLOT-VALUE
                  (MEM-APTR ARGS '(:UNION BM-CL-LIBWAYLAND:WL_ARGUMENT) 1)
                  '(:UNION BM-CL-LIBWAYLAND:WL_ARGUMENT)
                  'WL-FFI::U))))))
  0)

(DEFVAR *DISPATCHER* (CALLBACK DISPATCHER-FFI))

(DEFMETHOD INITIALIZE-INSTANCE :AFTER ((INSTANCE DISPATCH) &KEY)
  (LET* ((RESOURCE
          (CREATE-RESOURCE (PTR (CLIENT INSTANCE)) *INTERFACE*
           (VERSION INSTANCE) (ID INSTANCE))))
    (SETF (GETHASH (POINTER-ADDRESS RESOURCE) *RESOURCE-TRACKER*) INSTANCE)
    (SETF (PTR INSTANCE) RESOURCE)
    (RESOURCE-SET-DISPATCHER RESOURCE *DISPATCHER* (NULL-POINTER)
     (NULL-POINTER) (NULL-POINTER))))

(DEFMETHOD SEND-CONFIGURE
           ((DISPATCH DISPATCH) X Y WIDTH
            HEIGHT)
  (DEBUG-LOG! "Event: ~a~%" "configure")
  (LET ((ARG-LIST (FOREIGN-ALLOC '(:UNION WL_ARGUMENT) :COUNT 4)))
    (SETF (FOREIGN-SLOT-VALUE
           (MEM-AREF ARG-LIST '(:UNION BM-CL-LIBWAYLAND:WL_ARGUMENT) 0)
           '(:UNION BM-CL-LIBWAYLAND:WL_ARGUMENT) 'WL-FFI::I)
            X)
    (SETF (FOREIGN-SLOT-VALUE
           (MEM-AREF ARG-LIST '(:UNION BM-CL-LIBWAYLAND:WL_ARGUMENT) 1)
           '(:UNION BM-CL-LIBWAYLAND:WL_ARGUMENT) 'WL-FFI::I)
            Y)
    (SETF (FOREIGN-SLOT-VALUE
           (MEM-AREF ARG-LIST '(:UNION BM-CL-LIBWAYLAND:WL_ARGUMENT) 2)
           '(:UNION BM-CL-LIBWAYLAND:WL_ARGUMENT) 'WL-FFI::I)
            WIDTH)
    (SETF (FOREIGN-SLOT-VALUE
           (MEM-AREF ARG-LIST '(:UNION BM-CL-LIBWAYLAND:WL_ARGUMENT) 3)
           '(:UNION BM-CL-LIBWAYLAND:WL_ARGUMENT) 'WL-FFI::I)
            HEIGHT)
    (RESOURCE-POST-EVENT-ARRAY (PTR DISPATCH) 0 ARG-LIST)))

(DEFMETHOD SEND-POPUP-DONE ((DISPATCH DISPATCH))
  (DEBUG-LOG! "Event: ~a~%" "popup_done")
  (LET ((ARG-LIST (FOREIGN-ALLOC '(:UNION WL_ARGUMENT) :COUNT 0)))
    (RESOURCE-POST-EVENT-ARRAY (PTR DISPATCH) 1 ARG-LIST)))

(DEFMETHOD SEND-REPOSITIONED
           ((DISPATCH DISPATCH) TOKEN)
  (DEBUG-LOG! "Event: ~a~%" "repositioned")
  (LET ((ARG-LIST (FOREIGN-ALLOC '(:UNION WL_ARGUMENT) :COUNT 1)))
    (SETF (FOREIGN-SLOT-VALUE
           (MEM-AREF ARG-LIST '(:UNION BM-CL-LIBWAYLAND:WL_ARGUMENT) 0)
           '(:UNION BM-CL-LIBWAYLAND:WL_ARGUMENT) 'WL-FFI::U)
            TOKEN)
    (RESOURCE-POST-EVENT-ARRAY (PTR DISPATCH) 2 ARG-LIST)))

(DEFCLASS GLOBAL (BM-CL-WAYLAND::GLOBAL) NIL
          (:DEFAULT-INITARGS :VERSION 6 :DISPATCH-IMPL 'DISPATCH)
          (:DOCUMENTATION "short-lived, popup surfaces for menus

A popup surface is a short-lived, temporary surface. It can be used to
      implement for example menus, popovers, tooltips and other similar user
      interface concepts.

      A popup can be made to take an explicit grab. See xdg_popup.grab for
      details.

      When the popup is dismissed, a popup_done event will be sent out, and at
      the same time the surface will be unmapped. See the xdg_popup.popup_done
      event for details.

      Explicitly destroying the xdg_popup object will also dismiss the popup and
      unmap the surface. Clients that want to dismiss the popup when another
      surface of their own is clicked should dismiss the popup using the destroy
      request.

      A newly created xdg_popup will be stacked on top of all previously created
      xdg_popup surfaces associated with the same xdg_toplevel.

      The parent of an xdg_popup must be mapped (see the xdg_surface
      description) before the xdg_popup itself.

      The client must call wl_surface.commit on the corresponding wl_surface
      for the xdg_popup state to take effect.
"))

(DEFMETHOD DISPATCH-BIND ((GLOBAL GLOBAL) CLIENT DATA VERSION ID)
  "Default bind implementation for the xdg_popup global object.
This can be overriden by inheritance in case if custom behaviour is required."
  (DEBUG-LOG! "Binding ~a~%" "xdg_popup")
  (LET ((BOUND
         (MAKE-INSTANCE (DISPATCH-IMPL GLOBAL) :DISPLAY (DISPLAY CLIENT)
                        :CLIENT CLIENT :ID ID)))
    (SETF (IFACE CLIENT ID) BOUND)))

(CL-ASYNC-UTIL:DEFINE-C-CALLBACK DISPATCH-BIND-FFI
    :VOID
    ((CLIENT :POINTER) (DATA :POINTER) (VERSION :UINT) (ID :UINT))
  (DEBUG-LOG! "C-Binding ~a~%" "xdg_popup")
  (LET* ((CLIENT (GET-CLIENT CLIENT)) (GLOBAL (GET-DATA DATA)))
    (FUNCALL 'DISPATCH-BIND GLOBAL CLIENT (NULL-POINTER) VERSION ID)))

(DEFVAR *DISPATCH-BIND* (CALLBACK DISPATCH-BIND-FFI))

(DEFMETHOD INITIALIZE-INSTANCE :AFTER ((GLOBAL GLOBAL) &KEY)
  (DEBUG-LOG! "Initializing global object: ~a~%" "xdg_popup")
  (LET* ((NEXT-DATA-ID (RESERVE-DATA))
         (GLOBAL-PTR
          (GLOBAL-CREATE (DISPLAY GLOBAL) *INTERFACE* (VERSION GLOBAL)
           (DATA-PTR NEXT-DATA-ID) *DISPATCH-BIND*)))
    (SETF (BM-CL-WAYLAND:PTR GLOBAL) GLOBAL-PTR)
    (SET-DATA NEXT-DATA-ID
     (SETF (GETHASH (POINTER-ADDRESS GLOBAL-PTR) *GLOBAL-TRACKER*) GLOBAL))))

