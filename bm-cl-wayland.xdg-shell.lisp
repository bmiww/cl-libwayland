(DEFPACKAGE :XDG_WM_BASE
  (:USE :CL :WL :CFFI)
  (:NICKNAMES :XDG-WM-BASE)
  (:EXPORT DISPATCH GLOBAL))

(IN-PACKAGE :XDG_WM_BASE)

(DEFCSTRUCT INTERFACE (NAME :STRING) (VERSION :INT) (METHOD_COUNT :INT)
 (METHODS (:POINTER (:STRUCT WL_MESSAGE))) (EVENT_COUNT :INT)
 (EVENTS (:POINTER (:STRUCT WL_MESSAGE))) (DESTROY :POINTER)
 (CREATE_POSITIONER :POINTER) (GET_XDG_SURFACE :POINTER) (PONG :POINTER))

(DEFVAR *INTERFACE* (CFFI:FOREIGN-ALLOC '(:STRUCT INTERFACE)))

(DEFPACKAGE :XDG_POSITIONER
  (:USE :CL :WL :CFFI)
  (:NICKNAMES :XDG-POSITIONER)
  (:EXPORT DISPATCH GLOBAL))

(IN-PACKAGE :XDG_POSITIONER)

(DEFCSTRUCT INTERFACE (NAME :STRING) (VERSION :INT) (METHOD_COUNT :INT)
 (METHODS (:POINTER (:STRUCT WL_MESSAGE))) (EVENT_COUNT :INT)
 (EVENTS (:POINTER (:STRUCT WL_MESSAGE))) (DESTROY :POINTER)
 (SET_SIZE :POINTER) (SET_ANCHOR_RECT :POINTER) (SET_ANCHOR :POINTER)
 (SET_GRAVITY :POINTER) (SET_CONSTRAINT_ADJUSTMENT :POINTER)
 (SET_OFFSET :POINTER) (SET_REACTIVE :POINTER) (SET_PARENT_SIZE :POINTER)
 (SET_PARENT_CONFIGURE :POINTER))

(DEFVAR *INTERFACE* (CFFI:FOREIGN-ALLOC '(:STRUCT INTERFACE)))

(DEFPACKAGE :XDG_SURFACE
  (:USE :CL :WL :CFFI)
  (:NICKNAMES :XDG-SURFACE)
  (:EXPORT DISPATCH GLOBAL))

(IN-PACKAGE :XDG_SURFACE)

(DEFCSTRUCT INTERFACE (NAME :STRING) (VERSION :INT) (METHOD_COUNT :INT)
 (METHODS (:POINTER (:STRUCT WL_MESSAGE))) (EVENT_COUNT :INT)
 (EVENTS (:POINTER (:STRUCT WL_MESSAGE))) (DESTROY :POINTER)
 (GET_TOPLEVEL :POINTER) (GET_POPUP :POINTER) (SET_WINDOW_GEOMETRY :POINTER)
 (ACK_CONFIGURE :POINTER))

(DEFVAR *INTERFACE* (CFFI:FOREIGN-ALLOC '(:STRUCT INTERFACE)))

(DEFPACKAGE :XDG_TOPLEVEL
  (:USE :CL :WL :CFFI)
  (:NICKNAMES :XDG-TOPLEVEL)
  (:EXPORT DISPATCH GLOBAL))

(IN-PACKAGE :XDG_TOPLEVEL)

(DEFCSTRUCT INTERFACE (NAME :STRING) (VERSION :INT) (METHOD_COUNT :INT)
 (METHODS (:POINTER (:STRUCT WL_MESSAGE))) (EVENT_COUNT :INT)
 (EVENTS (:POINTER (:STRUCT WL_MESSAGE))) (DESTROY :POINTER)
 (SET_PARENT :POINTER) (SET_TITLE :POINTER) (SET_APP_ID :POINTER)
 (SHOW_WINDOW_MENU :POINTER) (MOVE :POINTER) (RESIZE :POINTER)
 (SET_MAX_SIZE :POINTER) (SET_MIN_SIZE :POINTER) (SET_MAXIMIZED :POINTER)
 (UNSET_MAXIMIZED :POINTER) (SET_FULLSCREEN :POINTER)
 (UNSET_FULLSCREEN :POINTER) (SET_MINIMIZED :POINTER))

(DEFVAR *INTERFACE* (CFFI:FOREIGN-ALLOC '(:STRUCT INTERFACE)))

(DEFPACKAGE :XDG_POPUP
  (:USE :CL :WL :CFFI)
  (:NICKNAMES :XDG-POPUP)
  (:EXPORT DISPATCH GLOBAL))

(IN-PACKAGE :XDG_POPUP)

(DEFCSTRUCT INTERFACE (NAME :STRING) (VERSION :INT) (METHOD_COUNT :INT)
 (METHODS (:POINTER (:STRUCT WL_MESSAGE))) (EVENT_COUNT :INT)
 (EVENTS (:POINTER (:STRUCT WL_MESSAGE))) (DESTROY :POINTER) (GRAB :POINTER)
 (REPOSITION :POINTER))

(DEFVAR *INTERFACE* (CFFI:FOREIGN-ALLOC '(:STRUCT INTERFACE)))

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
    (RESOURCE CLIENT))

(DEFGENERIC CREATE_POSITIONER
    (RESOURCE CLIENT ID))

(DEFGENERIC GET_XDG_SURFACE
    (RESOURCE CLIENT ID SURFACE))

(DEFGENERIC PONG
    (RESOURCE CLIENT SERIAL))

(DEFVAR *REQUESTS*
  (LET ((MESSAGES (CFFI:FOREIGN-ALLOC '(:STRUCT WL_MESSAGE) :COUNT 4)))
    (LET ((INTERFACE-ARRAY
           (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID)) :COUNT 0)))
      (SETF (FOREIGN-SLOT-VALUE MESSAGES '(:STRUCT WL_MESSAGE)
             'BM-CL-LIBWAYLAND::NAME)
              (FOREIGN-STRING-ALLOC "destroy")
            (FOREIGN-SLOT-VALUE MESSAGES '(:STRUCT WL_MESSAGE)
             'BM-CL-LIBWAYLAND::SIGNATURE)
              (FOREIGN-STRING-ALLOC "")
            (FOREIGN-SLOT-VALUE MESSAGES '(:STRUCT WL_MESSAGE)
             'BM-CL-LIBWAYLAND::TYPES)
              INTERFACE-ARRAY))
    (LET ((INTERFACE-ARRAY
           (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID)) :COUNT 1)))
      (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 0) XDG_POSITIONER::*INTERFACE*)
      (SETF (FOREIGN-SLOT-VALUE MESSAGES '(:STRUCT WL_MESSAGE)
             'BM-CL-LIBWAYLAND::NAME)
              (FOREIGN-STRING-ALLOC "create_positioner")
            (FOREIGN-SLOT-VALUE MESSAGES '(:STRUCT WL_MESSAGE)
             'BM-CL-LIBWAYLAND::SIGNATURE)
              (FOREIGN-STRING-ALLOC "n")
            (FOREIGN-SLOT-VALUE MESSAGES '(:STRUCT WL_MESSAGE)
             'BM-CL-LIBWAYLAND::TYPES)
              INTERFACE-ARRAY))
    (LET ((INTERFACE-ARRAY
           (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID)) :COUNT 2)))
      (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 0) XDG_SURFACE::*INTERFACE*)
      (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 1) WL_SURFACE::*INTERFACE*)
      (SETF (FOREIGN-SLOT-VALUE MESSAGES '(:STRUCT WL_MESSAGE)
             'BM-CL-LIBWAYLAND::NAME)
              (FOREIGN-STRING-ALLOC "get_xdg_surface")
            (FOREIGN-SLOT-VALUE MESSAGES '(:STRUCT WL_MESSAGE)
             'BM-CL-LIBWAYLAND::SIGNATURE)
              (FOREIGN-STRING-ALLOC "no")
            (FOREIGN-SLOT-VALUE MESSAGES '(:STRUCT WL_MESSAGE)
             'BM-CL-LIBWAYLAND::TYPES)
              INTERFACE-ARRAY))
    (LET ((INTERFACE-ARRAY
           (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID)) :COUNT 1)))
      (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 0) (NULL-POINTER))
      (SETF (FOREIGN-SLOT-VALUE MESSAGES '(:STRUCT WL_MESSAGE)
             'BM-CL-LIBWAYLAND::NAME)
              (FOREIGN-STRING-ALLOC "pong")
            (FOREIGN-SLOT-VALUE MESSAGES '(:STRUCT WL_MESSAGE)
             'BM-CL-LIBWAYLAND::SIGNATURE)
              (FOREIGN-STRING-ALLOC "u")
            (FOREIGN-SLOT-VALUE MESSAGES '(:STRUCT WL_MESSAGE)
             'BM-CL-LIBWAYLAND::TYPES)
              INTERFACE-ARRAY))
    MESSAGES))

(DEFVAR *EVENTS*
  (LET ((MESSAGES (CFFI:FOREIGN-ALLOC '(:STRUCT WL_MESSAGE) :COUNT 1)))
    (LET ((INTERFACE-ARRAY
           (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID)) :COUNT 1)))
      (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 0) (NULL-POINTER))
      (SETF (FOREIGN-SLOT-VALUE MESSAGES '(:STRUCT WL_MESSAGE)
             'BM-CL-LIBWAYLAND::NAME)
              (FOREIGN-STRING-ALLOC "ping")
            (FOREIGN-SLOT-VALUE MESSAGES '(:STRUCT WL_MESSAGE)
             'BM-CL-LIBWAYLAND::SIGNATURE)
              (FOREIGN-STRING-ALLOC "u")
            (FOREIGN-SLOT-VALUE MESSAGES '(:STRUCT WL_MESSAGE)
             'BM-CL-LIBWAYLAND::TYPES)
              INTERFACE-ARRAY))
    MESSAGES))

(SETF (FOREIGN-SLOT-VALUE *INTERFACE* '(:STRUCT INTERFACE) 'NAME)
        (FOREIGN-STRING-ALLOC "xdg_wm_base")
      (FOREIGN-SLOT-VALUE *INTERFACE* '(:STRUCT INTERFACE) 'VERSION) 6
      (FOREIGN-SLOT-VALUE *INTERFACE* '(:STRUCT INTERFACE) 'METHOD_COUNT) 4
      (FOREIGN-SLOT-VALUE *INTERFACE* '(:STRUCT INTERFACE) 'METHODS) *REQUESTS*
      (FOREIGN-SLOT-VALUE *INTERFACE* '(:STRUCT INTERFACE) 'EVENT_COUNT) 1
      (FOREIGN-SLOT-VALUE *INTERFACE* '(:STRUCT INTERFACE) 'EVENTS) *EVENTS*)

(DEFMETHOD DISPATCHER ((DISPATCH DISPATCH) TARGET OPCODE MESSAGE ARGS)
  (DEBUG-LOG! "Dispatching ~a~%" "xdg_wm_base")
  (DEBUG-LOG! "With arguments target: ~a, opcode: ~a, message: ~a, args: ~a~%"
   TARGET OPCODE MESSAGE ARGS)
  (ERROR "DISPATCHER NOT IMPLEMENTED"))

(CL-ASYNC-UTIL:DEFINE-C-CALLBACK DISPATCHER-FFI
    :VOID
    ((DATA :POINTER) (TARGET :POINTER) (OPCODE :UINT) (MESSAGE :POINTER)
     (ARGS :POINTER))
  (LET ((RESOURCE (POP-DATA DATA)))
    (DISPATCHER RESOURCE TARGET OPCODE MESSAGE ARGS)
    (ERROR "C DISPATCHER MAYBE NOT IMPLEMENTED")))

(DEFVAR *DISPATCHER* (CALLBACK DISPATCHER-FFI))

(DEFCLASS GLOBAL (BM-CL-WAYLAND::GLOBAL) NIL (:DEFAULT-INITARGS :VERSION 6)
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
                        :CLIENT CLIENT)))
    (SETF (IFACE CLIENT ID) BOUND)
    (LET ((RESOURCE (CREATE-RESOURCE (PTR CLIENT) *INTERFACE* VERSION ID))
          (NEXT-DATA-ID (RESERVE-DATA BOUND)))
      (RESOURCE-SET-DISPATCHER RESOURCE *DISPATCHER* (NULL-POINTER)
       (DATA-PTR NEXT-DATA-ID) (NULL-POINTER)))))

(CL-ASYNC-UTIL:DEFINE-C-CALLBACK DISPATCH-BIND-FFI
    :VOID
    ((CLIENT :POINTER) (DATA :POINTER) (VERSION :UINT) (ID :UINT))
  (DEBUG-LOG! "C-Binding ~a~%" "xdg_wm_base")
  (LET* ((CLIENT (GET-CLIENT CLIENT)) (GLOBAL (POP-DATA DATA)))
    (FUNCALL 'DISPATCH-BIND GLOBAL CLIENT (NULL-POINTER) VERSION ID)))

(DEFVAR *DISPATCH-BIND* (CALLBACK DISPATCH-BIND-FFI))

(DEFMETHOD INITIALIZE-INSTANCE :AFTER ((GLOBAL GLOBAL) &KEY)
  (DEBUG-LOG! "Initializing global object: ~a~%" "xdg_wm_base")
  (LET* ((NEXT-DATA-ID (RESERVE-DATA))
         (GLOBAL-PTR
          (GLOBAL-CREATE (DISPLAY GLOBAL) *INTERFACE* (VERSION GLOBAL)
           (DATA-PTR NEXT-DATA-ID) *DISPATCH-BIND*)))
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
    (RESOURCE CLIENT))

(DEFGENERIC SET_SIZE
    (RESOURCE CLIENT WIDTH HEIGHT))

(DEFGENERIC SET_ANCHOR_RECT
    (RESOURCE CLIENT X Y WIDTH HEIGHT))

(DEFGENERIC SET_ANCHOR
    (RESOURCE CLIENT ANCHOR))

(DEFGENERIC SET_GRAVITY
    (RESOURCE CLIENT GRAVITY))

(DEFGENERIC SET_CONSTRAINT_ADJUSTMENT
    (RESOURCE CLIENT CONSTRAINT_ADJUSTMENT))

(DEFGENERIC SET_OFFSET
    (RESOURCE CLIENT X Y))

(DEFGENERIC SET_REACTIVE
    (RESOURCE CLIENT))

(DEFGENERIC SET_PARENT_SIZE
    (RESOURCE CLIENT PARENT_WIDTH PARENT_HEIGHT))

(DEFGENERIC SET_PARENT_CONFIGURE
    (RESOURCE CLIENT SERIAL))

(DEFVAR *REQUESTS*
  (LET ((MESSAGES (CFFI:FOREIGN-ALLOC '(:STRUCT WL_MESSAGE) :COUNT 10)))
    (LET ((INTERFACE-ARRAY
           (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID)) :COUNT 0)))
      (SETF (FOREIGN-SLOT-VALUE MESSAGES '(:STRUCT WL_MESSAGE)
             'BM-CL-LIBWAYLAND::NAME)
              (FOREIGN-STRING-ALLOC "destroy")
            (FOREIGN-SLOT-VALUE MESSAGES '(:STRUCT WL_MESSAGE)
             'BM-CL-LIBWAYLAND::SIGNATURE)
              (FOREIGN-STRING-ALLOC "")
            (FOREIGN-SLOT-VALUE MESSAGES '(:STRUCT WL_MESSAGE)
             'BM-CL-LIBWAYLAND::TYPES)
              INTERFACE-ARRAY))
    (LET ((INTERFACE-ARRAY
           (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID)) :COUNT 2)))
      (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 0) (NULL-POINTER))
      (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 1) (NULL-POINTER))
      (SETF (FOREIGN-SLOT-VALUE MESSAGES '(:STRUCT WL_MESSAGE)
             'BM-CL-LIBWAYLAND::NAME)
              (FOREIGN-STRING-ALLOC "set_size")
            (FOREIGN-SLOT-VALUE MESSAGES '(:STRUCT WL_MESSAGE)
             'BM-CL-LIBWAYLAND::SIGNATURE)
              (FOREIGN-STRING-ALLOC "ii")
            (FOREIGN-SLOT-VALUE MESSAGES '(:STRUCT WL_MESSAGE)
             'BM-CL-LIBWAYLAND::TYPES)
              INTERFACE-ARRAY))
    (LET ((INTERFACE-ARRAY
           (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID)) :COUNT 4)))
      (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 0) (NULL-POINTER))
      (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 1) (NULL-POINTER))
      (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 2) (NULL-POINTER))
      (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 3) (NULL-POINTER))
      (SETF (FOREIGN-SLOT-VALUE MESSAGES '(:STRUCT WL_MESSAGE)
             'BM-CL-LIBWAYLAND::NAME)
              (FOREIGN-STRING-ALLOC "set_anchor_rect")
            (FOREIGN-SLOT-VALUE MESSAGES '(:STRUCT WL_MESSAGE)
             'BM-CL-LIBWAYLAND::SIGNATURE)
              (FOREIGN-STRING-ALLOC "iiii")
            (FOREIGN-SLOT-VALUE MESSAGES '(:STRUCT WL_MESSAGE)
             'BM-CL-LIBWAYLAND::TYPES)
              INTERFACE-ARRAY))
    (LET ((INTERFACE-ARRAY
           (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID)) :COUNT 1)))
      (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 0) (NULL-POINTER))
      (SETF (FOREIGN-SLOT-VALUE MESSAGES '(:STRUCT WL_MESSAGE)
             'BM-CL-LIBWAYLAND::NAME)
              (FOREIGN-STRING-ALLOC "set_anchor")
            (FOREIGN-SLOT-VALUE MESSAGES '(:STRUCT WL_MESSAGE)
             'BM-CL-LIBWAYLAND::SIGNATURE)
              (FOREIGN-STRING-ALLOC "u")
            (FOREIGN-SLOT-VALUE MESSAGES '(:STRUCT WL_MESSAGE)
             'BM-CL-LIBWAYLAND::TYPES)
              INTERFACE-ARRAY))
    (LET ((INTERFACE-ARRAY
           (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID)) :COUNT 1)))
      (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 0) (NULL-POINTER))
      (SETF (FOREIGN-SLOT-VALUE MESSAGES '(:STRUCT WL_MESSAGE)
             'BM-CL-LIBWAYLAND::NAME)
              (FOREIGN-STRING-ALLOC "set_gravity")
            (FOREIGN-SLOT-VALUE MESSAGES '(:STRUCT WL_MESSAGE)
             'BM-CL-LIBWAYLAND::SIGNATURE)
              (FOREIGN-STRING-ALLOC "u")
            (FOREIGN-SLOT-VALUE MESSAGES '(:STRUCT WL_MESSAGE)
             'BM-CL-LIBWAYLAND::TYPES)
              INTERFACE-ARRAY))
    (LET ((INTERFACE-ARRAY
           (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID)) :COUNT 1)))
      (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 0) (NULL-POINTER))
      (SETF (FOREIGN-SLOT-VALUE MESSAGES '(:STRUCT WL_MESSAGE)
             'BM-CL-LIBWAYLAND::NAME)
              (FOREIGN-STRING-ALLOC "set_constraint_adjustment")
            (FOREIGN-SLOT-VALUE MESSAGES '(:STRUCT WL_MESSAGE)
             'BM-CL-LIBWAYLAND::SIGNATURE)
              (FOREIGN-STRING-ALLOC "u")
            (FOREIGN-SLOT-VALUE MESSAGES '(:STRUCT WL_MESSAGE)
             'BM-CL-LIBWAYLAND::TYPES)
              INTERFACE-ARRAY))
    (LET ((INTERFACE-ARRAY
           (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID)) :COUNT 2)))
      (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 0) (NULL-POINTER))
      (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 1) (NULL-POINTER))
      (SETF (FOREIGN-SLOT-VALUE MESSAGES '(:STRUCT WL_MESSAGE)
             'BM-CL-LIBWAYLAND::NAME)
              (FOREIGN-STRING-ALLOC "set_offset")
            (FOREIGN-SLOT-VALUE MESSAGES '(:STRUCT WL_MESSAGE)
             'BM-CL-LIBWAYLAND::SIGNATURE)
              (FOREIGN-STRING-ALLOC "ii")
            (FOREIGN-SLOT-VALUE MESSAGES '(:STRUCT WL_MESSAGE)
             'BM-CL-LIBWAYLAND::TYPES)
              INTERFACE-ARRAY))
    (LET ((INTERFACE-ARRAY
           (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID)) :COUNT 0)))
      (SETF (FOREIGN-SLOT-VALUE MESSAGES '(:STRUCT WL_MESSAGE)
             'BM-CL-LIBWAYLAND::NAME)
              (FOREIGN-STRING-ALLOC "set_reactive")
            (FOREIGN-SLOT-VALUE MESSAGES '(:STRUCT WL_MESSAGE)
             'BM-CL-LIBWAYLAND::SIGNATURE)
              (FOREIGN-STRING-ALLOC "3")
            (FOREIGN-SLOT-VALUE MESSAGES '(:STRUCT WL_MESSAGE)
             'BM-CL-LIBWAYLAND::TYPES)
              INTERFACE-ARRAY))
    (LET ((INTERFACE-ARRAY
           (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID)) :COUNT 2)))
      (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 0) (NULL-POINTER))
      (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 1) (NULL-POINTER))
      (SETF (FOREIGN-SLOT-VALUE MESSAGES '(:STRUCT WL_MESSAGE)
             'BM-CL-LIBWAYLAND::NAME)
              (FOREIGN-STRING-ALLOC "set_parent_size")
            (FOREIGN-SLOT-VALUE MESSAGES '(:STRUCT WL_MESSAGE)
             'BM-CL-LIBWAYLAND::SIGNATURE)
              (FOREIGN-STRING-ALLOC "3ii")
            (FOREIGN-SLOT-VALUE MESSAGES '(:STRUCT WL_MESSAGE)
             'BM-CL-LIBWAYLAND::TYPES)
              INTERFACE-ARRAY))
    (LET ((INTERFACE-ARRAY
           (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID)) :COUNT 1)))
      (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 0) (NULL-POINTER))
      (SETF (FOREIGN-SLOT-VALUE MESSAGES '(:STRUCT WL_MESSAGE)
             'BM-CL-LIBWAYLAND::NAME)
              (FOREIGN-STRING-ALLOC "set_parent_configure")
            (FOREIGN-SLOT-VALUE MESSAGES '(:STRUCT WL_MESSAGE)
             'BM-CL-LIBWAYLAND::SIGNATURE)
              (FOREIGN-STRING-ALLOC "3u")
            (FOREIGN-SLOT-VALUE MESSAGES '(:STRUCT WL_MESSAGE)
             'BM-CL-LIBWAYLAND::TYPES)
              INTERFACE-ARRAY))
    MESSAGES))

(DEFVAR *EVENTS*
  (LET ((MESSAGES (CFFI:FOREIGN-ALLOC '(:STRUCT WL_MESSAGE) :COUNT 0)))
    MESSAGES))

(SETF (FOREIGN-SLOT-VALUE *INTERFACE* '(:STRUCT INTERFACE) 'NAME)
        (FOREIGN-STRING-ALLOC "xdg_positioner")
      (FOREIGN-SLOT-VALUE *INTERFACE* '(:STRUCT INTERFACE) 'VERSION) 6
      (FOREIGN-SLOT-VALUE *INTERFACE* '(:STRUCT INTERFACE) 'METHOD_COUNT) 10
      (FOREIGN-SLOT-VALUE *INTERFACE* '(:STRUCT INTERFACE) 'METHODS) *REQUESTS*
      (FOREIGN-SLOT-VALUE *INTERFACE* '(:STRUCT INTERFACE) 'EVENT_COUNT) 0
      (FOREIGN-SLOT-VALUE *INTERFACE* '(:STRUCT INTERFACE) 'EVENTS) *EVENTS*)

(DEFMETHOD DISPATCHER ((DISPATCH DISPATCH) TARGET OPCODE MESSAGE ARGS)
  (DEBUG-LOG! "Dispatching ~a~%" "xdg_positioner")
  (DEBUG-LOG! "With arguments target: ~a, opcode: ~a, message: ~a, args: ~a~%"
   TARGET OPCODE MESSAGE ARGS)
  (ERROR "DISPATCHER NOT IMPLEMENTED"))

(CL-ASYNC-UTIL:DEFINE-C-CALLBACK DISPATCHER-FFI
    :VOID
    ((DATA :POINTER) (TARGET :POINTER) (OPCODE :UINT) (MESSAGE :POINTER)
     (ARGS :POINTER))
  (LET ((RESOURCE (POP-DATA DATA)))
    (DISPATCHER RESOURCE TARGET OPCODE MESSAGE ARGS)
    (ERROR "C DISPATCHER MAYBE NOT IMPLEMENTED")))

(DEFVAR *DISPATCHER* (CALLBACK DISPATCHER-FFI))

(DEFCLASS GLOBAL (BM-CL-WAYLAND::GLOBAL) NIL (:DEFAULT-INITARGS :VERSION 6)
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
                        :CLIENT CLIENT)))
    (SETF (IFACE CLIENT ID) BOUND)
    (LET ((RESOURCE (CREATE-RESOURCE (PTR CLIENT) *INTERFACE* VERSION ID))
          (NEXT-DATA-ID (RESERVE-DATA BOUND)))
      (RESOURCE-SET-DISPATCHER RESOURCE *DISPATCHER* (NULL-POINTER)
       (DATA-PTR NEXT-DATA-ID) (NULL-POINTER)))))

(CL-ASYNC-UTIL:DEFINE-C-CALLBACK DISPATCH-BIND-FFI
    :VOID
    ((CLIENT :POINTER) (DATA :POINTER) (VERSION :UINT) (ID :UINT))
  (DEBUG-LOG! "C-Binding ~a~%" "xdg_positioner")
  (LET* ((CLIENT (GET-CLIENT CLIENT)) (GLOBAL (POP-DATA DATA)))
    (FUNCALL 'DISPATCH-BIND GLOBAL CLIENT (NULL-POINTER) VERSION ID)))

(DEFVAR *DISPATCH-BIND* (CALLBACK DISPATCH-BIND-FFI))

(DEFMETHOD INITIALIZE-INSTANCE :AFTER ((GLOBAL GLOBAL) &KEY)
  (DEBUG-LOG! "Initializing global object: ~a~%" "xdg_positioner")
  (LET* ((NEXT-DATA-ID (RESERVE-DATA))
         (GLOBAL-PTR
          (GLOBAL-CREATE (DISPLAY GLOBAL) *INTERFACE* (VERSION GLOBAL)
           (DATA-PTR NEXT-DATA-ID) *DISPATCH-BIND*)))
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
    (RESOURCE CLIENT))

(DEFGENERIC GET_TOPLEVEL
    (RESOURCE CLIENT ID))

(DEFGENERIC GET_POPUP
    (RESOURCE CLIENT ID PARENT POSITIONER))

(DEFGENERIC SET_WINDOW_GEOMETRY
    (RESOURCE CLIENT X Y WIDTH HEIGHT))

(DEFGENERIC ACK_CONFIGURE
    (RESOURCE CLIENT SERIAL))

(DEFVAR *REQUESTS*
  (LET ((MESSAGES (CFFI:FOREIGN-ALLOC '(:STRUCT WL_MESSAGE) :COUNT 5)))
    (LET ((INTERFACE-ARRAY
           (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID)) :COUNT 0)))
      (SETF (FOREIGN-SLOT-VALUE MESSAGES '(:STRUCT WL_MESSAGE)
             'BM-CL-LIBWAYLAND::NAME)
              (FOREIGN-STRING-ALLOC "destroy")
            (FOREIGN-SLOT-VALUE MESSAGES '(:STRUCT WL_MESSAGE)
             'BM-CL-LIBWAYLAND::SIGNATURE)
              (FOREIGN-STRING-ALLOC "")
            (FOREIGN-SLOT-VALUE MESSAGES '(:STRUCT WL_MESSAGE)
             'BM-CL-LIBWAYLAND::TYPES)
              INTERFACE-ARRAY))
    (LET ((INTERFACE-ARRAY
           (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID)) :COUNT 1)))
      (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 0) XDG_TOPLEVEL::*INTERFACE*)
      (SETF (FOREIGN-SLOT-VALUE MESSAGES '(:STRUCT WL_MESSAGE)
             'BM-CL-LIBWAYLAND::NAME)
              (FOREIGN-STRING-ALLOC "get_toplevel")
            (FOREIGN-SLOT-VALUE MESSAGES '(:STRUCT WL_MESSAGE)
             'BM-CL-LIBWAYLAND::SIGNATURE)
              (FOREIGN-STRING-ALLOC "n")
            (FOREIGN-SLOT-VALUE MESSAGES '(:STRUCT WL_MESSAGE)
             'BM-CL-LIBWAYLAND::TYPES)
              INTERFACE-ARRAY))
    (LET ((INTERFACE-ARRAY
           (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID)) :COUNT 3)))
      (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 0) XDG_POPUP::*INTERFACE*)
      (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 1) XDG_SURFACE::*INTERFACE*)
      (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 2) XDG_POSITIONER::*INTERFACE*)
      (SETF (FOREIGN-SLOT-VALUE MESSAGES '(:STRUCT WL_MESSAGE)
             'BM-CL-LIBWAYLAND::NAME)
              (FOREIGN-STRING-ALLOC "get_popup")
            (FOREIGN-SLOT-VALUE MESSAGES '(:STRUCT WL_MESSAGE)
             'BM-CL-LIBWAYLAND::SIGNATURE)
              (FOREIGN-STRING-ALLOC "n?oo")
            (FOREIGN-SLOT-VALUE MESSAGES '(:STRUCT WL_MESSAGE)
             'BM-CL-LIBWAYLAND::TYPES)
              INTERFACE-ARRAY))
    (LET ((INTERFACE-ARRAY
           (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID)) :COUNT 4)))
      (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 0) (NULL-POINTER))
      (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 1) (NULL-POINTER))
      (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 2) (NULL-POINTER))
      (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 3) (NULL-POINTER))
      (SETF (FOREIGN-SLOT-VALUE MESSAGES '(:STRUCT WL_MESSAGE)
             'BM-CL-LIBWAYLAND::NAME)
              (FOREIGN-STRING-ALLOC "set_window_geometry")
            (FOREIGN-SLOT-VALUE MESSAGES '(:STRUCT WL_MESSAGE)
             'BM-CL-LIBWAYLAND::SIGNATURE)
              (FOREIGN-STRING-ALLOC "iiii")
            (FOREIGN-SLOT-VALUE MESSAGES '(:STRUCT WL_MESSAGE)
             'BM-CL-LIBWAYLAND::TYPES)
              INTERFACE-ARRAY))
    (LET ((INTERFACE-ARRAY
           (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID)) :COUNT 1)))
      (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 0) (NULL-POINTER))
      (SETF (FOREIGN-SLOT-VALUE MESSAGES '(:STRUCT WL_MESSAGE)
             'BM-CL-LIBWAYLAND::NAME)
              (FOREIGN-STRING-ALLOC "ack_configure")
            (FOREIGN-SLOT-VALUE MESSAGES '(:STRUCT WL_MESSAGE)
             'BM-CL-LIBWAYLAND::SIGNATURE)
              (FOREIGN-STRING-ALLOC "u")
            (FOREIGN-SLOT-VALUE MESSAGES '(:STRUCT WL_MESSAGE)
             'BM-CL-LIBWAYLAND::TYPES)
              INTERFACE-ARRAY))
    MESSAGES))

(DEFVAR *EVENTS*
  (LET ((MESSAGES (CFFI:FOREIGN-ALLOC '(:STRUCT WL_MESSAGE) :COUNT 1)))
    (LET ((INTERFACE-ARRAY
           (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID)) :COUNT 1)))
      (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 0) (NULL-POINTER))
      (SETF (FOREIGN-SLOT-VALUE MESSAGES '(:STRUCT WL_MESSAGE)
             'BM-CL-LIBWAYLAND::NAME)
              (FOREIGN-STRING-ALLOC "configure")
            (FOREIGN-SLOT-VALUE MESSAGES '(:STRUCT WL_MESSAGE)
             'BM-CL-LIBWAYLAND::SIGNATURE)
              (FOREIGN-STRING-ALLOC "u")
            (FOREIGN-SLOT-VALUE MESSAGES '(:STRUCT WL_MESSAGE)
             'BM-CL-LIBWAYLAND::TYPES)
              INTERFACE-ARRAY))
    MESSAGES))

(SETF (FOREIGN-SLOT-VALUE *INTERFACE* '(:STRUCT INTERFACE) 'NAME)
        (FOREIGN-STRING-ALLOC "xdg_surface")
      (FOREIGN-SLOT-VALUE *INTERFACE* '(:STRUCT INTERFACE) 'VERSION) 6
      (FOREIGN-SLOT-VALUE *INTERFACE* '(:STRUCT INTERFACE) 'METHOD_COUNT) 5
      (FOREIGN-SLOT-VALUE *INTERFACE* '(:STRUCT INTERFACE) 'METHODS) *REQUESTS*
      (FOREIGN-SLOT-VALUE *INTERFACE* '(:STRUCT INTERFACE) 'EVENT_COUNT) 1
      (FOREIGN-SLOT-VALUE *INTERFACE* '(:STRUCT INTERFACE) 'EVENTS) *EVENTS*)

(DEFMETHOD DISPATCHER ((DISPATCH DISPATCH) TARGET OPCODE MESSAGE ARGS)
  (DEBUG-LOG! "Dispatching ~a~%" "xdg_surface")
  (DEBUG-LOG! "With arguments target: ~a, opcode: ~a, message: ~a, args: ~a~%"
   TARGET OPCODE MESSAGE ARGS)
  (ERROR "DISPATCHER NOT IMPLEMENTED"))

(CL-ASYNC-UTIL:DEFINE-C-CALLBACK DISPATCHER-FFI
    :VOID
    ((DATA :POINTER) (TARGET :POINTER) (OPCODE :UINT) (MESSAGE :POINTER)
     (ARGS :POINTER))
  (LET ((RESOURCE (POP-DATA DATA)))
    (DISPATCHER RESOURCE TARGET OPCODE MESSAGE ARGS)
    (ERROR "C DISPATCHER MAYBE NOT IMPLEMENTED")))

(DEFVAR *DISPATCHER* (CALLBACK DISPATCHER-FFI))

(DEFCLASS GLOBAL (BM-CL-WAYLAND::GLOBAL) NIL (:DEFAULT-INITARGS :VERSION 6)
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
                        :CLIENT CLIENT)))
    (SETF (IFACE CLIENT ID) BOUND)
    (LET ((RESOURCE (CREATE-RESOURCE (PTR CLIENT) *INTERFACE* VERSION ID))
          (NEXT-DATA-ID (RESERVE-DATA BOUND)))
      (RESOURCE-SET-DISPATCHER RESOURCE *DISPATCHER* (NULL-POINTER)
       (DATA-PTR NEXT-DATA-ID) (NULL-POINTER)))))

(CL-ASYNC-UTIL:DEFINE-C-CALLBACK DISPATCH-BIND-FFI
    :VOID
    ((CLIENT :POINTER) (DATA :POINTER) (VERSION :UINT) (ID :UINT))
  (DEBUG-LOG! "C-Binding ~a~%" "xdg_surface")
  (LET* ((CLIENT (GET-CLIENT CLIENT)) (GLOBAL (POP-DATA DATA)))
    (FUNCALL 'DISPATCH-BIND GLOBAL CLIENT (NULL-POINTER) VERSION ID)))

(DEFVAR *DISPATCH-BIND* (CALLBACK DISPATCH-BIND-FFI))

(DEFMETHOD INITIALIZE-INSTANCE :AFTER ((GLOBAL GLOBAL) &KEY)
  (DEBUG-LOG! "Initializing global object: ~a~%" "xdg_surface")
  (LET* ((NEXT-DATA-ID (RESERVE-DATA))
         (GLOBAL-PTR
          (GLOBAL-CREATE (DISPLAY GLOBAL) *INTERFACE* (VERSION GLOBAL)
           (DATA-PTR NEXT-DATA-ID) *DISPATCH-BIND*)))
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
    (RESOURCE CLIENT))

(DEFGENERIC SET_PARENT
    (RESOURCE CLIENT PARENT))

(DEFGENERIC SET_TITLE
    (RESOURCE CLIENT TITLE))

(DEFGENERIC SET_APP_ID
    (RESOURCE CLIENT APP_ID))

(DEFGENERIC SHOW_WINDOW_MENU
    (RESOURCE CLIENT SEAT SERIAL X Y))

(DEFGENERIC MOVE
    (RESOURCE CLIENT SEAT SERIAL))

(DEFGENERIC RESIZE
    (RESOURCE CLIENT SEAT SERIAL EDGES))

(DEFGENERIC SET_MAX_SIZE
    (RESOURCE CLIENT WIDTH HEIGHT))

(DEFGENERIC SET_MIN_SIZE
    (RESOURCE CLIENT WIDTH HEIGHT))

(DEFGENERIC SET_MAXIMIZED
    (RESOURCE CLIENT))

(DEFGENERIC UNSET_MAXIMIZED
    (RESOURCE CLIENT))

(DEFGENERIC SET_FULLSCREEN
    (RESOURCE CLIENT OUTPUT))

(DEFGENERIC UNSET_FULLSCREEN
    (RESOURCE CLIENT))

(DEFGENERIC SET_MINIMIZED
    (RESOURCE CLIENT))

(DEFVAR *REQUESTS*
  (LET ((MESSAGES (CFFI:FOREIGN-ALLOC '(:STRUCT WL_MESSAGE) :COUNT 14)))
    (LET ((INTERFACE-ARRAY
           (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID)) :COUNT 0)))
      (SETF (FOREIGN-SLOT-VALUE MESSAGES '(:STRUCT WL_MESSAGE)
             'BM-CL-LIBWAYLAND::NAME)
              (FOREIGN-STRING-ALLOC "destroy")
            (FOREIGN-SLOT-VALUE MESSAGES '(:STRUCT WL_MESSAGE)
             'BM-CL-LIBWAYLAND::SIGNATURE)
              (FOREIGN-STRING-ALLOC "")
            (FOREIGN-SLOT-VALUE MESSAGES '(:STRUCT WL_MESSAGE)
             'BM-CL-LIBWAYLAND::TYPES)
              INTERFACE-ARRAY))
    (LET ((INTERFACE-ARRAY
           (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID)) :COUNT 1)))
      (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 0) XDG_TOPLEVEL::*INTERFACE*)
      (SETF (FOREIGN-SLOT-VALUE MESSAGES '(:STRUCT WL_MESSAGE)
             'BM-CL-LIBWAYLAND::NAME)
              (FOREIGN-STRING-ALLOC "set_parent")
            (FOREIGN-SLOT-VALUE MESSAGES '(:STRUCT WL_MESSAGE)
             'BM-CL-LIBWAYLAND::SIGNATURE)
              (FOREIGN-STRING-ALLOC "?o")
            (FOREIGN-SLOT-VALUE MESSAGES '(:STRUCT WL_MESSAGE)
             'BM-CL-LIBWAYLAND::TYPES)
              INTERFACE-ARRAY))
    (LET ((INTERFACE-ARRAY
           (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID)) :COUNT 1)))
      (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 0) (NULL-POINTER))
      (SETF (FOREIGN-SLOT-VALUE MESSAGES '(:STRUCT WL_MESSAGE)
             'BM-CL-LIBWAYLAND::NAME)
              (FOREIGN-STRING-ALLOC "set_title")
            (FOREIGN-SLOT-VALUE MESSAGES '(:STRUCT WL_MESSAGE)
             'BM-CL-LIBWAYLAND::SIGNATURE)
              (FOREIGN-STRING-ALLOC "s")
            (FOREIGN-SLOT-VALUE MESSAGES '(:STRUCT WL_MESSAGE)
             'BM-CL-LIBWAYLAND::TYPES)
              INTERFACE-ARRAY))
    (LET ((INTERFACE-ARRAY
           (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID)) :COUNT 1)))
      (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 0) (NULL-POINTER))
      (SETF (FOREIGN-SLOT-VALUE MESSAGES '(:STRUCT WL_MESSAGE)
             'BM-CL-LIBWAYLAND::NAME)
              (FOREIGN-STRING-ALLOC "set_app_id")
            (FOREIGN-SLOT-VALUE MESSAGES '(:STRUCT WL_MESSAGE)
             'BM-CL-LIBWAYLAND::SIGNATURE)
              (FOREIGN-STRING-ALLOC "s")
            (FOREIGN-SLOT-VALUE MESSAGES '(:STRUCT WL_MESSAGE)
             'BM-CL-LIBWAYLAND::TYPES)
              INTERFACE-ARRAY))
    (LET ((INTERFACE-ARRAY
           (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID)) :COUNT 4)))
      (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 0) WL_SEAT::*INTERFACE*)
      (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 1) (NULL-POINTER))
      (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 2) (NULL-POINTER))
      (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 3) (NULL-POINTER))
      (SETF (FOREIGN-SLOT-VALUE MESSAGES '(:STRUCT WL_MESSAGE)
             'BM-CL-LIBWAYLAND::NAME)
              (FOREIGN-STRING-ALLOC "show_window_menu")
            (FOREIGN-SLOT-VALUE MESSAGES '(:STRUCT WL_MESSAGE)
             'BM-CL-LIBWAYLAND::SIGNATURE)
              (FOREIGN-STRING-ALLOC "ouii")
            (FOREIGN-SLOT-VALUE MESSAGES '(:STRUCT WL_MESSAGE)
             'BM-CL-LIBWAYLAND::TYPES)
              INTERFACE-ARRAY))
    (LET ((INTERFACE-ARRAY
           (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID)) :COUNT 2)))
      (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 0) WL_SEAT::*INTERFACE*)
      (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 1) (NULL-POINTER))
      (SETF (FOREIGN-SLOT-VALUE MESSAGES '(:STRUCT WL_MESSAGE)
             'BM-CL-LIBWAYLAND::NAME)
              (FOREIGN-STRING-ALLOC "move")
            (FOREIGN-SLOT-VALUE MESSAGES '(:STRUCT WL_MESSAGE)
             'BM-CL-LIBWAYLAND::SIGNATURE)
              (FOREIGN-STRING-ALLOC "ou")
            (FOREIGN-SLOT-VALUE MESSAGES '(:STRUCT WL_MESSAGE)
             'BM-CL-LIBWAYLAND::TYPES)
              INTERFACE-ARRAY))
    (LET ((INTERFACE-ARRAY
           (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID)) :COUNT 3)))
      (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 0) WL_SEAT::*INTERFACE*)
      (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 1) (NULL-POINTER))
      (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 2) (NULL-POINTER))
      (SETF (FOREIGN-SLOT-VALUE MESSAGES '(:STRUCT WL_MESSAGE)
             'BM-CL-LIBWAYLAND::NAME)
              (FOREIGN-STRING-ALLOC "resize")
            (FOREIGN-SLOT-VALUE MESSAGES '(:STRUCT WL_MESSAGE)
             'BM-CL-LIBWAYLAND::SIGNATURE)
              (FOREIGN-STRING-ALLOC "ouu")
            (FOREIGN-SLOT-VALUE MESSAGES '(:STRUCT WL_MESSAGE)
             'BM-CL-LIBWAYLAND::TYPES)
              INTERFACE-ARRAY))
    (LET ((INTERFACE-ARRAY
           (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID)) :COUNT 2)))
      (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 0) (NULL-POINTER))
      (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 1) (NULL-POINTER))
      (SETF (FOREIGN-SLOT-VALUE MESSAGES '(:STRUCT WL_MESSAGE)
             'BM-CL-LIBWAYLAND::NAME)
              (FOREIGN-STRING-ALLOC "set_max_size")
            (FOREIGN-SLOT-VALUE MESSAGES '(:STRUCT WL_MESSAGE)
             'BM-CL-LIBWAYLAND::SIGNATURE)
              (FOREIGN-STRING-ALLOC "ii")
            (FOREIGN-SLOT-VALUE MESSAGES '(:STRUCT WL_MESSAGE)
             'BM-CL-LIBWAYLAND::TYPES)
              INTERFACE-ARRAY))
    (LET ((INTERFACE-ARRAY
           (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID)) :COUNT 2)))
      (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 0) (NULL-POINTER))
      (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 1) (NULL-POINTER))
      (SETF (FOREIGN-SLOT-VALUE MESSAGES '(:STRUCT WL_MESSAGE)
             'BM-CL-LIBWAYLAND::NAME)
              (FOREIGN-STRING-ALLOC "set_min_size")
            (FOREIGN-SLOT-VALUE MESSAGES '(:STRUCT WL_MESSAGE)
             'BM-CL-LIBWAYLAND::SIGNATURE)
              (FOREIGN-STRING-ALLOC "ii")
            (FOREIGN-SLOT-VALUE MESSAGES '(:STRUCT WL_MESSAGE)
             'BM-CL-LIBWAYLAND::TYPES)
              INTERFACE-ARRAY))
    (LET ((INTERFACE-ARRAY
           (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID)) :COUNT 0)))
      (SETF (FOREIGN-SLOT-VALUE MESSAGES '(:STRUCT WL_MESSAGE)
             'BM-CL-LIBWAYLAND::NAME)
              (FOREIGN-STRING-ALLOC "set_maximized")
            (FOREIGN-SLOT-VALUE MESSAGES '(:STRUCT WL_MESSAGE)
             'BM-CL-LIBWAYLAND::SIGNATURE)
              (FOREIGN-STRING-ALLOC "")
            (FOREIGN-SLOT-VALUE MESSAGES '(:STRUCT WL_MESSAGE)
             'BM-CL-LIBWAYLAND::TYPES)
              INTERFACE-ARRAY))
    (LET ((INTERFACE-ARRAY
           (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID)) :COUNT 0)))
      (SETF (FOREIGN-SLOT-VALUE MESSAGES '(:STRUCT WL_MESSAGE)
             'BM-CL-LIBWAYLAND::NAME)
              (FOREIGN-STRING-ALLOC "unset_maximized")
            (FOREIGN-SLOT-VALUE MESSAGES '(:STRUCT WL_MESSAGE)
             'BM-CL-LIBWAYLAND::SIGNATURE)
              (FOREIGN-STRING-ALLOC "")
            (FOREIGN-SLOT-VALUE MESSAGES '(:STRUCT WL_MESSAGE)
             'BM-CL-LIBWAYLAND::TYPES)
              INTERFACE-ARRAY))
    (LET ((INTERFACE-ARRAY
           (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID)) :COUNT 1)))
      (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 0) WL_OUTPUT::*INTERFACE*)
      (SETF (FOREIGN-SLOT-VALUE MESSAGES '(:STRUCT WL_MESSAGE)
             'BM-CL-LIBWAYLAND::NAME)
              (FOREIGN-STRING-ALLOC "set_fullscreen")
            (FOREIGN-SLOT-VALUE MESSAGES '(:STRUCT WL_MESSAGE)
             'BM-CL-LIBWAYLAND::SIGNATURE)
              (FOREIGN-STRING-ALLOC "?o")
            (FOREIGN-SLOT-VALUE MESSAGES '(:STRUCT WL_MESSAGE)
             'BM-CL-LIBWAYLAND::TYPES)
              INTERFACE-ARRAY))
    (LET ((INTERFACE-ARRAY
           (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID)) :COUNT 0)))
      (SETF (FOREIGN-SLOT-VALUE MESSAGES '(:STRUCT WL_MESSAGE)
             'BM-CL-LIBWAYLAND::NAME)
              (FOREIGN-STRING-ALLOC "unset_fullscreen")
            (FOREIGN-SLOT-VALUE MESSAGES '(:STRUCT WL_MESSAGE)
             'BM-CL-LIBWAYLAND::SIGNATURE)
              (FOREIGN-STRING-ALLOC "")
            (FOREIGN-SLOT-VALUE MESSAGES '(:STRUCT WL_MESSAGE)
             'BM-CL-LIBWAYLAND::TYPES)
              INTERFACE-ARRAY))
    (LET ((INTERFACE-ARRAY
           (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID)) :COUNT 0)))
      (SETF (FOREIGN-SLOT-VALUE MESSAGES '(:STRUCT WL_MESSAGE)
             'BM-CL-LIBWAYLAND::NAME)
              (FOREIGN-STRING-ALLOC "set_minimized")
            (FOREIGN-SLOT-VALUE MESSAGES '(:STRUCT WL_MESSAGE)
             'BM-CL-LIBWAYLAND::SIGNATURE)
              (FOREIGN-STRING-ALLOC "")
            (FOREIGN-SLOT-VALUE MESSAGES '(:STRUCT WL_MESSAGE)
             'BM-CL-LIBWAYLAND::TYPES)
              INTERFACE-ARRAY))
    MESSAGES))

(DEFVAR *EVENTS*
  (LET ((MESSAGES (CFFI:FOREIGN-ALLOC '(:STRUCT WL_MESSAGE) :COUNT 4)))
    (LET ((INTERFACE-ARRAY
           (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID)) :COUNT 3)))
      (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 0) (NULL-POINTER))
      (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 1) (NULL-POINTER))
      (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 2) (NULL-POINTER))
      (SETF (FOREIGN-SLOT-VALUE MESSAGES '(:STRUCT WL_MESSAGE)
             'BM-CL-LIBWAYLAND::NAME)
              (FOREIGN-STRING-ALLOC "configure")
            (FOREIGN-SLOT-VALUE MESSAGES '(:STRUCT WL_MESSAGE)
             'BM-CL-LIBWAYLAND::SIGNATURE)
              (FOREIGN-STRING-ALLOC "iia")
            (FOREIGN-SLOT-VALUE MESSAGES '(:STRUCT WL_MESSAGE)
             'BM-CL-LIBWAYLAND::TYPES)
              INTERFACE-ARRAY))
    (LET ((INTERFACE-ARRAY
           (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID)) :COUNT 0)))
      (SETF (FOREIGN-SLOT-VALUE MESSAGES '(:STRUCT WL_MESSAGE)
             'BM-CL-LIBWAYLAND::NAME)
              (FOREIGN-STRING-ALLOC "close")
            (FOREIGN-SLOT-VALUE MESSAGES '(:STRUCT WL_MESSAGE)
             'BM-CL-LIBWAYLAND::SIGNATURE)
              (FOREIGN-STRING-ALLOC "")
            (FOREIGN-SLOT-VALUE MESSAGES '(:STRUCT WL_MESSAGE)
             'BM-CL-LIBWAYLAND::TYPES)
              INTERFACE-ARRAY))
    (LET ((INTERFACE-ARRAY
           (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID)) :COUNT 2)))
      (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 0) (NULL-POINTER))
      (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 1) (NULL-POINTER))
      (SETF (FOREIGN-SLOT-VALUE MESSAGES '(:STRUCT WL_MESSAGE)
             'BM-CL-LIBWAYLAND::NAME)
              (FOREIGN-STRING-ALLOC "configure_bounds")
            (FOREIGN-SLOT-VALUE MESSAGES '(:STRUCT WL_MESSAGE)
             'BM-CL-LIBWAYLAND::SIGNATURE)
              (FOREIGN-STRING-ALLOC "4ii")
            (FOREIGN-SLOT-VALUE MESSAGES '(:STRUCT WL_MESSAGE)
             'BM-CL-LIBWAYLAND::TYPES)
              INTERFACE-ARRAY))
    (LET ((INTERFACE-ARRAY
           (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID)) :COUNT 1)))
      (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 0) (NULL-POINTER))
      (SETF (FOREIGN-SLOT-VALUE MESSAGES '(:STRUCT WL_MESSAGE)
             'BM-CL-LIBWAYLAND::NAME)
              (FOREIGN-STRING-ALLOC "wm_capabilities")
            (FOREIGN-SLOT-VALUE MESSAGES '(:STRUCT WL_MESSAGE)
             'BM-CL-LIBWAYLAND::SIGNATURE)
              (FOREIGN-STRING-ALLOC "5a")
            (FOREIGN-SLOT-VALUE MESSAGES '(:STRUCT WL_MESSAGE)
             'BM-CL-LIBWAYLAND::TYPES)
              INTERFACE-ARRAY))
    MESSAGES))

(SETF (FOREIGN-SLOT-VALUE *INTERFACE* '(:STRUCT INTERFACE) 'NAME)
        (FOREIGN-STRING-ALLOC "xdg_toplevel")
      (FOREIGN-SLOT-VALUE *INTERFACE* '(:STRUCT INTERFACE) 'VERSION) 6
      (FOREIGN-SLOT-VALUE *INTERFACE* '(:STRUCT INTERFACE) 'METHOD_COUNT) 14
      (FOREIGN-SLOT-VALUE *INTERFACE* '(:STRUCT INTERFACE) 'METHODS) *REQUESTS*
      (FOREIGN-SLOT-VALUE *INTERFACE* '(:STRUCT INTERFACE) 'EVENT_COUNT) 4
      (FOREIGN-SLOT-VALUE *INTERFACE* '(:STRUCT INTERFACE) 'EVENTS) *EVENTS*)

(DEFMETHOD DISPATCHER ((DISPATCH DISPATCH) TARGET OPCODE MESSAGE ARGS)
  (DEBUG-LOG! "Dispatching ~a~%" "xdg_toplevel")
  (DEBUG-LOG! "With arguments target: ~a, opcode: ~a, message: ~a, args: ~a~%"
   TARGET OPCODE MESSAGE ARGS)
  (ERROR "DISPATCHER NOT IMPLEMENTED"))

(CL-ASYNC-UTIL:DEFINE-C-CALLBACK DISPATCHER-FFI
    :VOID
    ((DATA :POINTER) (TARGET :POINTER) (OPCODE :UINT) (MESSAGE :POINTER)
     (ARGS :POINTER))
  (LET ((RESOURCE (POP-DATA DATA)))
    (DISPATCHER RESOURCE TARGET OPCODE MESSAGE ARGS)
    (ERROR "C DISPATCHER MAYBE NOT IMPLEMENTED")))

(DEFVAR *DISPATCHER* (CALLBACK DISPATCHER-FFI))

(DEFCLASS GLOBAL (BM-CL-WAYLAND::GLOBAL) NIL (:DEFAULT-INITARGS :VERSION 6)
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
                        :CLIENT CLIENT)))
    (SETF (IFACE CLIENT ID) BOUND)
    (LET ((RESOURCE (CREATE-RESOURCE (PTR CLIENT) *INTERFACE* VERSION ID))
          (NEXT-DATA-ID (RESERVE-DATA BOUND)))
      (RESOURCE-SET-DISPATCHER RESOURCE *DISPATCHER* (NULL-POINTER)
       (DATA-PTR NEXT-DATA-ID) (NULL-POINTER)))))

(CL-ASYNC-UTIL:DEFINE-C-CALLBACK DISPATCH-BIND-FFI
    :VOID
    ((CLIENT :POINTER) (DATA :POINTER) (VERSION :UINT) (ID :UINT))
  (DEBUG-LOG! "C-Binding ~a~%" "xdg_toplevel")
  (LET* ((CLIENT (GET-CLIENT CLIENT)) (GLOBAL (POP-DATA DATA)))
    (FUNCALL 'DISPATCH-BIND GLOBAL CLIENT (NULL-POINTER) VERSION ID)))

(DEFVAR *DISPATCH-BIND* (CALLBACK DISPATCH-BIND-FFI))

(DEFMETHOD INITIALIZE-INSTANCE :AFTER ((GLOBAL GLOBAL) &KEY)
  (DEBUG-LOG! "Initializing global object: ~a~%" "xdg_toplevel")
  (LET* ((NEXT-DATA-ID (RESERVE-DATA))
         (GLOBAL-PTR
          (GLOBAL-CREATE (DISPLAY GLOBAL) *INTERFACE* (VERSION GLOBAL)
           (DATA-PTR NEXT-DATA-ID) *DISPATCH-BIND*)))
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
    (RESOURCE CLIENT))

(DEFGENERIC GRAB
    (RESOURCE CLIENT SEAT SERIAL))

(DEFGENERIC REPOSITION
    (RESOURCE CLIENT POSITIONER TOKEN))

(DEFVAR *REQUESTS*
  (LET ((MESSAGES (CFFI:FOREIGN-ALLOC '(:STRUCT WL_MESSAGE) :COUNT 3)))
    (LET ((INTERFACE-ARRAY
           (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID)) :COUNT 0)))
      (SETF (FOREIGN-SLOT-VALUE MESSAGES '(:STRUCT WL_MESSAGE)
             'BM-CL-LIBWAYLAND::NAME)
              (FOREIGN-STRING-ALLOC "destroy")
            (FOREIGN-SLOT-VALUE MESSAGES '(:STRUCT WL_MESSAGE)
             'BM-CL-LIBWAYLAND::SIGNATURE)
              (FOREIGN-STRING-ALLOC "")
            (FOREIGN-SLOT-VALUE MESSAGES '(:STRUCT WL_MESSAGE)
             'BM-CL-LIBWAYLAND::TYPES)
              INTERFACE-ARRAY))
    (LET ((INTERFACE-ARRAY
           (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID)) :COUNT 2)))
      (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 0) WL_SEAT::*INTERFACE*)
      (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 1) (NULL-POINTER))
      (SETF (FOREIGN-SLOT-VALUE MESSAGES '(:STRUCT WL_MESSAGE)
             'BM-CL-LIBWAYLAND::NAME)
              (FOREIGN-STRING-ALLOC "grab")
            (FOREIGN-SLOT-VALUE MESSAGES '(:STRUCT WL_MESSAGE)
             'BM-CL-LIBWAYLAND::SIGNATURE)
              (FOREIGN-STRING-ALLOC "ou")
            (FOREIGN-SLOT-VALUE MESSAGES '(:STRUCT WL_MESSAGE)
             'BM-CL-LIBWAYLAND::TYPES)
              INTERFACE-ARRAY))
    (LET ((INTERFACE-ARRAY
           (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID)) :COUNT 2)))
      (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 0) XDG_POSITIONER::*INTERFACE*)
      (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 1) (NULL-POINTER))
      (SETF (FOREIGN-SLOT-VALUE MESSAGES '(:STRUCT WL_MESSAGE)
             'BM-CL-LIBWAYLAND::NAME)
              (FOREIGN-STRING-ALLOC "reposition")
            (FOREIGN-SLOT-VALUE MESSAGES '(:STRUCT WL_MESSAGE)
             'BM-CL-LIBWAYLAND::SIGNATURE)
              (FOREIGN-STRING-ALLOC "3ou")
            (FOREIGN-SLOT-VALUE MESSAGES '(:STRUCT WL_MESSAGE)
             'BM-CL-LIBWAYLAND::TYPES)
              INTERFACE-ARRAY))
    MESSAGES))

(DEFVAR *EVENTS*
  (LET ((MESSAGES (CFFI:FOREIGN-ALLOC '(:STRUCT WL_MESSAGE) :COUNT 3)))
    (LET ((INTERFACE-ARRAY
           (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID)) :COUNT 4)))
      (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 0) (NULL-POINTER))
      (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 1) (NULL-POINTER))
      (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 2) (NULL-POINTER))
      (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 3) (NULL-POINTER))
      (SETF (FOREIGN-SLOT-VALUE MESSAGES '(:STRUCT WL_MESSAGE)
             'BM-CL-LIBWAYLAND::NAME)
              (FOREIGN-STRING-ALLOC "configure")
            (FOREIGN-SLOT-VALUE MESSAGES '(:STRUCT WL_MESSAGE)
             'BM-CL-LIBWAYLAND::SIGNATURE)
              (FOREIGN-STRING-ALLOC "iiii")
            (FOREIGN-SLOT-VALUE MESSAGES '(:STRUCT WL_MESSAGE)
             'BM-CL-LIBWAYLAND::TYPES)
              INTERFACE-ARRAY))
    (LET ((INTERFACE-ARRAY
           (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID)) :COUNT 0)))
      (SETF (FOREIGN-SLOT-VALUE MESSAGES '(:STRUCT WL_MESSAGE)
             'BM-CL-LIBWAYLAND::NAME)
              (FOREIGN-STRING-ALLOC "popup_done")
            (FOREIGN-SLOT-VALUE MESSAGES '(:STRUCT WL_MESSAGE)
             'BM-CL-LIBWAYLAND::SIGNATURE)
              (FOREIGN-STRING-ALLOC "")
            (FOREIGN-SLOT-VALUE MESSAGES '(:STRUCT WL_MESSAGE)
             'BM-CL-LIBWAYLAND::TYPES)
              INTERFACE-ARRAY))
    (LET ((INTERFACE-ARRAY
           (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID)) :COUNT 1)))
      (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 0) (NULL-POINTER))
      (SETF (FOREIGN-SLOT-VALUE MESSAGES '(:STRUCT WL_MESSAGE)
             'BM-CL-LIBWAYLAND::NAME)
              (FOREIGN-STRING-ALLOC "repositioned")
            (FOREIGN-SLOT-VALUE MESSAGES '(:STRUCT WL_MESSAGE)
             'BM-CL-LIBWAYLAND::SIGNATURE)
              (FOREIGN-STRING-ALLOC "3u")
            (FOREIGN-SLOT-VALUE MESSAGES '(:STRUCT WL_MESSAGE)
             'BM-CL-LIBWAYLAND::TYPES)
              INTERFACE-ARRAY))
    MESSAGES))

(SETF (FOREIGN-SLOT-VALUE *INTERFACE* '(:STRUCT INTERFACE) 'NAME)
        (FOREIGN-STRING-ALLOC "xdg_popup")
      (FOREIGN-SLOT-VALUE *INTERFACE* '(:STRUCT INTERFACE) 'VERSION) 6
      (FOREIGN-SLOT-VALUE *INTERFACE* '(:STRUCT INTERFACE) 'METHOD_COUNT) 3
      (FOREIGN-SLOT-VALUE *INTERFACE* '(:STRUCT INTERFACE) 'METHODS) *REQUESTS*
      (FOREIGN-SLOT-VALUE *INTERFACE* '(:STRUCT INTERFACE) 'EVENT_COUNT) 3
      (FOREIGN-SLOT-VALUE *INTERFACE* '(:STRUCT INTERFACE) 'EVENTS) *EVENTS*)

(DEFMETHOD DISPATCHER ((DISPATCH DISPATCH) TARGET OPCODE MESSAGE ARGS)
  (DEBUG-LOG! "Dispatching ~a~%" "xdg_popup")
  (DEBUG-LOG! "With arguments target: ~a, opcode: ~a, message: ~a, args: ~a~%"
   TARGET OPCODE MESSAGE ARGS)
  (ERROR "DISPATCHER NOT IMPLEMENTED"))

(CL-ASYNC-UTIL:DEFINE-C-CALLBACK DISPATCHER-FFI
    :VOID
    ((DATA :POINTER) (TARGET :POINTER) (OPCODE :UINT) (MESSAGE :POINTER)
     (ARGS :POINTER))
  (LET ((RESOURCE (POP-DATA DATA)))
    (DISPATCHER RESOURCE TARGET OPCODE MESSAGE ARGS)
    (ERROR "C DISPATCHER MAYBE NOT IMPLEMENTED")))

(DEFVAR *DISPATCHER* (CALLBACK DISPATCHER-FFI))

(DEFCLASS GLOBAL (BM-CL-WAYLAND::GLOBAL) NIL (:DEFAULT-INITARGS :VERSION 6)
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
                        :CLIENT CLIENT)))
    (SETF (IFACE CLIENT ID) BOUND)
    (LET ((RESOURCE (CREATE-RESOURCE (PTR CLIENT) *INTERFACE* VERSION ID))
          (NEXT-DATA-ID (RESERVE-DATA BOUND)))
      (RESOURCE-SET-DISPATCHER RESOURCE *DISPATCHER* (NULL-POINTER)
       (DATA-PTR NEXT-DATA-ID) (NULL-POINTER)))))

(CL-ASYNC-UTIL:DEFINE-C-CALLBACK DISPATCH-BIND-FFI
    :VOID
    ((CLIENT :POINTER) (DATA :POINTER) (VERSION :UINT) (ID :UINT))
  (DEBUG-LOG! "C-Binding ~a~%" "xdg_popup")
  (LET* ((CLIENT (GET-CLIENT CLIENT)) (GLOBAL (POP-DATA DATA)))
    (FUNCALL 'DISPATCH-BIND GLOBAL CLIENT (NULL-POINTER) VERSION ID)))

(DEFVAR *DISPATCH-BIND* (CALLBACK DISPATCH-BIND-FFI))

(DEFMETHOD INITIALIZE-INSTANCE :AFTER ((GLOBAL GLOBAL) &KEY)
  (DEBUG-LOG! "Initializing global object: ~a~%" "xdg_popup")
  (LET* ((NEXT-DATA-ID (RESERVE-DATA))
         (GLOBAL-PTR
          (GLOBAL-CREATE (DISPLAY GLOBAL) *INTERFACE* (VERSION GLOBAL)
           (DATA-PTR NEXT-DATA-ID) *DISPATCH-BIND*)))
    (SET-DATA NEXT-DATA-ID
     (SETF (GETHASH (POINTER-ADDRESS GLOBAL-PTR) *GLOBAL-TRACKER*) GLOBAL))))

