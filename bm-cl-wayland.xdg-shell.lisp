(DEFPACKAGE :XDG/XDG_WM_BASE
  (:USE :CL :WL :CFFI)
  (:NICKNAMES :XDG-WM-BASE)
  (:EXPORT DISPATCH GLOBAL))

(IN-PACKAGE :XDG/XDG_WM_BASE)

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

(CL-ASYNC-UTIL:DEFINE-C-CALLBACK DESTROY-FFI
    :VOID
    ((CLIENT :POINTER) (RESOURCE :POINTER))
  (LET ((CLIENT (GET-CLIENT CLIENT)) (RESOURCE (RESOURCE-GET-ID RESOURCE)))
    (FUNCALL 'DESTROY (IFACE CLIENT RESOURCE) CLIENT)))

(CL-ASYNC-UTIL:DEFINE-C-CALLBACK CREATE_POSITIONER-FFI
    :VOID
    ((CLIENT :POINTER) (RESOURCE :POINTER) (ID :UINT))
  (LET ((CLIENT (GET-CLIENT CLIENT)) (RESOURCE (RESOURCE-GET-ID RESOURCE)))
    (FUNCALL 'CREATE_POSITIONER (IFACE CLIENT RESOURCE) CLIENT ID)))

(CL-ASYNC-UTIL:DEFINE-C-CALLBACK GET_XDG_SURFACE-FFI
    :VOID
    ((CLIENT :POINTER) (RESOURCE :POINTER) (ID :UINT) (SURFACE :UINT))
  (LET ((CLIENT (GET-CLIENT CLIENT)) (RESOURCE (RESOURCE-GET-ID RESOURCE)))
    (FUNCALL 'GET_XDG_SURFACE (IFACE CLIENT RESOURCE) CLIENT ID SURFACE)))

(CL-ASYNC-UTIL:DEFINE-C-CALLBACK PONG-FFI
    :VOID
    ((CLIENT :POINTER) (RESOURCE :POINTER) (SERIAL :UINT))
  (LET ((CLIENT (GET-CLIENT CLIENT)) (RESOURCE (RESOURCE-GET-ID RESOURCE)))
    (FUNCALL 'PONG (IFACE CLIENT RESOURCE) CLIENT SERIAL)))

(DEFCSTRUCT INTERFACE (DESTROY :POINTER) (CREATE_POSITIONER :POINTER)
 (GET_XDG_SURFACE :POINTER) (PONG :POINTER))

(DEFVAR *INTERFACE*
  (WITH-FOREIGN-OBJECT (INTERFACE 'INTERFACE)
   (SETF (FOREIGN-SLOT-VALUE INTERFACE '(:STRUCT INTERFACE) 'DESTROY)
           (CALLBACK DESTROY-FFI))
   (SETF (FOREIGN-SLOT-VALUE INTERFACE '(:STRUCT INTERFACE) 'CREATE_POSITIONER)
           (CALLBACK CREATE_POSITIONER-FFI))
   (SETF (FOREIGN-SLOT-VALUE INTERFACE '(:STRUCT INTERFACE) 'GET_XDG_SURFACE)
           (CALLBACK GET_XDG_SURFACE-FFI))
   (SETF (FOREIGN-SLOT-VALUE INTERFACE '(:STRUCT INTERFACE) 'PONG)
           (CALLBACK PONG-FFI))
   INTERFACE))

(DEFCLASS GLOBAL (BM-CL-WAYLAND::GLOBAL) NIL (:DEFAULT-INITARGS :VERSION 6)
          (:DOCUMENTATION "create desktop-style surfaces

The xdg_wm_base interface is exposed as a global object enabling clients
      to turn their wl_surfaces into windows in a desktop environment. It
      defines the basic functionality needed for clients and the compositor to
      create windows that can be dragged, resized, maximized, etc, as well as
      creating transient windows such as popup menus.
"))

(DEFMETHOD DISPATCH-BIND ((GLOBAL GLOBAL) CLIENT DATA VERSION ID)
  "Default bind implementation for the ,(name interface) global object.
This can be overriden by inheritance in case if custom behaviour is required."
  (LET ((BOUND (MAKE-INSTANCE 'DISPATCH :DISPLAY (DISPLAY CLIENT))))
    (SETF (IFACE CLIENT ID) BOUND)
    (CREATE-RESOURCE CLIENT *INTERFACE* VERSION ID)))

(CL-ASYNC-UTIL:DEFINE-C-CALLBACK DISPATCH-BIND-FFI
    :VOID
    ((CLIENT :POINTER) (DATA :POINTER) (VERSION :UINT) (ID :UINT))
  (LET* ((CLIENT (GET-CLIENT CLIENT))
         (DATA (POP-DATA DATA))
         (GLOBAL (GETHASH DATA *GLOBAL-TRACKER*)))
    (FUNCALL 'DISPATCH-BIND GLOBAL CLIENT (NULL-POINTER)
             (MEM-REF VERSION :UINT) (MEM-REF ID :UINT))))

(DEFVAR *DISPATCH-BIND* (CALLBACK DISPATCH-BIND-FFI))

(DEFMETHOD INITIALIZE-INSTANCE :AFTER ((GLOBAL GLOBAL) &KEY)
  (LET* ((NEXT-DATA-ID (RESERVE-DATA))
         (GLOBAL-PTR
          (GLOBAL-CREATE (DISPLAY GLOBAL) *INTERFACE* (VERSION GLOBAL)
           (DATA-PTR NEXT-DATA-ID) *DISPATCH-BIND*)))
    (SET-DATA NEXT-DATA-ID
     (SETF (GETHASH (GLOBAL-GET-NAME GLOBAL-PTR (NULL-POINTER))
                    *GLOBAL-TRACKER*)
             GLOBAL))))

(DEFPACKAGE :XDG/XDG_POSITIONER
  (:USE :CL :WL :CFFI)
  (:NICKNAMES :XDG-POSITIONER)
  (:EXPORT DISPATCH GLOBAL))

(IN-PACKAGE :XDG/XDG_POSITIONER)

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

(CL-ASYNC-UTIL:DEFINE-C-CALLBACK DESTROY-FFI
    :VOID
    ((CLIENT :POINTER) (RESOURCE :POINTER))
  (LET ((CLIENT (GET-CLIENT CLIENT)) (RESOURCE (RESOURCE-GET-ID RESOURCE)))
    (FUNCALL 'DESTROY (IFACE CLIENT RESOURCE) CLIENT)))

(CL-ASYNC-UTIL:DEFINE-C-CALLBACK SET_SIZE-FFI
    :VOID
    ((CLIENT :POINTER) (RESOURCE :POINTER) (WIDTH :INT) (HEIGHT :INT))
  (LET ((CLIENT (GET-CLIENT CLIENT)) (RESOURCE (RESOURCE-GET-ID RESOURCE)))
    (FUNCALL 'SET_SIZE (IFACE CLIENT RESOURCE) CLIENT WIDTH HEIGHT)))

(CL-ASYNC-UTIL:DEFINE-C-CALLBACK SET_ANCHOR_RECT-FFI
    :VOID
    ((CLIENT :POINTER) (RESOURCE :POINTER) (X :INT) (Y :INT) (WIDTH :INT)
     (HEIGHT :INT))
  (LET ((CLIENT (GET-CLIENT CLIENT)) (RESOURCE (RESOURCE-GET-ID RESOURCE)))
    (FUNCALL 'SET_ANCHOR_RECT (IFACE CLIENT RESOURCE) CLIENT X Y WIDTH HEIGHT)))

(CL-ASYNC-UTIL:DEFINE-C-CALLBACK SET_ANCHOR-FFI
    :VOID
    ((CLIENT :POINTER) (RESOURCE :POINTER) (ANCHOR :UINT))
  (LET ((CLIENT (GET-CLIENT CLIENT)) (RESOURCE (RESOURCE-GET-ID RESOURCE)))
    (FUNCALL 'SET_ANCHOR (IFACE CLIENT RESOURCE) CLIENT ANCHOR)))

(CL-ASYNC-UTIL:DEFINE-C-CALLBACK SET_GRAVITY-FFI
    :VOID
    ((CLIENT :POINTER) (RESOURCE :POINTER) (GRAVITY :UINT))
  (LET ((CLIENT (GET-CLIENT CLIENT)) (RESOURCE (RESOURCE-GET-ID RESOURCE)))
    (FUNCALL 'SET_GRAVITY (IFACE CLIENT RESOURCE) CLIENT GRAVITY)))

(CL-ASYNC-UTIL:DEFINE-C-CALLBACK SET_CONSTRAINT_ADJUSTMENT-FFI
    :VOID
    ((CLIENT :POINTER) (RESOURCE :POINTER) (CONSTRAINT_ADJUSTMENT :UINT))
  (LET ((CLIENT (GET-CLIENT CLIENT)) (RESOURCE (RESOURCE-GET-ID RESOURCE)))
    (FUNCALL 'SET_CONSTRAINT_ADJUSTMENT (IFACE CLIENT RESOURCE) CLIENT
             CONSTRAINT_ADJUSTMENT)))

(CL-ASYNC-UTIL:DEFINE-C-CALLBACK SET_OFFSET-FFI
    :VOID
    ((CLIENT :POINTER) (RESOURCE :POINTER) (X :INT) (Y :INT))
  (LET ((CLIENT (GET-CLIENT CLIENT)) (RESOURCE (RESOURCE-GET-ID RESOURCE)))
    (FUNCALL 'SET_OFFSET (IFACE CLIENT RESOURCE) CLIENT X Y)))

(CL-ASYNC-UTIL:DEFINE-C-CALLBACK SET_REACTIVE-FFI
    :VOID
    ((CLIENT :POINTER) (RESOURCE :POINTER))
  (LET ((CLIENT (GET-CLIENT CLIENT)) (RESOURCE (RESOURCE-GET-ID RESOURCE)))
    (FUNCALL 'SET_REACTIVE (IFACE CLIENT RESOURCE) CLIENT)))

(CL-ASYNC-UTIL:DEFINE-C-CALLBACK SET_PARENT_SIZE-FFI
    :VOID
    ((CLIENT :POINTER) (RESOURCE :POINTER) (PARENT_WIDTH :INT)
     (PARENT_HEIGHT :INT))
  (LET ((CLIENT (GET-CLIENT CLIENT)) (RESOURCE (RESOURCE-GET-ID RESOURCE)))
    (FUNCALL 'SET_PARENT_SIZE (IFACE CLIENT RESOURCE) CLIENT PARENT_WIDTH
             PARENT_HEIGHT)))

(CL-ASYNC-UTIL:DEFINE-C-CALLBACK SET_PARENT_CONFIGURE-FFI
    :VOID
    ((CLIENT :POINTER) (RESOURCE :POINTER) (SERIAL :UINT))
  (LET ((CLIENT (GET-CLIENT CLIENT)) (RESOURCE (RESOURCE-GET-ID RESOURCE)))
    (FUNCALL 'SET_PARENT_CONFIGURE (IFACE CLIENT RESOURCE) CLIENT SERIAL)))

(DEFCSTRUCT INTERFACE (DESTROY :POINTER) (SET_SIZE :POINTER)
 (SET_ANCHOR_RECT :POINTER) (SET_ANCHOR :POINTER) (SET_GRAVITY :POINTER)
 (SET_CONSTRAINT_ADJUSTMENT :POINTER) (SET_OFFSET :POINTER)
 (SET_REACTIVE :POINTER) (SET_PARENT_SIZE :POINTER)
 (SET_PARENT_CONFIGURE :POINTER))

(DEFVAR *INTERFACE*
  (WITH-FOREIGN-OBJECT (INTERFACE 'INTERFACE)
   (SETF (FOREIGN-SLOT-VALUE INTERFACE '(:STRUCT INTERFACE) 'DESTROY)
           (CALLBACK DESTROY-FFI))
   (SETF (FOREIGN-SLOT-VALUE INTERFACE '(:STRUCT INTERFACE) 'SET_SIZE)
           (CALLBACK SET_SIZE-FFI))
   (SETF (FOREIGN-SLOT-VALUE INTERFACE '(:STRUCT INTERFACE) 'SET_ANCHOR_RECT)
           (CALLBACK SET_ANCHOR_RECT-FFI))
   (SETF (FOREIGN-SLOT-VALUE INTERFACE '(:STRUCT INTERFACE) 'SET_ANCHOR)
           (CALLBACK SET_ANCHOR-FFI))
   (SETF (FOREIGN-SLOT-VALUE INTERFACE '(:STRUCT INTERFACE) 'SET_GRAVITY)
           (CALLBACK SET_GRAVITY-FFI))
   (SETF (FOREIGN-SLOT-VALUE INTERFACE '(:STRUCT INTERFACE)
          'SET_CONSTRAINT_ADJUSTMENT)
           (CALLBACK SET_CONSTRAINT_ADJUSTMENT-FFI))
   (SETF (FOREIGN-SLOT-VALUE INTERFACE '(:STRUCT INTERFACE) 'SET_OFFSET)
           (CALLBACK SET_OFFSET-FFI))
   (SETF (FOREIGN-SLOT-VALUE INTERFACE '(:STRUCT INTERFACE) 'SET_REACTIVE)
           (CALLBACK SET_REACTIVE-FFI))
   (SETF (FOREIGN-SLOT-VALUE INTERFACE '(:STRUCT INTERFACE) 'SET_PARENT_SIZE)
           (CALLBACK SET_PARENT_SIZE-FFI))
   (SETF (FOREIGN-SLOT-VALUE INTERFACE '(:STRUCT INTERFACE)
          'SET_PARENT_CONFIGURE)
           (CALLBACK SET_PARENT_CONFIGURE-FFI))
   INTERFACE))

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
  "Default bind implementation for the ,(name interface) global object.
This can be overriden by inheritance in case if custom behaviour is required."
  (LET ((BOUND (MAKE-INSTANCE 'DISPATCH :DISPLAY (DISPLAY CLIENT))))
    (SETF (IFACE CLIENT ID) BOUND)
    (CREATE-RESOURCE CLIENT *INTERFACE* VERSION ID)))

(CL-ASYNC-UTIL:DEFINE-C-CALLBACK DISPATCH-BIND-FFI
    :VOID
    ((CLIENT :POINTER) (DATA :POINTER) (VERSION :UINT) (ID :UINT))
  (LET* ((CLIENT (GET-CLIENT CLIENT))
         (DATA (POP-DATA DATA))
         (GLOBAL (GETHASH DATA *GLOBAL-TRACKER*)))
    (FUNCALL 'DISPATCH-BIND GLOBAL CLIENT (NULL-POINTER)
             (MEM-REF VERSION :UINT) (MEM-REF ID :UINT))))

(DEFVAR *DISPATCH-BIND* (CALLBACK DISPATCH-BIND-FFI))

(DEFMETHOD INITIALIZE-INSTANCE :AFTER ((GLOBAL GLOBAL) &KEY)
  (LET* ((NEXT-DATA-ID (RESERVE-DATA))
         (GLOBAL-PTR
          (GLOBAL-CREATE (DISPLAY GLOBAL) *INTERFACE* (VERSION GLOBAL)
           (DATA-PTR NEXT-DATA-ID) *DISPATCH-BIND*)))
    (SET-DATA NEXT-DATA-ID
     (SETF (GETHASH (GLOBAL-GET-NAME GLOBAL-PTR (NULL-POINTER))
                    *GLOBAL-TRACKER*)
             GLOBAL))))

(DEFPACKAGE :XDG/XDG_SURFACE
  (:USE :CL :WL :CFFI)
  (:NICKNAMES :XDG-SURFACE)
  (:EXPORT DISPATCH GLOBAL))

(IN-PACKAGE :XDG/XDG_SURFACE)

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

(CL-ASYNC-UTIL:DEFINE-C-CALLBACK DESTROY-FFI
    :VOID
    ((CLIENT :POINTER) (RESOURCE :POINTER))
  (LET ((CLIENT (GET-CLIENT CLIENT)) (RESOURCE (RESOURCE-GET-ID RESOURCE)))
    (FUNCALL 'DESTROY (IFACE CLIENT RESOURCE) CLIENT)))

(CL-ASYNC-UTIL:DEFINE-C-CALLBACK GET_TOPLEVEL-FFI
    :VOID
    ((CLIENT :POINTER) (RESOURCE :POINTER) (ID :UINT))
  (LET ((CLIENT (GET-CLIENT CLIENT)) (RESOURCE (RESOURCE-GET-ID RESOURCE)))
    (FUNCALL 'GET_TOPLEVEL (IFACE CLIENT RESOURCE) CLIENT ID)))

(CL-ASYNC-UTIL:DEFINE-C-CALLBACK GET_POPUP-FFI
    :VOID
    ((CLIENT :POINTER) (RESOURCE :POINTER) (ID :UINT) (PARENT :UINT)
     (POSITIONER :UINT))
  (LET ((CLIENT (GET-CLIENT CLIENT)) (RESOURCE (RESOURCE-GET-ID RESOURCE)))
    (FUNCALL 'GET_POPUP (IFACE CLIENT RESOURCE) CLIENT ID PARENT POSITIONER)))

(CL-ASYNC-UTIL:DEFINE-C-CALLBACK SET_WINDOW_GEOMETRY-FFI
    :VOID
    ((CLIENT :POINTER) (RESOURCE :POINTER) (X :INT) (Y :INT) (WIDTH :INT)
     (HEIGHT :INT))
  (LET ((CLIENT (GET-CLIENT CLIENT)) (RESOURCE (RESOURCE-GET-ID RESOURCE)))
    (FUNCALL 'SET_WINDOW_GEOMETRY (IFACE CLIENT RESOURCE) CLIENT X Y WIDTH
             HEIGHT)))

(CL-ASYNC-UTIL:DEFINE-C-CALLBACK ACK_CONFIGURE-FFI
    :VOID
    ((CLIENT :POINTER) (RESOURCE :POINTER) (SERIAL :UINT))
  (LET ((CLIENT (GET-CLIENT CLIENT)) (RESOURCE (RESOURCE-GET-ID RESOURCE)))
    (FUNCALL 'ACK_CONFIGURE (IFACE CLIENT RESOURCE) CLIENT SERIAL)))

(DEFCSTRUCT INTERFACE (DESTROY :POINTER) (GET_TOPLEVEL :POINTER)
 (GET_POPUP :POINTER) (SET_WINDOW_GEOMETRY :POINTER) (ACK_CONFIGURE :POINTER))

(DEFVAR *INTERFACE*
  (WITH-FOREIGN-OBJECT (INTERFACE 'INTERFACE)
   (SETF (FOREIGN-SLOT-VALUE INTERFACE '(:STRUCT INTERFACE) 'DESTROY)
           (CALLBACK DESTROY-FFI))
   (SETF (FOREIGN-SLOT-VALUE INTERFACE '(:STRUCT INTERFACE) 'GET_TOPLEVEL)
           (CALLBACK GET_TOPLEVEL-FFI))
   (SETF (FOREIGN-SLOT-VALUE INTERFACE '(:STRUCT INTERFACE) 'GET_POPUP)
           (CALLBACK GET_POPUP-FFI))
   (SETF (FOREIGN-SLOT-VALUE INTERFACE '(:STRUCT INTERFACE)
          'SET_WINDOW_GEOMETRY)
           (CALLBACK SET_WINDOW_GEOMETRY-FFI))
   (SETF (FOREIGN-SLOT-VALUE INTERFACE '(:STRUCT INTERFACE) 'ACK_CONFIGURE)
           (CALLBACK ACK_CONFIGURE-FFI))
   INTERFACE))

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
  "Default bind implementation for the ,(name interface) global object.
This can be overriden by inheritance in case if custom behaviour is required."
  (LET ((BOUND (MAKE-INSTANCE 'DISPATCH :DISPLAY (DISPLAY CLIENT))))
    (SETF (IFACE CLIENT ID) BOUND)
    (CREATE-RESOURCE CLIENT *INTERFACE* VERSION ID)))

(CL-ASYNC-UTIL:DEFINE-C-CALLBACK DISPATCH-BIND-FFI
    :VOID
    ((CLIENT :POINTER) (DATA :POINTER) (VERSION :UINT) (ID :UINT))
  (LET* ((CLIENT (GET-CLIENT CLIENT))
         (DATA (POP-DATA DATA))
         (GLOBAL (GETHASH DATA *GLOBAL-TRACKER*)))
    (FUNCALL 'DISPATCH-BIND GLOBAL CLIENT (NULL-POINTER)
             (MEM-REF VERSION :UINT) (MEM-REF ID :UINT))))

(DEFVAR *DISPATCH-BIND* (CALLBACK DISPATCH-BIND-FFI))

(DEFMETHOD INITIALIZE-INSTANCE :AFTER ((GLOBAL GLOBAL) &KEY)
  (LET* ((NEXT-DATA-ID (RESERVE-DATA))
         (GLOBAL-PTR
          (GLOBAL-CREATE (DISPLAY GLOBAL) *INTERFACE* (VERSION GLOBAL)
           (DATA-PTR NEXT-DATA-ID) *DISPATCH-BIND*)))
    (SET-DATA NEXT-DATA-ID
     (SETF (GETHASH (GLOBAL-GET-NAME GLOBAL-PTR (NULL-POINTER))
                    *GLOBAL-TRACKER*)
             GLOBAL))))

(DEFPACKAGE :XDG/XDG_TOPLEVEL
  (:USE :CL :WL :CFFI)
  (:NICKNAMES :XDG-TOPLEVEL)
  (:EXPORT DISPATCH GLOBAL))

(IN-PACKAGE :XDG/XDG_TOPLEVEL)

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

(CL-ASYNC-UTIL:DEFINE-C-CALLBACK DESTROY-FFI
    :VOID
    ((CLIENT :POINTER) (RESOURCE :POINTER))
  (LET ((CLIENT (GET-CLIENT CLIENT)) (RESOURCE (RESOURCE-GET-ID RESOURCE)))
    (FUNCALL 'DESTROY (IFACE CLIENT RESOURCE) CLIENT)))

(CL-ASYNC-UTIL:DEFINE-C-CALLBACK SET_PARENT-FFI
    :VOID
    ((CLIENT :POINTER) (RESOURCE :POINTER) (PARENT :UINT))
  (LET ((CLIENT (GET-CLIENT CLIENT)) (RESOURCE (RESOURCE-GET-ID RESOURCE)))
    (FUNCALL 'SET_PARENT (IFACE CLIENT RESOURCE) CLIENT PARENT)))

(CL-ASYNC-UTIL:DEFINE-C-CALLBACK SET_TITLE-FFI
    :VOID
    ((CLIENT :POINTER) (RESOURCE :POINTER) (TITLE (:POINTER :CHAR)))
  (LET ((CLIENT (GET-CLIENT CLIENT)) (RESOURCE (RESOURCE-GET-ID RESOURCE)))
    (FUNCALL 'SET_TITLE (IFACE CLIENT RESOURCE) CLIENT TITLE)))

(CL-ASYNC-UTIL:DEFINE-C-CALLBACK SET_APP_ID-FFI
    :VOID
    ((CLIENT :POINTER) (RESOURCE :POINTER) (APP_ID (:POINTER :CHAR)))
  (LET ((CLIENT (GET-CLIENT CLIENT)) (RESOURCE (RESOURCE-GET-ID RESOURCE)))
    (FUNCALL 'SET_APP_ID (IFACE CLIENT RESOURCE) CLIENT APP_ID)))

(CL-ASYNC-UTIL:DEFINE-C-CALLBACK SHOW_WINDOW_MENU-FFI
    :VOID
    ((CLIENT :POINTER) (RESOURCE :POINTER) (SEAT :UINT) (SERIAL :UINT) (X :INT)
     (Y :INT))
  (LET ((CLIENT (GET-CLIENT CLIENT)) (RESOURCE (RESOURCE-GET-ID RESOURCE)))
    (FUNCALL 'SHOW_WINDOW_MENU (IFACE CLIENT RESOURCE) CLIENT SEAT SERIAL X Y)))

(CL-ASYNC-UTIL:DEFINE-C-CALLBACK MOVE-FFI
    :VOID
    ((CLIENT :POINTER) (RESOURCE :POINTER) (SEAT :UINT) (SERIAL :UINT))
  (LET ((CLIENT (GET-CLIENT CLIENT)) (RESOURCE (RESOURCE-GET-ID RESOURCE)))
    (FUNCALL 'MOVE (IFACE CLIENT RESOURCE) CLIENT SEAT SERIAL)))

(CL-ASYNC-UTIL:DEFINE-C-CALLBACK RESIZE-FFI
    :VOID
    ((CLIENT :POINTER) (RESOURCE :POINTER) (SEAT :UINT) (SERIAL :UINT)
     (EDGES :UINT))
  (LET ((CLIENT (GET-CLIENT CLIENT)) (RESOURCE (RESOURCE-GET-ID RESOURCE)))
    (FUNCALL 'RESIZE (IFACE CLIENT RESOURCE) CLIENT SEAT SERIAL EDGES)))

(CL-ASYNC-UTIL:DEFINE-C-CALLBACK SET_MAX_SIZE-FFI
    :VOID
    ((CLIENT :POINTER) (RESOURCE :POINTER) (WIDTH :INT) (HEIGHT :INT))
  (LET ((CLIENT (GET-CLIENT CLIENT)) (RESOURCE (RESOURCE-GET-ID RESOURCE)))
    (FUNCALL 'SET_MAX_SIZE (IFACE CLIENT RESOURCE) CLIENT WIDTH HEIGHT)))

(CL-ASYNC-UTIL:DEFINE-C-CALLBACK SET_MIN_SIZE-FFI
    :VOID
    ((CLIENT :POINTER) (RESOURCE :POINTER) (WIDTH :INT) (HEIGHT :INT))
  (LET ((CLIENT (GET-CLIENT CLIENT)) (RESOURCE (RESOURCE-GET-ID RESOURCE)))
    (FUNCALL 'SET_MIN_SIZE (IFACE CLIENT RESOURCE) CLIENT WIDTH HEIGHT)))

(CL-ASYNC-UTIL:DEFINE-C-CALLBACK SET_MAXIMIZED-FFI
    :VOID
    ((CLIENT :POINTER) (RESOURCE :POINTER))
  (LET ((CLIENT (GET-CLIENT CLIENT)) (RESOURCE (RESOURCE-GET-ID RESOURCE)))
    (FUNCALL 'SET_MAXIMIZED (IFACE CLIENT RESOURCE) CLIENT)))

(CL-ASYNC-UTIL:DEFINE-C-CALLBACK UNSET_MAXIMIZED-FFI
    :VOID
    ((CLIENT :POINTER) (RESOURCE :POINTER))
  (LET ((CLIENT (GET-CLIENT CLIENT)) (RESOURCE (RESOURCE-GET-ID RESOURCE)))
    (FUNCALL 'UNSET_MAXIMIZED (IFACE CLIENT RESOURCE) CLIENT)))

(CL-ASYNC-UTIL:DEFINE-C-CALLBACK SET_FULLSCREEN-FFI
    :VOID
    ((CLIENT :POINTER) (RESOURCE :POINTER) (OUTPUT :UINT))
  (LET ((CLIENT (GET-CLIENT CLIENT)) (RESOURCE (RESOURCE-GET-ID RESOURCE)))
    (FUNCALL 'SET_FULLSCREEN (IFACE CLIENT RESOURCE) CLIENT OUTPUT)))

(CL-ASYNC-UTIL:DEFINE-C-CALLBACK UNSET_FULLSCREEN-FFI
    :VOID
    ((CLIENT :POINTER) (RESOURCE :POINTER))
  (LET ((CLIENT (GET-CLIENT CLIENT)) (RESOURCE (RESOURCE-GET-ID RESOURCE)))
    (FUNCALL 'UNSET_FULLSCREEN (IFACE CLIENT RESOURCE) CLIENT)))

(CL-ASYNC-UTIL:DEFINE-C-CALLBACK SET_MINIMIZED-FFI
    :VOID
    ((CLIENT :POINTER) (RESOURCE :POINTER))
  (LET ((CLIENT (GET-CLIENT CLIENT)) (RESOURCE (RESOURCE-GET-ID RESOURCE)))
    (FUNCALL 'SET_MINIMIZED (IFACE CLIENT RESOURCE) CLIENT)))

(DEFCSTRUCT INTERFACE (DESTROY :POINTER) (SET_PARENT :POINTER)
 (SET_TITLE :POINTER) (SET_APP_ID :POINTER) (SHOW_WINDOW_MENU :POINTER)
 (MOVE :POINTER) (RESIZE :POINTER) (SET_MAX_SIZE :POINTER)
 (SET_MIN_SIZE :POINTER) (SET_MAXIMIZED :POINTER) (UNSET_MAXIMIZED :POINTER)
 (SET_FULLSCREEN :POINTER) (UNSET_FULLSCREEN :POINTER) (SET_MINIMIZED :POINTER))

(DEFVAR *INTERFACE*
  (WITH-FOREIGN-OBJECT (INTERFACE 'INTERFACE)
   (SETF (FOREIGN-SLOT-VALUE INTERFACE '(:STRUCT INTERFACE) 'DESTROY)
           (CALLBACK DESTROY-FFI))
   (SETF (FOREIGN-SLOT-VALUE INTERFACE '(:STRUCT INTERFACE) 'SET_PARENT)
           (CALLBACK SET_PARENT-FFI))
   (SETF (FOREIGN-SLOT-VALUE INTERFACE '(:STRUCT INTERFACE) 'SET_TITLE)
           (CALLBACK SET_TITLE-FFI))
   (SETF (FOREIGN-SLOT-VALUE INTERFACE '(:STRUCT INTERFACE) 'SET_APP_ID)
           (CALLBACK SET_APP_ID-FFI))
   (SETF (FOREIGN-SLOT-VALUE INTERFACE '(:STRUCT INTERFACE) 'SHOW_WINDOW_MENU)
           (CALLBACK SHOW_WINDOW_MENU-FFI))
   (SETF (FOREIGN-SLOT-VALUE INTERFACE '(:STRUCT INTERFACE) 'MOVE)
           (CALLBACK MOVE-FFI))
   (SETF (FOREIGN-SLOT-VALUE INTERFACE '(:STRUCT INTERFACE) 'RESIZE)
           (CALLBACK RESIZE-FFI))
   (SETF (FOREIGN-SLOT-VALUE INTERFACE '(:STRUCT INTERFACE) 'SET_MAX_SIZE)
           (CALLBACK SET_MAX_SIZE-FFI))
   (SETF (FOREIGN-SLOT-VALUE INTERFACE '(:STRUCT INTERFACE) 'SET_MIN_SIZE)
           (CALLBACK SET_MIN_SIZE-FFI))
   (SETF (FOREIGN-SLOT-VALUE INTERFACE '(:STRUCT INTERFACE) 'SET_MAXIMIZED)
           (CALLBACK SET_MAXIMIZED-FFI))
   (SETF (FOREIGN-SLOT-VALUE INTERFACE '(:STRUCT INTERFACE) 'UNSET_MAXIMIZED)
           (CALLBACK UNSET_MAXIMIZED-FFI))
   (SETF (FOREIGN-SLOT-VALUE INTERFACE '(:STRUCT INTERFACE) 'SET_FULLSCREEN)
           (CALLBACK SET_FULLSCREEN-FFI))
   (SETF (FOREIGN-SLOT-VALUE INTERFACE '(:STRUCT INTERFACE) 'UNSET_FULLSCREEN)
           (CALLBACK UNSET_FULLSCREEN-FFI))
   (SETF (FOREIGN-SLOT-VALUE INTERFACE '(:STRUCT INTERFACE) 'SET_MINIMIZED)
           (CALLBACK SET_MINIMIZED-FFI))
   INTERFACE))

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
  "Default bind implementation for the ,(name interface) global object.
This can be overriden by inheritance in case if custom behaviour is required."
  (LET ((BOUND (MAKE-INSTANCE 'DISPATCH :DISPLAY (DISPLAY CLIENT))))
    (SETF (IFACE CLIENT ID) BOUND)
    (CREATE-RESOURCE CLIENT *INTERFACE* VERSION ID)))

(CL-ASYNC-UTIL:DEFINE-C-CALLBACK DISPATCH-BIND-FFI
    :VOID
    ((CLIENT :POINTER) (DATA :POINTER) (VERSION :UINT) (ID :UINT))
  (LET* ((CLIENT (GET-CLIENT CLIENT))
         (DATA (POP-DATA DATA))
         (GLOBAL (GETHASH DATA *GLOBAL-TRACKER*)))
    (FUNCALL 'DISPATCH-BIND GLOBAL CLIENT (NULL-POINTER)
             (MEM-REF VERSION :UINT) (MEM-REF ID :UINT))))

(DEFVAR *DISPATCH-BIND* (CALLBACK DISPATCH-BIND-FFI))

(DEFMETHOD INITIALIZE-INSTANCE :AFTER ((GLOBAL GLOBAL) &KEY)
  (LET* ((NEXT-DATA-ID (RESERVE-DATA))
         (GLOBAL-PTR
          (GLOBAL-CREATE (DISPLAY GLOBAL) *INTERFACE* (VERSION GLOBAL)
           (DATA-PTR NEXT-DATA-ID) *DISPATCH-BIND*)))
    (SET-DATA NEXT-DATA-ID
     (SETF (GETHASH (GLOBAL-GET-NAME GLOBAL-PTR (NULL-POINTER))
                    *GLOBAL-TRACKER*)
             GLOBAL))))

(DEFPACKAGE :XDG/XDG_POPUP
  (:USE :CL :WL :CFFI)
  (:NICKNAMES :XDG-POPUP)
  (:EXPORT DISPATCH GLOBAL))

(IN-PACKAGE :XDG/XDG_POPUP)

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

(CL-ASYNC-UTIL:DEFINE-C-CALLBACK DESTROY-FFI
    :VOID
    ((CLIENT :POINTER) (RESOURCE :POINTER))
  (LET ((CLIENT (GET-CLIENT CLIENT)) (RESOURCE (RESOURCE-GET-ID RESOURCE)))
    (FUNCALL 'DESTROY (IFACE CLIENT RESOURCE) CLIENT)))

(CL-ASYNC-UTIL:DEFINE-C-CALLBACK GRAB-FFI
    :VOID
    ((CLIENT :POINTER) (RESOURCE :POINTER) (SEAT :UINT) (SERIAL :UINT))
  (LET ((CLIENT (GET-CLIENT CLIENT)) (RESOURCE (RESOURCE-GET-ID RESOURCE)))
    (FUNCALL 'GRAB (IFACE CLIENT RESOURCE) CLIENT SEAT SERIAL)))

(CL-ASYNC-UTIL:DEFINE-C-CALLBACK REPOSITION-FFI
    :VOID
    ((CLIENT :POINTER) (RESOURCE :POINTER) (POSITIONER :UINT) (TOKEN :UINT))
  (LET ((CLIENT (GET-CLIENT CLIENT)) (RESOURCE (RESOURCE-GET-ID RESOURCE)))
    (FUNCALL 'REPOSITION (IFACE CLIENT RESOURCE) CLIENT POSITIONER TOKEN)))

(DEFCSTRUCT INTERFACE (DESTROY :POINTER) (GRAB :POINTER) (REPOSITION :POINTER))

(DEFVAR *INTERFACE*
  (WITH-FOREIGN-OBJECT (INTERFACE 'INTERFACE)
   (SETF (FOREIGN-SLOT-VALUE INTERFACE '(:STRUCT INTERFACE) 'DESTROY)
           (CALLBACK DESTROY-FFI))
   (SETF (FOREIGN-SLOT-VALUE INTERFACE '(:STRUCT INTERFACE) 'GRAB)
           (CALLBACK GRAB-FFI))
   (SETF (FOREIGN-SLOT-VALUE INTERFACE '(:STRUCT INTERFACE) 'REPOSITION)
           (CALLBACK REPOSITION-FFI))
   INTERFACE))

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
  "Default bind implementation for the ,(name interface) global object.
This can be overriden by inheritance in case if custom behaviour is required."
  (LET ((BOUND (MAKE-INSTANCE 'DISPATCH :DISPLAY (DISPLAY CLIENT))))
    (SETF (IFACE CLIENT ID) BOUND)
    (CREATE-RESOURCE CLIENT *INTERFACE* VERSION ID)))

(CL-ASYNC-UTIL:DEFINE-C-CALLBACK DISPATCH-BIND-FFI
    :VOID
    ((CLIENT :POINTER) (DATA :POINTER) (VERSION :UINT) (ID :UINT))
  (LET* ((CLIENT (GET-CLIENT CLIENT))
         (DATA (POP-DATA DATA))
         (GLOBAL (GETHASH DATA *GLOBAL-TRACKER*)))
    (FUNCALL 'DISPATCH-BIND GLOBAL CLIENT (NULL-POINTER)
             (MEM-REF VERSION :UINT) (MEM-REF ID :UINT))))

(DEFVAR *DISPATCH-BIND* (CALLBACK DISPATCH-BIND-FFI))

(DEFMETHOD INITIALIZE-INSTANCE :AFTER ((GLOBAL GLOBAL) &KEY)
  (LET* ((NEXT-DATA-ID (RESERVE-DATA))
         (GLOBAL-PTR
          (GLOBAL-CREATE (DISPLAY GLOBAL) *INTERFACE* (VERSION GLOBAL)
           (DATA-PTR NEXT-DATA-ID) *DISPATCH-BIND*)))
    (SET-DATA NEXT-DATA-ID
     (SETF (GETHASH (GLOBAL-GET-NAME GLOBAL-PTR (NULL-POINTER))
                    *GLOBAL-TRACKER*)
             GLOBAL))))

