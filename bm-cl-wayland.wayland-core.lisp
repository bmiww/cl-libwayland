(DEFPACKAGE :WL/WL_DISPLAY
  (:USE :CL :WL :CFFI)
  (:EXPORT DISPATCH GLOBAL))

(IN-PACKAGE :WL/WL_DISPLAY)

(DEFCLASS DISPATCH (BM-CL-WAYLAND:OBJECT) NIL (:DEFAULT-INITARGS :VERSION 1)
          (:DOCUMENTATION "core global object

The core global object.  This is a special singleton object.  It
      is used for internal Wayland protocol features.
"))

(DEFVAR *INTERFACE* NIL)

(DEFCSTRUCT INTERFACE (SYNC :POINTER) (GET_REGISTRY :POINTER))

(DEFGENERIC SYNC
    (RESOURCE CLIENT CALLBACK))

(DEFGENERIC GET_REGISTRY
    (RESOURCE CLIENT REGISTRY))

(CL-ASYNC-UTIL:DEFINE-C-CALLBACK SYNC-FFI
    :VOID
    ((CLIENT :POINTER) (RESOURCE :POINTER) (CALLBACK :UINT))
  (LET ((CLIENT (GET-CLIENT CLIENT)) (RESOURCE (RESOURCE-GET-ID RESOURCE)))
    (FUNCALL 'SYNC (IFACE CLIENT RESOURCE) CLIENT ID)))

(CL-ASYNC-UTIL:DEFINE-C-CALLBACK GET_REGISTRY-FFI
    :VOID
    ((CLIENT :POINTER) (RESOURCE :POINTER) (REGISTRY :UINT))
  (LET ((CLIENT (GET-CLIENT CLIENT)) (RESOURCE (RESOURCE-GET-ID RESOURCE)))
    (FUNCALL 'GET_REGISTRY (IFACE CLIENT RESOURCE) CLIENT ID)))

(DEFPACKAGE :WL/WL_REGISTRY
  (:USE :CL :WL :CFFI)
  (:EXPORT DISPATCH GLOBAL))

(IN-PACKAGE :WL/WL_REGISTRY)

(DEFCLASS DISPATCH (BM-CL-WAYLAND:OBJECT) NIL (:DEFAULT-INITARGS :VERSION 1)
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

(DEFVAR *INTERFACE* NIL)

(DEFCSTRUCT INTERFACE (BIND :POINTER))

(DEFGENERIC BIND
    (RESOURCE CLIENT NAME ID))

(CL-ASYNC-UTIL:DEFINE-C-CALLBACK BIND-FFI
    :VOID
    ((CLIENT :POINTER) (RESOURCE :POINTER) (NAME :UINT) (ID :UINT))
  (LET ((CLIENT (GET-CLIENT CLIENT)) (RESOURCE (RESOURCE-GET-ID RESOURCE)))
    (FUNCALL 'BIND (IFACE CLIENT RESOURCE) CLIENT ID)))

(DEFCLASS GLOBAL (BM-CL-WAYLAND::GLOBAL) NIL (:DEFAULT-INITARGS :VERSION 1)
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
  "Default bind implementation for the ,(name interface) global object.
This can be overriden by inheritance in case if custom behaviour is required."
  (LET ((BOUND (MAKE-INSTANCE 'DISPATCH (DISPLAY CLIENT))))
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

(DEFPACKAGE :WL/WL_CALLBACK
  (:USE :CL :WL :CFFI)
  (:EXPORT DISPATCH GLOBAL))

(IN-PACKAGE :WL/WL_CALLBACK)

(DEFCLASS DISPATCH (BM-CL-WAYLAND:OBJECT) NIL (:DEFAULT-INITARGS :VERSION 1)
          (:DOCUMENTATION "callback object

Clients can handle the 'done' event to get notified when
      the related request is done.

      Note, because wl_callback objects are created from multiple independent
      factory interfaces, the wl_callback interface is frozen at version 1.
"))

(DEFVAR *INTERFACE* NIL)

(DEFCSTRUCT INTERFACE)

(DEFPACKAGE :WL/WL_COMPOSITOR
  (:USE :CL :WL :CFFI)
  (:EXPORT DISPATCH GLOBAL))

(IN-PACKAGE :WL/WL_COMPOSITOR)

(DEFCLASS DISPATCH (BM-CL-WAYLAND:OBJECT) NIL (:DEFAULT-INITARGS :VERSION 6)
          (:DOCUMENTATION "the compositor singleton

A compositor.  This object is a singleton global.  The
      compositor is in charge of combining the contents of multiple
      surfaces into one displayable output.
"))

(DEFVAR *INTERFACE* NIL)

(DEFCSTRUCT INTERFACE (CREATE_SURFACE :POINTER) (CREATE_REGION :POINTER))

(DEFGENERIC CREATE_SURFACE
    (RESOURCE CLIENT ID))

(DEFGENERIC CREATE_REGION
    (RESOURCE CLIENT ID))

(CL-ASYNC-UTIL:DEFINE-C-CALLBACK CREATE_SURFACE-FFI
    :VOID
    ((CLIENT :POINTER) (RESOURCE :POINTER) (ID :UINT))
  (LET ((CLIENT (GET-CLIENT CLIENT)) (RESOURCE (RESOURCE-GET-ID RESOURCE)))
    (FUNCALL 'CREATE_SURFACE (IFACE CLIENT RESOURCE) CLIENT ID)))

(CL-ASYNC-UTIL:DEFINE-C-CALLBACK CREATE_REGION-FFI
    :VOID
    ((CLIENT :POINTER) (RESOURCE :POINTER) (ID :UINT))
  (LET ((CLIENT (GET-CLIENT CLIENT)) (RESOURCE (RESOURCE-GET-ID RESOURCE)))
    (FUNCALL 'CREATE_REGION (IFACE CLIENT RESOURCE) CLIENT ID)))

(DEFCLASS GLOBAL (BM-CL-WAYLAND::GLOBAL) NIL (:DEFAULT-INITARGS :VERSION 6)
          (:DOCUMENTATION "the compositor singleton

A compositor.  This object is a singleton global.  The
      compositor is in charge of combining the contents of multiple
      surfaces into one displayable output.
"))

(DEFMETHOD DISPATCH-BIND ((GLOBAL GLOBAL) CLIENT DATA VERSION ID)
  "Default bind implementation for the ,(name interface) global object.
This can be overriden by inheritance in case if custom behaviour is required."
  (LET ((BOUND (MAKE-INSTANCE 'DISPATCH (DISPLAY CLIENT))))
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

(DEFPACKAGE :WL/WL_SHM_POOL
  (:USE :CL :WL :CFFI)
  (:EXPORT DISPATCH GLOBAL))

(IN-PACKAGE :WL/WL_SHM_POOL)

(DEFCLASS DISPATCH (BM-CL-WAYLAND:OBJECT) NIL (:DEFAULT-INITARGS :VERSION 1)
          (:DOCUMENTATION "a shared memory pool

The wl_shm_pool object encapsulates a piece of memory shared
      between the compositor and client.  Through the wl_shm_pool
      object, the client can allocate shared memory wl_buffer objects.
      All objects created through the same pool share the same
      underlying mapped memory. Reusing the mapped memory avoids the
      setup/teardown overhead and is useful when interactively resizing
      a surface or for many small buffers.
"))

(DEFVAR *INTERFACE* NIL)

(DEFCSTRUCT INTERFACE (CREATE_BUFFER :POINTER) (DESTROY :POINTER)
 (RESIZE :POINTER))

(DEFGENERIC CREATE_BUFFER
    (RESOURCE CLIENT ID OFFSET WIDTH HEIGHT STRIDE FORMAT))

(DEFGENERIC DESTROY
    (RESOURCE CLIENT))

(DEFGENERIC RESIZE
    (RESOURCE CLIENT SIZE))

(CL-ASYNC-UTIL:DEFINE-C-CALLBACK CREATE_BUFFER-FFI
    :VOID
    ((CLIENT :POINTER) (RESOURCE :POINTER) (ID :UINT) (OFFSET :INT)
     (WIDTH :INT) (HEIGHT :INT) (STRIDE :INT) (FORMAT :UINT))
  (LET ((CLIENT (GET-CLIENT CLIENT)) (RESOURCE (RESOURCE-GET-ID RESOURCE)))
    (FUNCALL 'CREATE_BUFFER (IFACE CLIENT RESOURCE) CLIENT ID)))

(CL-ASYNC-UTIL:DEFINE-C-CALLBACK DESTROY-FFI
    :VOID
    ((CLIENT :POINTER) (RESOURCE :POINTER))
  (LET ((CLIENT (GET-CLIENT CLIENT)) (RESOURCE (RESOURCE-GET-ID RESOURCE)))
    (FUNCALL 'DESTROY (IFACE CLIENT RESOURCE) CLIENT ID)))

(CL-ASYNC-UTIL:DEFINE-C-CALLBACK RESIZE-FFI
    :VOID
    ((CLIENT :POINTER) (RESOURCE :POINTER) (SIZE :INT))
  (LET ((CLIENT (GET-CLIENT CLIENT)) (RESOURCE (RESOURCE-GET-ID RESOURCE)))
    (FUNCALL 'RESIZE (IFACE CLIENT RESOURCE) CLIENT ID)))

(DEFPACKAGE :WL/WL_SHM
  (:USE :CL :WL :CFFI)
  (:EXPORT DISPATCH GLOBAL))

(IN-PACKAGE :WL/WL_SHM)

(DEFCLASS DISPATCH (BM-CL-WAYLAND:OBJECT) NIL (:DEFAULT-INITARGS :VERSION 1)
          (:DOCUMENTATION "shared memory support

A singleton global object that provides support for shared
      memory.

      Clients can create wl_shm_pool objects using the create_pool
      request.

      On binding the wl_shm object one or more format events
      are emitted to inform clients about the valid pixel formats
      that can be used for buffers.
"))

(DEFVAR *INTERFACE* NIL)

(DEFCSTRUCT INTERFACE (CREATE_POOL :POINTER))

(DEFGENERIC CREATE_POOL
    (RESOURCE CLIENT ID FD SIZE))

(CL-ASYNC-UTIL:DEFINE-C-CALLBACK CREATE_POOL-FFI
    :VOID
    ((CLIENT :POINTER) (RESOURCE :POINTER) (ID :UINT) (FD :UINT) (SIZE :INT))
  (LET ((CLIENT (GET-CLIENT CLIENT)) (RESOURCE (RESOURCE-GET-ID RESOURCE)))
    (FUNCALL 'CREATE_POOL (IFACE CLIENT RESOURCE) CLIENT ID)))

(DEFCLASS GLOBAL (BM-CL-WAYLAND::GLOBAL) NIL (:DEFAULT-INITARGS :VERSION 1)
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
  "Default bind implementation for the ,(name interface) global object.
This can be overriden by inheritance in case if custom behaviour is required."
  (LET ((BOUND (MAKE-INSTANCE 'DISPATCH (DISPLAY CLIENT))))
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

(DEFPACKAGE :WL/WL_BUFFER
  (:USE :CL :WL :CFFI)
  (:EXPORT DISPATCH GLOBAL))

(IN-PACKAGE :WL/WL_BUFFER)

(DEFCLASS DISPATCH (BM-CL-WAYLAND:OBJECT) NIL (:DEFAULT-INITARGS :VERSION 1)
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

(DEFVAR *INTERFACE* NIL)

(DEFCSTRUCT INTERFACE (DESTROY :POINTER))

(DEFGENERIC DESTROY
    (RESOURCE CLIENT))

(CL-ASYNC-UTIL:DEFINE-C-CALLBACK DESTROY-FFI
    :VOID
    ((CLIENT :POINTER) (RESOURCE :POINTER))
  (LET ((CLIENT (GET-CLIENT CLIENT)) (RESOURCE (RESOURCE-GET-ID RESOURCE)))
    (FUNCALL 'DESTROY (IFACE CLIENT RESOURCE) CLIENT ID)))

(DEFPACKAGE :WL/WL_DATA_OFFER
  (:USE :CL :WL :CFFI)
  (:EXPORT DISPATCH GLOBAL))

(IN-PACKAGE :WL/WL_DATA_OFFER)

(DEFCLASS DISPATCH (BM-CL-WAYLAND:OBJECT) NIL (:DEFAULT-INITARGS :VERSION 3)
          (:DOCUMENTATION "offer to transfer data

A wl_data_offer represents a piece of data offered for transfer
      by another client (the source client).  It is used by the
      copy-and-paste and drag-and-drop mechanisms.  The offer
      describes the different mime types that the data can be
      converted to and provides the mechanism for transferring the
      data directly from the source client.
"))

(DEFVAR *INTERFACE* NIL)

(DEFCSTRUCT INTERFACE (ACCEPT :POINTER) (RECEIVE :POINTER) (DESTROY :POINTER)
 (FINISH :POINTER) (SET_ACTIONS :POINTER))

(DEFGENERIC ACCEPT
    (RESOURCE CLIENT SERIAL MIME_TYPE))

(DEFGENERIC RECEIVE
    (RESOURCE CLIENT MIME_TYPE FD))

(DEFGENERIC DESTROY
    (RESOURCE CLIENT))

(DEFGENERIC FINISH
    (RESOURCE CLIENT))

(DEFGENERIC SET_ACTIONS
    (RESOURCE CLIENT DND_ACTIONS PREFERRED_ACTION))

(CL-ASYNC-UTIL:DEFINE-C-CALLBACK ACCEPT-FFI
    :VOID
    ((CLIENT :POINTER) (RESOURCE :POINTER) (SERIAL :UINT)
     (MIME_TYPE (:POINTER :CHAR)))
  (LET ((CLIENT (GET-CLIENT CLIENT)) (RESOURCE (RESOURCE-GET-ID RESOURCE)))
    (FUNCALL 'ACCEPT (IFACE CLIENT RESOURCE) CLIENT ID)))

(CL-ASYNC-UTIL:DEFINE-C-CALLBACK RECEIVE-FFI
    :VOID
    ((CLIENT :POINTER) (RESOURCE :POINTER) (MIME_TYPE (:POINTER :CHAR))
     (FD :UINT))
  (LET ((CLIENT (GET-CLIENT CLIENT)) (RESOURCE (RESOURCE-GET-ID RESOURCE)))
    (FUNCALL 'RECEIVE (IFACE CLIENT RESOURCE) CLIENT ID)))

(CL-ASYNC-UTIL:DEFINE-C-CALLBACK DESTROY-FFI
    :VOID
    ((CLIENT :POINTER) (RESOURCE :POINTER))
  (LET ((CLIENT (GET-CLIENT CLIENT)) (RESOURCE (RESOURCE-GET-ID RESOURCE)))
    (FUNCALL 'DESTROY (IFACE CLIENT RESOURCE) CLIENT ID)))

(CL-ASYNC-UTIL:DEFINE-C-CALLBACK FINISH-FFI
    :VOID
    ((CLIENT :POINTER) (RESOURCE :POINTER))
  (LET ((CLIENT (GET-CLIENT CLIENT)) (RESOURCE (RESOURCE-GET-ID RESOURCE)))
    (FUNCALL 'FINISH (IFACE CLIENT RESOURCE) CLIENT ID)))

(CL-ASYNC-UTIL:DEFINE-C-CALLBACK SET_ACTIONS-FFI
    :VOID
    ((CLIENT :POINTER) (RESOURCE :POINTER) (DND_ACTIONS :UINT)
     (PREFERRED_ACTION :UINT))
  (LET ((CLIENT (GET-CLIENT CLIENT)) (RESOURCE (RESOURCE-GET-ID RESOURCE)))
    (FUNCALL 'SET_ACTIONS (IFACE CLIENT RESOURCE) CLIENT ID)))

(DEFPACKAGE :WL/WL_DATA_SOURCE
  (:USE :CL :WL :CFFI)
  (:EXPORT DISPATCH GLOBAL))

(IN-PACKAGE :WL/WL_DATA_SOURCE)

(DEFCLASS DISPATCH (BM-CL-WAYLAND:OBJECT) NIL (:DEFAULT-INITARGS :VERSION 3)
          (:DOCUMENTATION "offer to transfer data

The wl_data_source object is the source side of a wl_data_offer.
      It is created by the source client in a data transfer and
      provides a way to describe the offered data and a way to respond
      to requests to transfer the data.
"))

(DEFVAR *INTERFACE* NIL)

(DEFCSTRUCT INTERFACE (OFFER :POINTER) (DESTROY :POINTER)
 (SET_ACTIONS :POINTER))

(DEFGENERIC OFFER
    (RESOURCE CLIENT MIME_TYPE))

(DEFGENERIC DESTROY
    (RESOURCE CLIENT))

(DEFGENERIC SET_ACTIONS
    (RESOURCE CLIENT DND_ACTIONS))

(CL-ASYNC-UTIL:DEFINE-C-CALLBACK OFFER-FFI
    :VOID
    ((CLIENT :POINTER) (RESOURCE :POINTER) (MIME_TYPE (:POINTER :CHAR)))
  (LET ((CLIENT (GET-CLIENT CLIENT)) (RESOURCE (RESOURCE-GET-ID RESOURCE)))
    (FUNCALL 'OFFER (IFACE CLIENT RESOURCE) CLIENT ID)))

(CL-ASYNC-UTIL:DEFINE-C-CALLBACK DESTROY-FFI
    :VOID
    ((CLIENT :POINTER) (RESOURCE :POINTER))
  (LET ((CLIENT (GET-CLIENT CLIENT)) (RESOURCE (RESOURCE-GET-ID RESOURCE)))
    (FUNCALL 'DESTROY (IFACE CLIENT RESOURCE) CLIENT ID)))

(CL-ASYNC-UTIL:DEFINE-C-CALLBACK SET_ACTIONS-FFI
    :VOID
    ((CLIENT :POINTER) (RESOURCE :POINTER) (DND_ACTIONS :UINT))
  (LET ((CLIENT (GET-CLIENT CLIENT)) (RESOURCE (RESOURCE-GET-ID RESOURCE)))
    (FUNCALL 'SET_ACTIONS (IFACE CLIENT RESOURCE) CLIENT ID)))

(DEFPACKAGE :WL/WL_DATA_DEVICE
  (:USE :CL :WL :CFFI)
  (:EXPORT DISPATCH GLOBAL))

(IN-PACKAGE :WL/WL_DATA_DEVICE)

(DEFCLASS DISPATCH (BM-CL-WAYLAND:OBJECT) NIL (:DEFAULT-INITARGS :VERSION 3)
          (:DOCUMENTATION "data transfer device

There is one wl_data_device per seat which can be obtained
      from the global wl_data_device_manager singleton.

      A wl_data_device provides access to inter-client data transfer
      mechanisms such as copy-and-paste and drag-and-drop.
"))

(DEFVAR *INTERFACE* NIL)

(DEFCSTRUCT INTERFACE (START_DRAG :POINTER) (SET_SELECTION :POINTER)
 (RELEASE :POINTER))

(DEFGENERIC START_DRAG
    (RESOURCE CLIENT SOURCE ORIGIN ICON SERIAL))

(DEFGENERIC SET_SELECTION
    (RESOURCE CLIENT SOURCE SERIAL))

(DEFGENERIC RELEASE
    (RESOURCE CLIENT))

(CL-ASYNC-UTIL:DEFINE-C-CALLBACK START_DRAG-FFI
    :VOID
    ((CLIENT :POINTER) (RESOURCE :POINTER) (SOURCE :UINT) (ORIGIN :UINT)
     (ICON :UINT) (SERIAL :UINT))
  (LET ((CLIENT (GET-CLIENT CLIENT)) (RESOURCE (RESOURCE-GET-ID RESOURCE)))
    (FUNCALL 'START_DRAG (IFACE CLIENT RESOURCE) CLIENT ID)))

(CL-ASYNC-UTIL:DEFINE-C-CALLBACK SET_SELECTION-FFI
    :VOID
    ((CLIENT :POINTER) (RESOURCE :POINTER) (SOURCE :UINT) (SERIAL :UINT))
  (LET ((CLIENT (GET-CLIENT CLIENT)) (RESOURCE (RESOURCE-GET-ID RESOURCE)))
    (FUNCALL 'SET_SELECTION (IFACE CLIENT RESOURCE) CLIENT ID)))

(CL-ASYNC-UTIL:DEFINE-C-CALLBACK RELEASE-FFI
    :VOID
    ((CLIENT :POINTER) (RESOURCE :POINTER))
  (LET ((CLIENT (GET-CLIENT CLIENT)) (RESOURCE (RESOURCE-GET-ID RESOURCE)))
    (FUNCALL 'RELEASE (IFACE CLIENT RESOURCE) CLIENT ID)))

(DEFCLASS GLOBAL (BM-CL-WAYLAND::GLOBAL) NIL (:DEFAULT-INITARGS :VERSION 3)
          (:DOCUMENTATION "data transfer device

There is one wl_data_device per seat which can be obtained
      from the global wl_data_device_manager singleton.

      A wl_data_device provides access to inter-client data transfer
      mechanisms such as copy-and-paste and drag-and-drop.
"))

(DEFMETHOD DISPATCH-BIND ((GLOBAL GLOBAL) CLIENT DATA VERSION ID)
  "Default bind implementation for the ,(name interface) global object.
This can be overriden by inheritance in case if custom behaviour is required."
  (LET ((BOUND (MAKE-INSTANCE 'DISPATCH (DISPLAY CLIENT))))
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

(DEFPACKAGE :WL/WL_DATA_DEVICE_MANAGER
  (:USE :CL :WL :CFFI)
  (:EXPORT DISPATCH GLOBAL))

(IN-PACKAGE :WL/WL_DATA_DEVICE_MANAGER)

(DEFCLASS DISPATCH (BM-CL-WAYLAND:OBJECT) NIL (:DEFAULT-INITARGS :VERSION 3)
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

(DEFVAR *INTERFACE* NIL)

(DEFCSTRUCT INTERFACE (CREATE_DATA_SOURCE :POINTER) (GET_DATA_DEVICE :POINTER))

(DEFGENERIC CREATE_DATA_SOURCE
    (RESOURCE CLIENT ID))

(DEFGENERIC GET_DATA_DEVICE
    (RESOURCE CLIENT ID SEAT))

(CL-ASYNC-UTIL:DEFINE-C-CALLBACK CREATE_DATA_SOURCE-FFI
    :VOID
    ((CLIENT :POINTER) (RESOURCE :POINTER) (ID :UINT))
  (LET ((CLIENT (GET-CLIENT CLIENT)) (RESOURCE (RESOURCE-GET-ID RESOURCE)))
    (FUNCALL 'CREATE_DATA_SOURCE (IFACE CLIENT RESOURCE) CLIENT ID)))

(CL-ASYNC-UTIL:DEFINE-C-CALLBACK GET_DATA_DEVICE-FFI
    :VOID
    ((CLIENT :POINTER) (RESOURCE :POINTER) (ID :UINT) (SEAT :UINT))
  (LET ((CLIENT (GET-CLIENT CLIENT)) (RESOURCE (RESOURCE-GET-ID RESOURCE)))
    (FUNCALL 'GET_DATA_DEVICE (IFACE CLIENT RESOURCE) CLIENT ID)))

(DEFCLASS GLOBAL (BM-CL-WAYLAND::GLOBAL) NIL (:DEFAULT-INITARGS :VERSION 3)
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
  "Default bind implementation for the ,(name interface) global object.
This can be overriden by inheritance in case if custom behaviour is required."
  (LET ((BOUND (MAKE-INSTANCE 'DISPATCH (DISPLAY CLIENT))))
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

(DEFPACKAGE :WL/WL_SHELL
  (:USE :CL :WL :CFFI)
  (:EXPORT DISPATCH GLOBAL))

(IN-PACKAGE :WL/WL_SHELL)

(DEFCLASS DISPATCH (BM-CL-WAYLAND:OBJECT) NIL (:DEFAULT-INITARGS :VERSION 1)
          (:DOCUMENTATION "create desktop-style surfaces

This interface is implemented by servers that provide
      desktop-style user interfaces.

      It allows clients to associate a wl_shell_surface with
      a basic surface.

      Note! This protocol is deprecated and not intended for production use.
      For desktop-style user interfaces, use xdg_shell. Compositors and clients
      should not implement this interface.
"))

(DEFVAR *INTERFACE* NIL)

(DEFCSTRUCT INTERFACE (GET_SHELL_SURFACE :POINTER))

(DEFGENERIC GET_SHELL_SURFACE
    (RESOURCE CLIENT ID SURFACE))

(CL-ASYNC-UTIL:DEFINE-C-CALLBACK GET_SHELL_SURFACE-FFI
    :VOID
    ((CLIENT :POINTER) (RESOURCE :POINTER) (ID :UINT) (SURFACE :UINT))
  (LET ((CLIENT (GET-CLIENT CLIENT)) (RESOURCE (RESOURCE-GET-ID RESOURCE)))
    (FUNCALL 'GET_SHELL_SURFACE (IFACE CLIENT RESOURCE) CLIENT ID)))

(DEFPACKAGE :WL/WL_SHELL_SURFACE
  (:USE :CL :WL :CFFI)
  (:EXPORT DISPATCH GLOBAL))

(IN-PACKAGE :WL/WL_SHELL_SURFACE)

(DEFCLASS DISPATCH (BM-CL-WAYLAND:OBJECT) NIL (:DEFAULT-INITARGS :VERSION 1)
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

(DEFVAR *INTERFACE* NIL)

(DEFCSTRUCT INTERFACE (PONG :POINTER) (MOVE :POINTER) (RESIZE :POINTER)
 (SET_TOPLEVEL :POINTER) (SET_TRANSIENT :POINTER) (SET_FULLSCREEN :POINTER)
 (SET_POPUP :POINTER) (SET_MAXIMIZED :POINTER) (SET_TITLE :POINTER)
 (SET_CLASS :POINTER))

(DEFGENERIC PONG
    (RESOURCE CLIENT SERIAL))

(DEFGENERIC MOVE
    (RESOURCE CLIENT SEAT SERIAL))

(DEFGENERIC RESIZE
    (RESOURCE CLIENT SEAT SERIAL EDGES))

(DEFGENERIC SET_TOPLEVEL
    (RESOURCE CLIENT))

(DEFGENERIC SET_TRANSIENT
    (RESOURCE CLIENT PARENT X Y FLAGS))

(DEFGENERIC SET_FULLSCREEN
    (RESOURCE CLIENT METHOD FRAMERATE OUTPUT))

(DEFGENERIC SET_POPUP
    (RESOURCE CLIENT SEAT SERIAL PARENT X Y FLAGS))

(DEFGENERIC SET_MAXIMIZED
    (RESOURCE CLIENT OUTPUT))

(DEFGENERIC SET_TITLE
    (RESOURCE CLIENT TITLE))

(DEFGENERIC SET_CLASS
    (RESOURCE CLIENT CLASS_))

(CL-ASYNC-UTIL:DEFINE-C-CALLBACK PONG-FFI
    :VOID
    ((CLIENT :POINTER) (RESOURCE :POINTER) (SERIAL :UINT))
  (LET ((CLIENT (GET-CLIENT CLIENT)) (RESOURCE (RESOURCE-GET-ID RESOURCE)))
    (FUNCALL 'PONG (IFACE CLIENT RESOURCE) CLIENT ID)))

(CL-ASYNC-UTIL:DEFINE-C-CALLBACK MOVE-FFI
    :VOID
    ((CLIENT :POINTER) (RESOURCE :POINTER) (SEAT :UINT) (SERIAL :UINT))
  (LET ((CLIENT (GET-CLIENT CLIENT)) (RESOURCE (RESOURCE-GET-ID RESOURCE)))
    (FUNCALL 'MOVE (IFACE CLIENT RESOURCE) CLIENT ID)))

(CL-ASYNC-UTIL:DEFINE-C-CALLBACK RESIZE-FFI
    :VOID
    ((CLIENT :POINTER) (RESOURCE :POINTER) (SEAT :UINT) (SERIAL :UINT)
     (EDGES :UINT))
  (LET ((CLIENT (GET-CLIENT CLIENT)) (RESOURCE (RESOURCE-GET-ID RESOURCE)))
    (FUNCALL 'RESIZE (IFACE CLIENT RESOURCE) CLIENT ID)))

(CL-ASYNC-UTIL:DEFINE-C-CALLBACK SET_TOPLEVEL-FFI
    :VOID
    ((CLIENT :POINTER) (RESOURCE :POINTER))
  (LET ((CLIENT (GET-CLIENT CLIENT)) (RESOURCE (RESOURCE-GET-ID RESOURCE)))
    (FUNCALL 'SET_TOPLEVEL (IFACE CLIENT RESOURCE) CLIENT ID)))

(CL-ASYNC-UTIL:DEFINE-C-CALLBACK SET_TRANSIENT-FFI
    :VOID
    ((CLIENT :POINTER) (RESOURCE :POINTER) (PARENT :UINT) (X :INT) (Y :INT)
     (FLAGS :UINT))
  (LET ((CLIENT (GET-CLIENT CLIENT)) (RESOURCE (RESOURCE-GET-ID RESOURCE)))
    (FUNCALL 'SET_TRANSIENT (IFACE CLIENT RESOURCE) CLIENT ID)))

(CL-ASYNC-UTIL:DEFINE-C-CALLBACK SET_FULLSCREEN-FFI
    :VOID
    ((CLIENT :POINTER) (RESOURCE :POINTER) (METHOD :UINT) (FRAMERATE :UINT)
     (OUTPUT :UINT))
  (LET ((CLIENT (GET-CLIENT CLIENT)) (RESOURCE (RESOURCE-GET-ID RESOURCE)))
    (FUNCALL 'SET_FULLSCREEN (IFACE CLIENT RESOURCE) CLIENT ID)))

(CL-ASYNC-UTIL:DEFINE-C-CALLBACK SET_POPUP-FFI
    :VOID
    ((CLIENT :POINTER) (RESOURCE :POINTER) (SEAT :UINT) (SERIAL :UINT)
     (PARENT :UINT) (X :INT) (Y :INT) (FLAGS :UINT))
  (LET ((CLIENT (GET-CLIENT CLIENT)) (RESOURCE (RESOURCE-GET-ID RESOURCE)))
    (FUNCALL 'SET_POPUP (IFACE CLIENT RESOURCE) CLIENT ID)))

(CL-ASYNC-UTIL:DEFINE-C-CALLBACK SET_MAXIMIZED-FFI
    :VOID
    ((CLIENT :POINTER) (RESOURCE :POINTER) (OUTPUT :UINT))
  (LET ((CLIENT (GET-CLIENT CLIENT)) (RESOURCE (RESOURCE-GET-ID RESOURCE)))
    (FUNCALL 'SET_MAXIMIZED (IFACE CLIENT RESOURCE) CLIENT ID)))

(CL-ASYNC-UTIL:DEFINE-C-CALLBACK SET_TITLE-FFI
    :VOID
    ((CLIENT :POINTER) (RESOURCE :POINTER) (TITLE (:POINTER :CHAR)))
  (LET ((CLIENT (GET-CLIENT CLIENT)) (RESOURCE (RESOURCE-GET-ID RESOURCE)))
    (FUNCALL 'SET_TITLE (IFACE CLIENT RESOURCE) CLIENT ID)))

(CL-ASYNC-UTIL:DEFINE-C-CALLBACK SET_CLASS-FFI
    :VOID
    ((CLIENT :POINTER) (RESOURCE :POINTER) (CLASS_ (:POINTER :CHAR)))
  (LET ((CLIENT (GET-CLIENT CLIENT)) (RESOURCE (RESOURCE-GET-ID RESOURCE)))
    (FUNCALL 'SET_CLASS (IFACE CLIENT RESOURCE) CLIENT ID)))

(DEFPACKAGE :WL/WL_SURFACE
  (:USE :CL :WL :CFFI)
  (:EXPORT DISPATCH GLOBAL))

(IN-PACKAGE :WL/WL_SURFACE)

(DEFCLASS DISPATCH (BM-CL-WAYLAND:OBJECT) NIL (:DEFAULT-INITARGS :VERSION 6)
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

(DEFVAR *INTERFACE* NIL)

(DEFCSTRUCT INTERFACE (DESTROY :POINTER) (ATTACH :POINTER) (DAMAGE :POINTER)
 (FRAME :POINTER) (SET_OPAQUE_REGION :POINTER) (SET_INPUT_REGION :POINTER)
 (COMMIT :POINTER) (SET_BUFFER_TRANSFORM :POINTER) (SET_BUFFER_SCALE :POINTER)
 (DAMAGE_BUFFER :POINTER) (OFFSET :POINTER))

(DEFGENERIC DESTROY
    (RESOURCE CLIENT))

(DEFGENERIC ATTACH
    (RESOURCE CLIENT BUFFER X Y))

(DEFGENERIC DAMAGE
    (RESOURCE CLIENT X Y WIDTH HEIGHT))

(DEFGENERIC FRAME
    (RESOURCE CLIENT CALLBACK))

(DEFGENERIC SET_OPAQUE_REGION
    (RESOURCE CLIENT REGION))

(DEFGENERIC SET_INPUT_REGION
    (RESOURCE CLIENT REGION))

(DEFGENERIC COMMIT
    (RESOURCE CLIENT))

(DEFGENERIC SET_BUFFER_TRANSFORM
    (RESOURCE CLIENT TRANSFORM))

(DEFGENERIC SET_BUFFER_SCALE
    (RESOURCE CLIENT SCALE))

(DEFGENERIC DAMAGE_BUFFER
    (RESOURCE CLIENT X Y WIDTH HEIGHT))

(DEFGENERIC OFFSET
    (RESOURCE CLIENT X Y))

(CL-ASYNC-UTIL:DEFINE-C-CALLBACK DESTROY-FFI
    :VOID
    ((CLIENT :POINTER) (RESOURCE :POINTER))
  (LET ((CLIENT (GET-CLIENT CLIENT)) (RESOURCE (RESOURCE-GET-ID RESOURCE)))
    (FUNCALL 'DESTROY (IFACE CLIENT RESOURCE) CLIENT ID)))

(CL-ASYNC-UTIL:DEFINE-C-CALLBACK ATTACH-FFI
    :VOID
    ((CLIENT :POINTER) (RESOURCE :POINTER) (BUFFER :UINT) (X :INT) (Y :INT))
  (LET ((CLIENT (GET-CLIENT CLIENT)) (RESOURCE (RESOURCE-GET-ID RESOURCE)))
    (FUNCALL 'ATTACH (IFACE CLIENT RESOURCE) CLIENT ID)))

(CL-ASYNC-UTIL:DEFINE-C-CALLBACK DAMAGE-FFI
    :VOID
    ((CLIENT :POINTER) (RESOURCE :POINTER) (X :INT) (Y :INT) (WIDTH :INT)
     (HEIGHT :INT))
  (LET ((CLIENT (GET-CLIENT CLIENT)) (RESOURCE (RESOURCE-GET-ID RESOURCE)))
    (FUNCALL 'DAMAGE (IFACE CLIENT RESOURCE) CLIENT ID)))

(CL-ASYNC-UTIL:DEFINE-C-CALLBACK FRAME-FFI
    :VOID
    ((CLIENT :POINTER) (RESOURCE :POINTER) (CALLBACK :UINT))
  (LET ((CLIENT (GET-CLIENT CLIENT)) (RESOURCE (RESOURCE-GET-ID RESOURCE)))
    (FUNCALL 'FRAME (IFACE CLIENT RESOURCE) CLIENT ID)))

(CL-ASYNC-UTIL:DEFINE-C-CALLBACK SET_OPAQUE_REGION-FFI
    :VOID
    ((CLIENT :POINTER) (RESOURCE :POINTER) (REGION :UINT))
  (LET ((CLIENT (GET-CLIENT CLIENT)) (RESOURCE (RESOURCE-GET-ID RESOURCE)))
    (FUNCALL 'SET_OPAQUE_REGION (IFACE CLIENT RESOURCE) CLIENT ID)))

(CL-ASYNC-UTIL:DEFINE-C-CALLBACK SET_INPUT_REGION-FFI
    :VOID
    ((CLIENT :POINTER) (RESOURCE :POINTER) (REGION :UINT))
  (LET ((CLIENT (GET-CLIENT CLIENT)) (RESOURCE (RESOURCE-GET-ID RESOURCE)))
    (FUNCALL 'SET_INPUT_REGION (IFACE CLIENT RESOURCE) CLIENT ID)))

(CL-ASYNC-UTIL:DEFINE-C-CALLBACK COMMIT-FFI
    :VOID
    ((CLIENT :POINTER) (RESOURCE :POINTER))
  (LET ((CLIENT (GET-CLIENT CLIENT)) (RESOURCE (RESOURCE-GET-ID RESOURCE)))
    (FUNCALL 'COMMIT (IFACE CLIENT RESOURCE) CLIENT ID)))

(CL-ASYNC-UTIL:DEFINE-C-CALLBACK SET_BUFFER_TRANSFORM-FFI
    :VOID
    ((CLIENT :POINTER) (RESOURCE :POINTER) (TRANSFORM :UINT))
  (LET ((CLIENT (GET-CLIENT CLIENT)) (RESOURCE (RESOURCE-GET-ID RESOURCE)))
    (FUNCALL 'SET_BUFFER_TRANSFORM (IFACE CLIENT RESOURCE) CLIENT ID)))

(CL-ASYNC-UTIL:DEFINE-C-CALLBACK SET_BUFFER_SCALE-FFI
    :VOID
    ((CLIENT :POINTER) (RESOURCE :POINTER) (SCALE :INT))
  (LET ((CLIENT (GET-CLIENT CLIENT)) (RESOURCE (RESOURCE-GET-ID RESOURCE)))
    (FUNCALL 'SET_BUFFER_SCALE (IFACE CLIENT RESOURCE) CLIENT ID)))

(CL-ASYNC-UTIL:DEFINE-C-CALLBACK DAMAGE_BUFFER-FFI
    :VOID
    ((CLIENT :POINTER) (RESOURCE :POINTER) (X :INT) (Y :INT) (WIDTH :INT)
     (HEIGHT :INT))
  (LET ((CLIENT (GET-CLIENT CLIENT)) (RESOURCE (RESOURCE-GET-ID RESOURCE)))
    (FUNCALL 'DAMAGE_BUFFER (IFACE CLIENT RESOURCE) CLIENT ID)))

(CL-ASYNC-UTIL:DEFINE-C-CALLBACK OFFSET-FFI
    :VOID
    ((CLIENT :POINTER) (RESOURCE :POINTER) (X :INT) (Y :INT))
  (LET ((CLIENT (GET-CLIENT CLIENT)) (RESOURCE (RESOURCE-GET-ID RESOURCE)))
    (FUNCALL 'OFFSET (IFACE CLIENT RESOURCE) CLIENT ID)))

(DEFPACKAGE :WL/WL_SEAT
  (:USE :CL :WL :CFFI)
  (:EXPORT DISPATCH GLOBAL))

(IN-PACKAGE :WL/WL_SEAT)

(DEFCLASS DISPATCH (BM-CL-WAYLAND:OBJECT) NIL (:DEFAULT-INITARGS :VERSION 9)
          (:DOCUMENTATION "group of input devices

A seat is a group of keyboards, pointer and touch devices. This
      object is published as a global during start up, or when such a
      device is hot plugged.  A seat typically has a pointer and
      maintains a keyboard focus and a pointer focus.
"))

(DEFVAR *INTERFACE* NIL)

(DEFCSTRUCT INTERFACE (GET_POINTER :POINTER) (GET_KEYBOARD :POINTER)
 (GET_TOUCH :POINTER) (RELEASE :POINTER))

(DEFGENERIC GET_POINTER
    (RESOURCE CLIENT ID))

(DEFGENERIC GET_KEYBOARD
    (RESOURCE CLIENT ID))

(DEFGENERIC GET_TOUCH
    (RESOURCE CLIENT ID))

(DEFGENERIC RELEASE
    (RESOURCE CLIENT))

(CL-ASYNC-UTIL:DEFINE-C-CALLBACK GET_POINTER-FFI
    :VOID
    ((CLIENT :POINTER) (RESOURCE :POINTER) (ID :UINT))
  (LET ((CLIENT (GET-CLIENT CLIENT)) (RESOURCE (RESOURCE-GET-ID RESOURCE)))
    (FUNCALL 'GET_POINTER (IFACE CLIENT RESOURCE) CLIENT ID)))

(CL-ASYNC-UTIL:DEFINE-C-CALLBACK GET_KEYBOARD-FFI
    :VOID
    ((CLIENT :POINTER) (RESOURCE :POINTER) (ID :UINT))
  (LET ((CLIENT (GET-CLIENT CLIENT)) (RESOURCE (RESOURCE-GET-ID RESOURCE)))
    (FUNCALL 'GET_KEYBOARD (IFACE CLIENT RESOURCE) CLIENT ID)))

(CL-ASYNC-UTIL:DEFINE-C-CALLBACK GET_TOUCH-FFI
    :VOID
    ((CLIENT :POINTER) (RESOURCE :POINTER) (ID :UINT))
  (LET ((CLIENT (GET-CLIENT CLIENT)) (RESOURCE (RESOURCE-GET-ID RESOURCE)))
    (FUNCALL 'GET_TOUCH (IFACE CLIENT RESOURCE) CLIENT ID)))

(CL-ASYNC-UTIL:DEFINE-C-CALLBACK RELEASE-FFI
    :VOID
    ((CLIENT :POINTER) (RESOURCE :POINTER))
  (LET ((CLIENT (GET-CLIENT CLIENT)) (RESOURCE (RESOURCE-GET-ID RESOURCE)))
    (FUNCALL 'RELEASE (IFACE CLIENT RESOURCE) CLIENT ID)))

(DEFCLASS GLOBAL (BM-CL-WAYLAND::GLOBAL) NIL (:DEFAULT-INITARGS :VERSION 9)
          (:DOCUMENTATION "group of input devices

A seat is a group of keyboards, pointer and touch devices. This
      object is published as a global during start up, or when such a
      device is hot plugged.  A seat typically has a pointer and
      maintains a keyboard focus and a pointer focus.
"))

(DEFMETHOD DISPATCH-BIND ((GLOBAL GLOBAL) CLIENT DATA VERSION ID)
  "Default bind implementation for the ,(name interface) global object.
This can be overriden by inheritance in case if custom behaviour is required."
  (LET ((BOUND (MAKE-INSTANCE 'DISPATCH (DISPLAY CLIENT))))
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

(DEFPACKAGE :WL/WL_POINTER
  (:USE :CL :WL :CFFI)
  (:EXPORT DISPATCH GLOBAL))

(IN-PACKAGE :WL/WL_POINTER)

(DEFCLASS DISPATCH (BM-CL-WAYLAND:OBJECT) NIL (:DEFAULT-INITARGS :VERSION 9)
          (:DOCUMENTATION "pointer input device

The wl_pointer interface represents one or more input devices,
      such as mice, which control the pointer location and pointer_focus
      of a seat.

      The wl_pointer interface generates motion, enter and leave
      events for the surfaces that the pointer is located over,
      and button and axis events for button presses, button releases
      and scrolling.
"))

(DEFVAR *INTERFACE* NIL)

(DEFCSTRUCT INTERFACE (SET_CURSOR :POINTER) (RELEASE :POINTER))

(DEFGENERIC SET_CURSOR
    (RESOURCE CLIENT SERIAL SURFACE HOTSPOT_X HOTSPOT_Y))

(DEFGENERIC RELEASE
    (RESOURCE CLIENT))

(CL-ASYNC-UTIL:DEFINE-C-CALLBACK SET_CURSOR-FFI
    :VOID
    ((CLIENT :POINTER) (RESOURCE :POINTER) (SERIAL :UINT) (SURFACE :UINT)
     (HOTSPOT_X :INT) (HOTSPOT_Y :INT))
  (LET ((CLIENT (GET-CLIENT CLIENT)) (RESOURCE (RESOURCE-GET-ID RESOURCE)))
    (FUNCALL 'SET_CURSOR (IFACE CLIENT RESOURCE) CLIENT ID)))

(CL-ASYNC-UTIL:DEFINE-C-CALLBACK RELEASE-FFI
    :VOID
    ((CLIENT :POINTER) (RESOURCE :POINTER))
  (LET ((CLIENT (GET-CLIENT CLIENT)) (RESOURCE (RESOURCE-GET-ID RESOURCE)))
    (FUNCALL 'RELEASE (IFACE CLIENT RESOURCE) CLIENT ID)))

(DEFPACKAGE :WL/WL_KEYBOARD
  (:USE :CL :WL :CFFI)
  (:EXPORT DISPATCH GLOBAL))

(IN-PACKAGE :WL/WL_KEYBOARD)

(DEFCLASS DISPATCH (BM-CL-WAYLAND:OBJECT) NIL (:DEFAULT-INITARGS :VERSION 9)
          (:DOCUMENTATION "keyboard input device

The wl_keyboard interface represents one or more keyboards
      associated with a seat.
"))

(DEFVAR *INTERFACE* NIL)

(DEFCSTRUCT INTERFACE (RELEASE :POINTER))

(DEFGENERIC RELEASE
    (RESOURCE CLIENT))

(CL-ASYNC-UTIL:DEFINE-C-CALLBACK RELEASE-FFI
    :VOID
    ((CLIENT :POINTER) (RESOURCE :POINTER))
  (LET ((CLIENT (GET-CLIENT CLIENT)) (RESOURCE (RESOURCE-GET-ID RESOURCE)))
    (FUNCALL 'RELEASE (IFACE CLIENT RESOURCE) CLIENT ID)))

(DEFPACKAGE :WL/WL_TOUCH
  (:USE :CL :WL :CFFI)
  (:EXPORT DISPATCH GLOBAL))

(IN-PACKAGE :WL/WL_TOUCH)

(DEFCLASS DISPATCH (BM-CL-WAYLAND:OBJECT) NIL (:DEFAULT-INITARGS :VERSION 9)
          (:DOCUMENTATION "touchscreen input device

The wl_touch interface represents a touchscreen
      associated with a seat.

      Touch interactions can consist of one or more contacts.
      For each contact, a series of events is generated, starting
      with a down event, followed by zero or more motion events,
      and ending with an up event. Events relating to the same
      contact point can be identified by the ID of the sequence.
"))

(DEFVAR *INTERFACE* NIL)

(DEFCSTRUCT INTERFACE (RELEASE :POINTER))

(DEFGENERIC RELEASE
    (RESOURCE CLIENT))

(CL-ASYNC-UTIL:DEFINE-C-CALLBACK RELEASE-FFI
    :VOID
    ((CLIENT :POINTER) (RESOURCE :POINTER))
  (LET ((CLIENT (GET-CLIENT CLIENT)) (RESOURCE (RESOURCE-GET-ID RESOURCE)))
    (FUNCALL 'RELEASE (IFACE CLIENT RESOURCE) CLIENT ID)))

(DEFPACKAGE :WL/WL_OUTPUT
  (:USE :CL :WL :CFFI)
  (:EXPORT DISPATCH GLOBAL))

(IN-PACKAGE :WL/WL_OUTPUT)

(DEFCLASS DISPATCH (BM-CL-WAYLAND:OBJECT) NIL (:DEFAULT-INITARGS :VERSION 4)
          (:DOCUMENTATION "compositor output region

An output describes part of the compositor geometry.  The
      compositor works in the 'compositor coordinate system' and an
      output corresponds to a rectangular area in that space that is
      actually visible.  This typically corresponds to a monitor that
      displays part of the compositor space.  This object is published
      as global during start up, or when a monitor is hotplugged.
"))

(DEFVAR *INTERFACE* NIL)

(DEFCSTRUCT INTERFACE (RELEASE :POINTER))

(DEFGENERIC RELEASE
    (RESOURCE CLIENT))

(CL-ASYNC-UTIL:DEFINE-C-CALLBACK RELEASE-FFI
    :VOID
    ((CLIENT :POINTER) (RESOURCE :POINTER))
  (LET ((CLIENT (GET-CLIENT CLIENT)) (RESOURCE (RESOURCE-GET-ID RESOURCE)))
    (FUNCALL 'RELEASE (IFACE CLIENT RESOURCE) CLIENT ID)))

(DEFCLASS GLOBAL (BM-CL-WAYLAND::GLOBAL) NIL (:DEFAULT-INITARGS :VERSION 4)
          (:DOCUMENTATION "compositor output region

An output describes part of the compositor geometry.  The
      compositor works in the 'compositor coordinate system' and an
      output corresponds to a rectangular area in that space that is
      actually visible.  This typically corresponds to a monitor that
      displays part of the compositor space.  This object is published
      as global during start up, or when a monitor is hotplugged.
"))

(DEFMETHOD DISPATCH-BIND ((GLOBAL GLOBAL) CLIENT DATA VERSION ID)
  "Default bind implementation for the ,(name interface) global object.
This can be overriden by inheritance in case if custom behaviour is required."
  (LET ((BOUND (MAKE-INSTANCE 'DISPATCH (DISPLAY CLIENT))))
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

(DEFPACKAGE :WL/WL_REGION
  (:USE :CL :WL :CFFI)
  (:EXPORT DISPATCH GLOBAL))

(IN-PACKAGE :WL/WL_REGION)

(DEFCLASS DISPATCH (BM-CL-WAYLAND:OBJECT) NIL (:DEFAULT-INITARGS :VERSION 1)
          (:DOCUMENTATION "region interface

A region object describes an area.

      Region objects are used to describe the opaque and input
      regions of a surface.
"))

(DEFVAR *INTERFACE* NIL)

(DEFCSTRUCT INTERFACE (DESTROY :POINTER) (ADD :POINTER) (SUBTRACT :POINTER))

(DEFGENERIC DESTROY
    (RESOURCE CLIENT))

(DEFGENERIC ADD
    (RESOURCE CLIENT X Y WIDTH HEIGHT))

(DEFGENERIC SUBTRACT
    (RESOURCE CLIENT X Y WIDTH HEIGHT))

(CL-ASYNC-UTIL:DEFINE-C-CALLBACK DESTROY-FFI
    :VOID
    ((CLIENT :POINTER) (RESOURCE :POINTER))
  (LET ((CLIENT (GET-CLIENT CLIENT)) (RESOURCE (RESOURCE-GET-ID RESOURCE)))
    (FUNCALL 'DESTROY (IFACE CLIENT RESOURCE) CLIENT ID)))

(CL-ASYNC-UTIL:DEFINE-C-CALLBACK ADD-FFI
    :VOID
    ((CLIENT :POINTER) (RESOURCE :POINTER) (X :INT) (Y :INT) (WIDTH :INT)
     (HEIGHT :INT))
  (LET ((CLIENT (GET-CLIENT CLIENT)) (RESOURCE (RESOURCE-GET-ID RESOURCE)))
    (FUNCALL 'ADD (IFACE CLIENT RESOURCE) CLIENT ID)))

(CL-ASYNC-UTIL:DEFINE-C-CALLBACK SUBTRACT-FFI
    :VOID
    ((CLIENT :POINTER) (RESOURCE :POINTER) (X :INT) (Y :INT) (WIDTH :INT)
     (HEIGHT :INT))
  (LET ((CLIENT (GET-CLIENT CLIENT)) (RESOURCE (RESOURCE-GET-ID RESOURCE)))
    (FUNCALL 'SUBTRACT (IFACE CLIENT RESOURCE) CLIENT ID)))

(DEFPACKAGE :WL/WL_SUBCOMPOSITOR
  (:USE :CL :WL :CFFI)
  (:EXPORT DISPATCH GLOBAL))

(IN-PACKAGE :WL/WL_SUBCOMPOSITOR)

(DEFCLASS DISPATCH (BM-CL-WAYLAND:OBJECT) NIL (:DEFAULT-INITARGS :VERSION 1)
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

(DEFVAR *INTERFACE* NIL)

(DEFCSTRUCT INTERFACE (DESTROY :POINTER) (GET_SUBSURFACE :POINTER))

(DEFGENERIC DESTROY
    (RESOURCE CLIENT))

(DEFGENERIC GET_SUBSURFACE
    (RESOURCE CLIENT ID SURFACE PARENT))

(CL-ASYNC-UTIL:DEFINE-C-CALLBACK DESTROY-FFI
    :VOID
    ((CLIENT :POINTER) (RESOURCE :POINTER))
  (LET ((CLIENT (GET-CLIENT CLIENT)) (RESOURCE (RESOURCE-GET-ID RESOURCE)))
    (FUNCALL 'DESTROY (IFACE CLIENT RESOURCE) CLIENT ID)))

(CL-ASYNC-UTIL:DEFINE-C-CALLBACK GET_SUBSURFACE-FFI
    :VOID
    ((CLIENT :POINTER) (RESOURCE :POINTER) (ID :UINT) (SURFACE :UINT)
     (PARENT :UINT))
  (LET ((CLIENT (GET-CLIENT CLIENT)) (RESOURCE (RESOURCE-GET-ID RESOURCE)))
    (FUNCALL 'GET_SUBSURFACE (IFACE CLIENT RESOURCE) CLIENT ID)))

(DEFCLASS GLOBAL (BM-CL-WAYLAND::GLOBAL) NIL (:DEFAULT-INITARGS :VERSION 1)
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
  "Default bind implementation for the ,(name interface) global object.
This can be overriden by inheritance in case if custom behaviour is required."
  (LET ((BOUND (MAKE-INSTANCE 'DISPATCH (DISPLAY CLIENT))))
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

(DEFPACKAGE :WL/WL_SUBSURFACE
  (:USE :CL :WL :CFFI)
  (:EXPORT DISPATCH GLOBAL))

(IN-PACKAGE :WL/WL_SUBSURFACE)

(DEFCLASS DISPATCH (BM-CL-WAYLAND:OBJECT) NIL (:DEFAULT-INITARGS :VERSION 1)
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

(DEFVAR *INTERFACE* NIL)

(DEFCSTRUCT INTERFACE (DESTROY :POINTER) (SET_POSITION :POINTER)
 (PLACE_ABOVE :POINTER) (PLACE_BELOW :POINTER) (SET_SYNC :POINTER)
 (SET_DESYNC :POINTER))

(DEFGENERIC DESTROY
    (RESOURCE CLIENT))

(DEFGENERIC SET_POSITION
    (RESOURCE CLIENT X Y))

(DEFGENERIC PLACE_ABOVE
    (RESOURCE CLIENT SIBLING))

(DEFGENERIC PLACE_BELOW
    (RESOURCE CLIENT SIBLING))

(DEFGENERIC SET_SYNC
    (RESOURCE CLIENT))

(DEFGENERIC SET_DESYNC
    (RESOURCE CLIENT))

(CL-ASYNC-UTIL:DEFINE-C-CALLBACK DESTROY-FFI
    :VOID
    ((CLIENT :POINTER) (RESOURCE :POINTER))
  (LET ((CLIENT (GET-CLIENT CLIENT)) (RESOURCE (RESOURCE-GET-ID RESOURCE)))
    (FUNCALL 'DESTROY (IFACE CLIENT RESOURCE) CLIENT ID)))

(CL-ASYNC-UTIL:DEFINE-C-CALLBACK SET_POSITION-FFI
    :VOID
    ((CLIENT :POINTER) (RESOURCE :POINTER) (X :INT) (Y :INT))
  (LET ((CLIENT (GET-CLIENT CLIENT)) (RESOURCE (RESOURCE-GET-ID RESOURCE)))
    (FUNCALL 'SET_POSITION (IFACE CLIENT RESOURCE) CLIENT ID)))

(CL-ASYNC-UTIL:DEFINE-C-CALLBACK PLACE_ABOVE-FFI
    :VOID
    ((CLIENT :POINTER) (RESOURCE :POINTER) (SIBLING :UINT))
  (LET ((CLIENT (GET-CLIENT CLIENT)) (RESOURCE (RESOURCE-GET-ID RESOURCE)))
    (FUNCALL 'PLACE_ABOVE (IFACE CLIENT RESOURCE) CLIENT ID)))

(CL-ASYNC-UTIL:DEFINE-C-CALLBACK PLACE_BELOW-FFI
    :VOID
    ((CLIENT :POINTER) (RESOURCE :POINTER) (SIBLING :UINT))
  (LET ((CLIENT (GET-CLIENT CLIENT)) (RESOURCE (RESOURCE-GET-ID RESOURCE)))
    (FUNCALL 'PLACE_BELOW (IFACE CLIENT RESOURCE) CLIENT ID)))

(CL-ASYNC-UTIL:DEFINE-C-CALLBACK SET_SYNC-FFI
    :VOID
    ((CLIENT :POINTER) (RESOURCE :POINTER))
  (LET ((CLIENT (GET-CLIENT CLIENT)) (RESOURCE (RESOURCE-GET-ID RESOURCE)))
    (FUNCALL 'SET_SYNC (IFACE CLIENT RESOURCE) CLIENT ID)))

(CL-ASYNC-UTIL:DEFINE-C-CALLBACK SET_DESYNC-FFI
    :VOID
    ((CLIENT :POINTER) (RESOURCE :POINTER))
  (LET ((CLIENT (GET-CLIENT CLIENT)) (RESOURCE (RESOURCE-GET-ID RESOURCE)))
    (FUNCALL 'SET_DESYNC (IFACE CLIENT RESOURCE) CLIENT ID)))

