(DEFPACKAGE :ZWP_LINUX_DMABUF_V1
  (:USE :CL :CFFI :CL-WL.FFI)
  (:NICKNAMES :ZWP-LINUX-DMABUF-V1)
  (:EXPORT DISPATCH
           GLOBAL
           DISPATCH-BIND
           DESTROY
           CREATE-PARAMS
           GET-DEFAULT-FEEDBACK
           GET-SURFACE-FEEDBACK
           SEND-FORMAT
           SEND-MODIFIER))

(IN-PACKAGE :ZWP_LINUX_DMABUF_V1)

(DEFCSTRUCT INTERFACE (NAME :STRING) (VERSION :INT) (METHOD_COUNT :INT)
 (METHODS (:POINTER (:STRUCT CL-WL.FFI:WL_MESSAGE))) (EVENT_COUNT :INT)
 (EVENTS (:POINTER (:STRUCT CL-WL.FFI:WL_MESSAGE))))

(DEFVAR *INTERFACE* NIL)

(DEFPACKAGE :ZWP_LINUX_BUFFER_PARAMS_V1
  (:USE :CL :CFFI :CL-WL.FFI)
  (:NICKNAMES :ZWP-LINUX-BUFFER-PARAMS-V1)
  (:EXPORT DISPATCH
           GLOBAL
           DISPATCH-BIND
           DESTROY
           ADD
           CREATE
           CREATE-IMMED
           SEND-CREATED
           SEND-FAILED))

(IN-PACKAGE :ZWP_LINUX_BUFFER_PARAMS_V1)

(DEFCSTRUCT INTERFACE (NAME :STRING) (VERSION :INT) (METHOD_COUNT :INT)
 (METHODS (:POINTER (:STRUCT CL-WL.FFI:WL_MESSAGE))) (EVENT_COUNT :INT)
 (EVENTS (:POINTER (:STRUCT CL-WL.FFI:WL_MESSAGE))))

(DEFVAR *INTERFACE* NIL)

(DEFPACKAGE :ZWP_LINUX_DMABUF_FEEDBACK_V1
  (:USE :CL :CFFI :CL-WL.FFI)
  (:NICKNAMES :ZWP-LINUX-DMABUF-FEEDBACK-V1)
  (:EXPORT DISPATCH
           GLOBAL
           DISPATCH-BIND
           DESTROY
           SEND-DONE
           SEND-FORMAT-TABLE
           SEND-MAIN-DEVICE
           SEND-TRANCHE-DONE
           SEND-TRANCHE-TARGET-DEVICE
           SEND-TRANCHE-FORMATS
           SEND-TRANCHE-FLAGS))

(IN-PACKAGE :ZWP_LINUX_DMABUF_FEEDBACK_V1)

(DEFCSTRUCT INTERFACE (NAME :STRING) (VERSION :INT) (METHOD_COUNT :INT)
 (METHODS (:POINTER (:STRUCT CL-WL.FFI:WL_MESSAGE))) (EVENT_COUNT :INT)
 (EVENTS (:POINTER (:STRUCT CL-WL.FFI:WL_MESSAGE))))

(DEFVAR *INTERFACE* NIL)

(IN-PACKAGE :ZWP_LINUX_DMABUF_V1)

(DEFCLASS DISPATCH (CL-WL:OBJECT)
          ((ZWP_LINUX_DMABUF_V1-ID :INITFORM NIL :ACCESSOR
            ZWP_LINUX_DMABUF_V1-ID)
           (ZWP_LINUX_DMABUF_V1-PTR :INITFORM NIL :ACCESSOR
            ZWP_LINUX_DMABUF_V1-PTR))
          (:DEFAULT-INITARGS :VERSION 5)
          (:DOCUMENTATION "factory for creating dmabuf-based wl_buffers

Following the interfaces from:
      https://www.khronos.org/registry/egl/extensions/EXT/EGL_EXT_image_dma_buf_import.txt
      https://www.khronos.org/registry/EGL/extensions/EXT/EGL_EXT_image_dma_buf_import_modifiers.txt
      and the Linux DRM sub-system's AddFb2 ioctl.

      This interface offers ways to create generic dmabuf-based wl_buffers.

      Clients can use the get_surface_feedback request to get dmabuf feedback
      for a particular surface. If the client wants to retrieve feedback not
      tied to a surface, they can use the get_default_feedback request.

      The following are required from clients:

      - Clients must ensure that either all data in the dma-buf is
        coherent for all subsequent read access or that coherency is
        correctly handled by the underlying kernel-side dma-buf
        implementation.

      - Don't make any more attachments after sending the buffer to the
        compositor. Making more attachments later increases the risk of
        the compositor not being able to use (re-import) an existing
        dmabuf-based wl_buffer.

      The underlying graphics stack must ensure the following:

      - The dmabuf file descriptors relayed to the server will stay valid
        for the whole lifetime of the wl_buffer. This means the server may
        at any time use those fds to import the dmabuf into any kernel
        sub-system that might accept it.

      However, when the underlying graphics stack fails to deliver the
      promise, because of e.g. a device hot-unplug which raises internal
      errors, after the wl_buffer has been successfully created the
      compositor must not raise protocol errors to the client when dmabuf
      import later fails.

      To create a wl_buffer from one or more dmabufs, a client creates a
      zwp_linux_dmabuf_params_v1 object with a zwp_linux_dmabuf_v1.create_params
      request. All planes required by the intended format are added with
      the 'add' request. Finally, a 'create' or 'create_immed' request is
      issued, which has the following outcome depending on the import success.

      The 'create' request,
      - on success, triggers a 'created' event which provides the final
        wl_buffer to the client.
      - on failure, triggers a 'failed' event to convey that the server
        cannot use the dmabufs received from the client.

      For the 'create_immed' request,
      - on success, the server immediately imports the added dmabufs to
        create a wl_buffer. No event is sent from the server in this case.
      - on failure, the server can choose to either:
        - terminate the client by raising a fatal error.
        - mark the wl_buffer as failed, and send a 'failed' event to the
          client. If the client uses a failed wl_buffer as an argument to any
          request, the behaviour is compositor implementation-defined.

      For all DRM formats and unless specified in another protocol extension,
      pre-multiplied alpha is used for pixel values.

      Unless specified otherwise in another protocol extension, implicit
      synchronization is used. In other words, compositors and clients must
      wait and signal fences implicitly passed via the DMA-BUF's reservation
      mechanism.
"))

(DEFGENERIC DESTROY
    (RESOURCE))

(DEFGENERIC CREATE-PARAMS
    (RESOURCE PARAMS_ID))

(DEFGENERIC GET-DEFAULT-FEEDBACK
    (RESOURCE ID))

(DEFGENERIC GET-SURFACE-FEEDBACK
    (RESOURCE ID SURFACE))

(DEFCONTINUE:DEFCONTINUE CL-WL:DESTROY ((DISPATCH DISPATCH))
                         (CL-WL::DEBUG-LOG! "Destroying dispatch object: ~a~%"
                                            "zwp_linux_dmabuf_v1")
                         (LET ((CLIENT (CL-WL:CLIENT DISPATCH))
                               (RESOURCE-PTR
                                (ZWP_LINUX_DMABUF_V1-PTR DISPATCH)))
                           (CL-WL:REMOVE-CLIENT-OBJECT CLIENT
                                                       (ZWP_LINUX_DMABUF_V1-ID
                                                        DISPATCH))
                           (CL-WL::REMOVE-RESOURCE
                            (POINTER-ADDRESS RESOURCE-PTR)))
                         (CL-WL::DN-IF DISPATCH))

(DEFMETHOD DESTROY ((DISPATCH DISPATCH)) (CL-WL:DESTROY DISPATCH))

(DEFMETHOD DESTROY (EMPTY) NIL)

(PUSHNEW
 (LIST "zwp_linux_dmabuf_v1"
       (LIST "zwp_linux_buffer_params_v1" "zwp_linux_dmabuf_feedback_v1"
             "wl_surface")
       (LAMBDA ()
         (SETF *INTERFACE* (FOREIGN-ALLOC '(:STRUCT INTERFACE)))
         (LET ((REQUESTS-PTR
                (LET ((MESSAGES
                       (CFFI:FOREIGN-ALLOC '(:STRUCT CL-WL.FFI:WL_MESSAGE)
                                           :COUNT 4)))
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
                            ZWP_LINUX_BUFFER_PARAMS_V1::*INTERFACE*)
                    (WITH-FOREIGN-SLOTS
                     ((NAME SIGNATURE TYPES) MSG-PTR
                      (:STRUCT CL-WL.FFI:WL_MESSAGE))
                     (SETF NAME (FOREIGN-STRING-ALLOC "create_params")
                           SIGNATURE "n"
                           TYPES INTERFACE-ARRAY)))
                  (LET ((INTERFACE-ARRAY
                         (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID))
                                             :COUNT 1))
                        (MSG-PTR
                         (MEM-APTR MESSAGES '(:STRUCT CL-WL.FFI:WL_MESSAGE) 2)))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 0)
                            ZWP_LINUX_DMABUF_FEEDBACK_V1::*INTERFACE*)
                    (WITH-FOREIGN-SLOTS
                     ((NAME SIGNATURE TYPES) MSG-PTR
                      (:STRUCT CL-WL.FFI:WL_MESSAGE))
                     (SETF NAME (FOREIGN-STRING-ALLOC "get_default_feedback")
                           SIGNATURE "4n"
                           TYPES INTERFACE-ARRAY)))
                  (LET ((INTERFACE-ARRAY
                         (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID))
                                             :COUNT 2))
                        (MSG-PTR
                         (MEM-APTR MESSAGES '(:STRUCT CL-WL.FFI:WL_MESSAGE) 3)))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 0)
                            ZWP_LINUX_DMABUF_FEEDBACK_V1::*INTERFACE*)
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 1)
                            WL_SURFACE::*INTERFACE*)
                    (WITH-FOREIGN-SLOTS
                     ((NAME SIGNATURE TYPES) MSG-PTR
                      (:STRUCT CL-WL.FFI:WL_MESSAGE))
                     (SETF NAME (FOREIGN-STRING-ALLOC "get_surface_feedback")
                           SIGNATURE "4no"
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
                     (SETF NAME (FOREIGN-STRING-ALLOC "format")
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
                     (SETF NAME (FOREIGN-STRING-ALLOC "modifier")
                           SIGNATURE "3uuu"
                           TYPES INTERFACE-ARRAY)))
                  MESSAGES)))
           (WITH-FOREIGN-SLOTS
            ((NAME VERSION METHOD_COUNT METHODS EVENT_COUNT EVENTS) *INTERFACE*
             (:STRUCT INTERFACE))
            (SETF NAME (FOREIGN-STRING-ALLOC "zwp_linux_dmabuf_v1")
                  VERSION 5
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
   (RESTART-CASE (ECASE OPCODE
                   (0
                    (CL-WL::DEBUG-LOG! "Dispatching ~a:~a~%"
                                       "zwp_linux_dmabuf_v1" "destroy")
                    (FUNCALL 'DESTROY RESOURCE))
                   (1
                    (CL-WL::DEBUG-LOG! "Dispatching ~a:~a~%"
                                       "zwp_linux_dmabuf_v1" "create-params")
                    (FUNCALL 'CREATE-PARAMS RESOURCE
                             (VALUES
                              (FOREIGN-SLOT-VALUE
                               (MEM-APTR ARGS '(:UNION CL-WL.FFI:WL_ARGUMENT)
                                0)
                               '(:UNION CL-WL.FFI:WL_ARGUMENT)
                               'WL-FFI::N))))
                   (2
                    (CL-WL::DEBUG-LOG! "Dispatching ~a:~a~%"
                                       "zwp_linux_dmabuf_v1"
                                       "get-default-feedback")
                    (FUNCALL 'GET-DEFAULT-FEEDBACK RESOURCE
                             (VALUES
                              (FOREIGN-SLOT-VALUE
                               (MEM-APTR ARGS '(:UNION CL-WL.FFI:WL_ARGUMENT)
                                0)
                               '(:UNION CL-WL.FFI:WL_ARGUMENT)
                               'WL-FFI::N))))
                   (3
                    (CL-WL::DEBUG-LOG! "Dispatching ~a:~a~%"
                                       "zwp_linux_dmabuf_v1"
                                       "get-surface-feedback")
                    (FUNCALL 'GET-SURFACE-FEEDBACK RESOURCE
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
  (UNLESS (ZWP_LINUX_DMABUF_V1-ID INSTANCE)
    (LET* ((RESOURCE
            (CL-WL::CREATE-RESOURCE (CL-WL:PTR (CL-WL:CLIENT INSTANCE))
                                    *INTERFACE* (VERSION INSTANCE)
                                    (OR ID 0))))
      (SETF (CL-WL::TRANSIENT-ID INSTANCE)
              (SETF (ZWP_LINUX_DMABUF_V1-ID INSTANCE)
                      (OR ID (CL-WL.FFI:RESOURCE-GET-ID RESOURCE))))
      (SETF (ZWP_LINUX_DMABUF_V1-PTR INSTANCE) RESOURCE)
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
            FORMAT)
    (RESOURCE-POST-EVENT-ARRAY (ZWP_LINUX_DMABUF_V1-PTR DISPATCH) 0
     ARG-LIST)))

(DEFMETHOD SEND-MODIFIER
           ((DISPATCH DISPATCH) FORMAT MODIFIER_HI
            MODIFIER_LO)
  (CL-WL::DEBUG-LOG! "Event: ~a~%" "modifier")
  (LET ((ARG-LIST (FOREIGN-ALLOC '(:UNION WL_ARGUMENT) :COUNT 3)))
    (SETF (FOREIGN-SLOT-VALUE
           (MEM-AREF ARG-LIST '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 0)
           '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 'WL-FFI::U)
            FORMAT)
    (SETF (FOREIGN-SLOT-VALUE
           (MEM-AREF ARG-LIST '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 1)
           '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 'WL-FFI::U)
            MODIFIER_HI)
    (SETF (FOREIGN-SLOT-VALUE
           (MEM-AREF ARG-LIST '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 2)
           '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 'WL-FFI::U)
            MODIFIER_LO)
    (RESOURCE-POST-EVENT-ARRAY (ZWP_LINUX_DMABUF_V1-PTR DISPATCH) 1
     ARG-LIST)))

(DEFCLASS GLOBAL (CL-WL:GLOBAL) NIL
          (:DEFAULT-INITARGS :VERSION 5 :DISPATCH-IMPL 'DISPATCH)
          (:DOCUMENTATION "factory for creating dmabuf-based wl_buffers

Following the interfaces from:
      https://www.khronos.org/registry/egl/extensions/EXT/EGL_EXT_image_dma_buf_import.txt
      https://www.khronos.org/registry/EGL/extensions/EXT/EGL_EXT_image_dma_buf_import_modifiers.txt
      and the Linux DRM sub-system's AddFb2 ioctl.

      This interface offers ways to create generic dmabuf-based wl_buffers.

      Clients can use the get_surface_feedback request to get dmabuf feedback
      for a particular surface. If the client wants to retrieve feedback not
      tied to a surface, they can use the get_default_feedback request.

      The following are required from clients:

      - Clients must ensure that either all data in the dma-buf is
        coherent for all subsequent read access or that coherency is
        correctly handled by the underlying kernel-side dma-buf
        implementation.

      - Don't make any more attachments after sending the buffer to the
        compositor. Making more attachments later increases the risk of
        the compositor not being able to use (re-import) an existing
        dmabuf-based wl_buffer.

      The underlying graphics stack must ensure the following:

      - The dmabuf file descriptors relayed to the server will stay valid
        for the whole lifetime of the wl_buffer. This means the server may
        at any time use those fds to import the dmabuf into any kernel
        sub-system that might accept it.

      However, when the underlying graphics stack fails to deliver the
      promise, because of e.g. a device hot-unplug which raises internal
      errors, after the wl_buffer has been successfully created the
      compositor must not raise protocol errors to the client when dmabuf
      import later fails.

      To create a wl_buffer from one or more dmabufs, a client creates a
      zwp_linux_dmabuf_params_v1 object with a zwp_linux_dmabuf_v1.create_params
      request. All planes required by the intended format are added with
      the 'add' request. Finally, a 'create' or 'create_immed' request is
      issued, which has the following outcome depending on the import success.

      The 'create' request,
      - on success, triggers a 'created' event which provides the final
        wl_buffer to the client.
      - on failure, triggers a 'failed' event to convey that the server
        cannot use the dmabufs received from the client.

      For the 'create_immed' request,
      - on success, the server immediately imports the added dmabufs to
        create a wl_buffer. No event is sent from the server in this case.
      - on failure, the server can choose to either:
        - terminate the client by raising a fatal error.
        - mark the wl_buffer as failed, and send a 'failed' event to the
          client. If the client uses a failed wl_buffer as an argument to any
          request, the behaviour is compositor implementation-defined.

      For all DRM formats and unless specified in another protocol extension,
      pre-multiplied alpha is used for pixel values.

      Unless specified otherwise in another protocol extension, implicit
      synchronization is used. In other words, compositors and clients must
      wait and signal fences implicitly passed via the DMA-BUF's reservation
      mechanism.
"))

(DEFMETHOD DISPATCH-BIND ((GLOBAL GLOBAL) CLIENT DATA VERSION ID)
  "Default bind implementation for the zwp_linux_dmabuf_v1 global object.
This can be overriden by inheritance in case if custom behaviour is required."
  (CL-WL::DEBUG-LOG! "Binding ~a~%" "zwp_linux_dmabuf_v1")
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
  (CL-WL::DEBUG-LOG! "Initializing global object: ~a~%" "zwp_linux_dmabuf_v1")
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

(IN-PACKAGE :ZWP_LINUX_BUFFER_PARAMS_V1)

(DEFCLASS DISPATCH (CL-WL:OBJECT)
          ((ZWP_LINUX_BUFFER_PARAMS_V1-ID :INITFORM NIL :ACCESSOR
            ZWP_LINUX_BUFFER_PARAMS_V1-ID)
           (ZWP_LINUX_BUFFER_PARAMS_V1-PTR :INITFORM NIL :ACCESSOR
            ZWP_LINUX_BUFFER_PARAMS_V1-PTR))
          (:DEFAULT-INITARGS :VERSION 5)
          (:DOCUMENTATION "parameters for creating a dmabuf-based wl_buffer

This temporary object is a collection of dmabufs and other
      parameters that together form a single logical buffer. The temporary
      object may eventually create one wl_buffer unless cancelled by
      destroying it before requesting 'create'.

      Single-planar formats only require one dmabuf, however
      multi-planar formats may require more than one dmabuf. For all
      formats, an 'add' request must be called once per plane (even if the
      underlying dmabuf fd is identical).

      You must use consecutive plane indices ('plane_idx' argument for 'add')
      from zero to the number of planes used by the drm_fourcc format code.
      All planes required by the format must be given exactly once, but can
      be given in any order. Each plane index can be set only once.
"))

(DEFGENERIC DESTROY
    (RESOURCE))

(DEFGENERIC ADD
    (RESOURCE FD PLANE_IDX OFFSET
     STRIDE MODIFIER_HI MODIFIER_LO))

(DEFGENERIC CREATE
    (RESOURCE WIDTH HEIGHT FORMAT
     FLAGS))

(DEFGENERIC CREATE-IMMED
    (RESOURCE BUFFER_ID WIDTH HEIGHT
     FORMAT FLAGS))

(DEFCONTINUE:DEFCONTINUE CL-WL:DESTROY ((DISPATCH DISPATCH))
                         (CL-WL::DEBUG-LOG! "Destroying dispatch object: ~a~%"
                                            "zwp_linux_buffer_params_v1")
                         (LET ((CLIENT (CL-WL:CLIENT DISPATCH))
                               (RESOURCE-PTR
                                (ZWP_LINUX_BUFFER_PARAMS_V1-PTR
                                 DISPATCH)))
                           (CL-WL:REMOVE-CLIENT-OBJECT CLIENT
                                                       (ZWP_LINUX_BUFFER_PARAMS_V1-ID
                                                        DISPATCH))
                           (CL-WL::REMOVE-RESOURCE
                            (POINTER-ADDRESS RESOURCE-PTR)))
                         (CL-WL::DN-IF DISPATCH))

(DEFMETHOD DESTROY ((DISPATCH DISPATCH)) (CL-WL:DESTROY DISPATCH))

(DEFMETHOD DESTROY (EMPTY) NIL)

(PUSHNEW
 (LIST "zwp_linux_buffer_params_v1" (LIST "wl_buffer")
       (LAMBDA ()
         (SETF *INTERFACE* (FOREIGN-ALLOC '(:STRUCT INTERFACE)))
         (LET ((REQUESTS-PTR
                (LET ((MESSAGES
                       (CFFI:FOREIGN-ALLOC '(:STRUCT CL-WL.FFI:WL_MESSAGE)
                                           :COUNT 4)))
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
                                             :COUNT 6))
                        (MSG-PTR
                         (MEM-APTR MESSAGES '(:STRUCT CL-WL.FFI:WL_MESSAGE) 1)))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 0) (NULL-POINTER))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 1) (NULL-POINTER))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 2) (NULL-POINTER))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 3) (NULL-POINTER))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 4) (NULL-POINTER))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 5) (NULL-POINTER))
                    (WITH-FOREIGN-SLOTS
                     ((NAME SIGNATURE TYPES) MSG-PTR
                      (:STRUCT CL-WL.FFI:WL_MESSAGE))
                     (SETF NAME (FOREIGN-STRING-ALLOC "add")
                           SIGNATURE "huuuuu"
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
                     (SETF NAME (FOREIGN-STRING-ALLOC "create")
                           SIGNATURE "iiuu"
                           TYPES INTERFACE-ARRAY)))
                  (LET ((INTERFACE-ARRAY
                         (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID))
                                             :COUNT 5))
                        (MSG-PTR
                         (MEM-APTR MESSAGES '(:STRUCT CL-WL.FFI:WL_MESSAGE) 3)))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 0)
                            WL_BUFFER::*INTERFACE*)
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 1) (NULL-POINTER))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 2) (NULL-POINTER))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 3) (NULL-POINTER))
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 4) (NULL-POINTER))
                    (WITH-FOREIGN-SLOTS
                     ((NAME SIGNATURE TYPES) MSG-PTR
                      (:STRUCT CL-WL.FFI:WL_MESSAGE))
                     (SETF NAME (FOREIGN-STRING-ALLOC "create_immed")
                           SIGNATURE "2niiuu"
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
                    (SETF (MEM-AREF INTERFACE-ARRAY :POINTER 0)
                            WL_BUFFER::*INTERFACE*)
                    (WITH-FOREIGN-SLOTS
                     ((NAME SIGNATURE TYPES) MSG-PTR
                      (:STRUCT CL-WL.FFI:WL_MESSAGE))
                     (SETF NAME (FOREIGN-STRING-ALLOC "created")
                           SIGNATURE "n"
                           TYPES INTERFACE-ARRAY)))
                  (LET ((INTERFACE-ARRAY
                         (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID))
                                             :COUNT 0))
                        (MSG-PTR
                         (MEM-APTR MESSAGES '(:STRUCT CL-WL.FFI:WL_MESSAGE) 1)))
                    (WITH-FOREIGN-SLOTS
                     ((NAME SIGNATURE TYPES) MSG-PTR
                      (:STRUCT CL-WL.FFI:WL_MESSAGE))
                     (SETF NAME (FOREIGN-STRING-ALLOC "failed")
                           SIGNATURE ""
                           TYPES INTERFACE-ARRAY)))
                  MESSAGES)))
           (WITH-FOREIGN-SLOTS
            ((NAME VERSION METHOD_COUNT METHODS EVENT_COUNT EVENTS) *INTERFACE*
             (:STRUCT INTERFACE))
            (SETF NAME (FOREIGN-STRING-ALLOC "zwp_linux_buffer_params_v1")
                  VERSION 5
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
   (RESTART-CASE (ECASE OPCODE
                   (0
                    (CL-WL::DEBUG-LOG! "Dispatching ~a:~a~%"
                                       "zwp_linux_buffer_params_v1" "destroy")
                    (FUNCALL 'DESTROY RESOURCE))
                   (1
                    (CL-WL::DEBUG-LOG! "Dispatching ~a:~a~%"
                                       "zwp_linux_buffer_params_v1" "add")
                    (FUNCALL 'ADD RESOURCE
                             (VALUES
                              (FOREIGN-SLOT-VALUE
                               (MEM-APTR ARGS '(:UNION CL-WL.FFI:WL_ARGUMENT)
                                0)
                               '(:UNION CL-WL.FFI:WL_ARGUMENT)
                               'WL-FFI::H))
                             (VALUES
                              (FOREIGN-SLOT-VALUE
                               (MEM-APTR ARGS '(:UNION CL-WL.FFI:WL_ARGUMENT)
                                1)
                               '(:UNION CL-WL.FFI:WL_ARGUMENT)
                               'WL-FFI::U))
                             (VALUES
                              (FOREIGN-SLOT-VALUE
                               (MEM-APTR ARGS '(:UNION CL-WL.FFI:WL_ARGUMENT)
                                2)
                               '(:UNION CL-WL.FFI:WL_ARGUMENT)
                               'WL-FFI::U))
                             (VALUES
                              (FOREIGN-SLOT-VALUE
                               (MEM-APTR ARGS '(:UNION CL-WL.FFI:WL_ARGUMENT)
                                3)
                               '(:UNION CL-WL.FFI:WL_ARGUMENT)
                               'WL-FFI::U))
                             (VALUES
                              (FOREIGN-SLOT-VALUE
                               (MEM-APTR ARGS '(:UNION CL-WL.FFI:WL_ARGUMENT)
                                4)
                               '(:UNION CL-WL.FFI:WL_ARGUMENT)
                               'WL-FFI::U))
                             (VALUES
                              (FOREIGN-SLOT-VALUE
                               (MEM-APTR ARGS '(:UNION CL-WL.FFI:WL_ARGUMENT)
                                5)
                               '(:UNION CL-WL.FFI:WL_ARGUMENT)
                               'WL-FFI::U))))
                   (2
                    (CL-WL::DEBUG-LOG! "Dispatching ~a:~a~%"
                                       "zwp_linux_buffer_params_v1" "create")
                    (FUNCALL 'CREATE RESOURCE
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
                               'WL-FFI::U))
                             (FLAGS-FROM-VALUE
                              (FOREIGN-SLOT-VALUE
                               (MEM-APTR ARGS '(:UNION CL-WL.FFI:WL_ARGUMENT)
                                3)
                               '(:UNION CL-WL.FFI:WL_ARGUMENT)
                               'WL-FFI::U))))
                   (3
                    (CL-WL::DEBUG-LOG! "Dispatching ~a:~a~%"
                                       "zwp_linux_buffer_params_v1"
                                       "create-immed")
                    (FUNCALL 'CREATE-IMMED RESOURCE
                             (VALUES
                              (FOREIGN-SLOT-VALUE
                               (MEM-APTR ARGS '(:UNION CL-WL.FFI:WL_ARGUMENT)
                                0)
                               '(:UNION CL-WL.FFI:WL_ARGUMENT)
                               'WL-FFI::N))
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
                               'WL-FFI::U))
                             (FLAGS-FROM-VALUE
                              (FOREIGN-SLOT-VALUE
                               (MEM-APTR ARGS '(:UNION CL-WL.FFI:WL_ARGUMENT)
                                4)
                               '(:UNION CL-WL.FFI:WL_ARGUMENT)
                               'WL-FFI::U)))))
     (KILL-CLIENT NIL :REPORT "Kill the client causing errors"
      (CL-WL:DESTROY-CLIENT (CL-WL:CLIENT RESOURCE)) 0)))
 0)

(DEFVAR *DISPATCHER* (CALLBACK DISPATCHER-FFI))

(DEFMETHOD SHARED-INITIALIZE :AFTER ((INSTANCE DISPATCH) SLOT-NAMES &KEY ID)
  (DECLARE (IGNORE SLOT-NAMES))
  (UNLESS (ZWP_LINUX_BUFFER_PARAMS_V1-ID INSTANCE)
    (LET* ((RESOURCE
            (CL-WL::CREATE-RESOURCE (CL-WL:PTR (CL-WL:CLIENT INSTANCE))
                                    *INTERFACE* (VERSION INSTANCE)
                                    (OR ID 0))))
      (SETF (CL-WL::TRANSIENT-ID INSTANCE)
              (SETF (ZWP_LINUX_BUFFER_PARAMS_V1-ID INSTANCE)
                      (OR ID (CL-WL.FFI:RESOURCE-GET-ID RESOURCE))))
      (SETF (ZWP_LINUX_BUFFER_PARAMS_V1-PTR INSTANCE) RESOURCE)
      (SETF (GETHASH (POINTER-ADDRESS RESOURCE) CL-WL::*RESOURCE-TRACKER*)
              INSTANCE)
      (RESOURCE-SET-DISPATCHER RESOURCE *DISPATCHER* (NULL-POINTER)
       (NULL-POINTER) (NULL-POINTER)))))

(DEFMETHOD SEND-CREATED ((DISPATCH DISPATCH) BUFFER)
  (CL-WL::DEBUG-LOG! "Event: ~a~%" "created")
  (LET ((ARG-LIST (FOREIGN-ALLOC '(:UNION WL_ARGUMENT) :COUNT 1)))
    (SETF (FOREIGN-SLOT-VALUE
           (MEM-AREF ARG-LIST '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 0)
           '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 'WL-FFI::N)
            (WL_BUFFER::WL_BUFFER-PTR BUFFER))
    (RESOURCE-POST-EVENT-ARRAY
     (ZWP_LINUX_BUFFER_PARAMS_V1-PTR DISPATCH) 0 ARG-LIST)))

(DEFMETHOD SEND-FAILED ((DISPATCH DISPATCH))
  (CL-WL::DEBUG-LOG! "Event: ~a~%" "failed")
  (LET ((ARG-LIST (FOREIGN-ALLOC '(:UNION WL_ARGUMENT) :COUNT 0)))
    (RESOURCE-POST-EVENT-ARRAY
     (ZWP_LINUX_BUFFER_PARAMS_V1-PTR DISPATCH) 1 ARG-LIST)))

(DEFUN ERROR-FROM-VALUE (NUMBER)
  (LOOP FOR ENTRY IN '((0 :ALREADY-USED) (1 :PLANE-IDX)
                       (2 :PLANE-SET) (3 :INCOMPLETE)
                       (4 :INVALID-FORMAT)
                       (5 :INVALID-DIMENSIONS)
                       (6 :OUT-OF-BOUNDS)
                       (7 :INVALID-WL-BUFFER))
        FOR VALUE = (CAR ENTRY)
        FOR KEYWORD = (CADR ENTRY)
        WHEN (EQ NUMBER VALUE) RETURN KEYWORD
        FINALLY (ERROR (FORMAT NIL "Unknown enum value: ~a" NUMBER))))

(DEFUN ERROR-TO-VALUE (KEY)
  (LOOP FOR ENTRY IN '((0 :ALREADY-USED) (1 :PLANE-IDX)
                       (2 :PLANE-SET) (3 :INCOMPLETE)
                       (4 :INVALID-FORMAT)
                       (5 :INVALID-DIMENSIONS)
                       (6 :OUT-OF-BOUNDS)
                       (7 :INVALID-WL-BUFFER))
        FOR VALUE = (CAR ENTRY)
        FOR KEYWORD = (CADR ENTRY)
        WHEN (EQ KEY KEYWORD) RETURN VALUE
        FINALLY (ERROR (FORMAT NIL "Unknown enum keyword: ~a" KEY))))

(DEFUN FLAGS-FROM-VALUE (BITS)
  (LOOP FOR ENTRY IN '((1 :Y-INVERT) (2 :INTERLACED)
                       (4 :BOTTOM-FIRST))
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

(DEFUN FLAGS-TO-VALUE (KEYWORDS)
  (REDUCE #'+ KEYWORDS :KEY
          (LAMBDA (KEYWORD)
            (OR
             (CADR
              (ASSOC KEYWORD
                     '((:Y-INVERT 1) (:INTERLACED 2)
                       (:BOTTOM-FIRST 4))))
             0))))

(DEFCLASS GLOBAL (CL-WL:GLOBAL) NIL
          (:DEFAULT-INITARGS :VERSION 5 :DISPATCH-IMPL 'DISPATCH)
          (:DOCUMENTATION "parameters for creating a dmabuf-based wl_buffer

This temporary object is a collection of dmabufs and other
      parameters that together form a single logical buffer. The temporary
      object may eventually create one wl_buffer unless cancelled by
      destroying it before requesting 'create'.

      Single-planar formats only require one dmabuf, however
      multi-planar formats may require more than one dmabuf. For all
      formats, an 'add' request must be called once per plane (even if the
      underlying dmabuf fd is identical).

      You must use consecutive plane indices ('plane_idx' argument for 'add')
      from zero to the number of planes used by the drm_fourcc format code.
      All planes required by the format must be given exactly once, but can
      be given in any order. Each plane index can be set only once.
"))

(DEFMETHOD DISPATCH-BIND ((GLOBAL GLOBAL) CLIENT DATA VERSION ID)
  "Default bind implementation for the zwp_linux_buffer_params_v1 global object.
This can be overriden by inheritance in case if custom behaviour is required."
  (CL-WL::DEBUG-LOG! "Binding ~a~%" "zwp_linux_buffer_params_v1")
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
                     "zwp_linux_buffer_params_v1")
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

(IN-PACKAGE :ZWP_LINUX_DMABUF_FEEDBACK_V1)

(DEFCLASS DISPATCH (CL-WL:OBJECT)
          ((ZWP_LINUX_DMABUF_FEEDBACK_V1-ID :INITFORM NIL :ACCESSOR
            ZWP_LINUX_DMABUF_FEEDBACK_V1-ID)
           (ZWP_LINUX_DMABUF_FEEDBACK_V1-PTR :INITFORM NIL
            :ACCESSOR ZWP_LINUX_DMABUF_FEEDBACK_V1-PTR))
          (:DEFAULT-INITARGS :VERSION 5)
          (:DOCUMENTATION "dmabuf feedback

This object advertises dmabuf parameters feedback. This includes the
      preferred devices and the supported formats/modifiers.

      The parameters are sent once when this object is created and whenever they
      change. The done event is always sent once after all parameters have been
      sent. When a single parameter changes, all parameters are re-sent by the
      compositor.

      Compositors can re-send the parameters when the current client buffer
      allocations are sub-optimal. Compositors should not re-send the
      parameters if re-allocating the buffers would not result in a more optimal
      configuration. In particular, compositors should avoid sending the exact
      same parameters multiple times in a row.

      The tranche_target_device and tranche_formats events are grouped by
      tranches of preference. For each tranche, a tranche_target_device, one
      tranche_flags and one or more tranche_formats events are sent, followed
      by a tranche_done event finishing the list. The tranches are sent in
      descending order of preference. All formats and modifiers in the same
      tranche have the same preference.

      To send parameters, the compositor sends one main_device event, tranches
      (each consisting of one tranche_target_device event, one tranche_flags
      event, tranche_formats events and then a tranche_done event), then one
      done event.
"))

(DEFGENERIC DESTROY
    (RESOURCE))

(DEFCONTINUE:DEFCONTINUE CL-WL:DESTROY ((DISPATCH DISPATCH))
                         (CL-WL::DEBUG-LOG! "Destroying dispatch object: ~a~%"
                                            "zwp_linux_dmabuf_feedback_v1")
                         (LET ((CLIENT (CL-WL:CLIENT DISPATCH))
                               (RESOURCE-PTR
                                (ZWP_LINUX_DMABUF_FEEDBACK_V1-PTR
                                 DISPATCH)))
                           (CL-WL:REMOVE-CLIENT-OBJECT CLIENT
                                                       (ZWP_LINUX_DMABUF_FEEDBACK_V1-ID
                                                        DISPATCH))
                           (CL-WL::REMOVE-RESOURCE
                            (POINTER-ADDRESS RESOURCE-PTR)))
                         (CL-WL::DN-IF DISPATCH))

(DEFMETHOD DESTROY ((DISPATCH DISPATCH)) (CL-WL:DESTROY DISPATCH))

(DEFMETHOD DESTROY (EMPTY) NIL)

(PUSHNEW
 (LIST "zwp_linux_dmabuf_feedback_v1" (LIST)
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
                                           :COUNT 7)))
                  (LET ((INTERFACE-ARRAY
                         (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID))
                                             :COUNT 0))
                        (MSG-PTR
                         (MEM-APTR MESSAGES '(:STRUCT CL-WL.FFI:WL_MESSAGE) 0)))
                    (WITH-FOREIGN-SLOTS
                     ((NAME SIGNATURE TYPES) MSG-PTR
                      (:STRUCT CL-WL.FFI:WL_MESSAGE))
                     (SETF NAME (FOREIGN-STRING-ALLOC "done")
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
                     (SETF NAME (FOREIGN-STRING-ALLOC "format_table")
                           SIGNATURE "hu"
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
                     (SETF NAME (FOREIGN-STRING-ALLOC "main_device")
                           SIGNATURE "a"
                           TYPES INTERFACE-ARRAY)))
                  (LET ((INTERFACE-ARRAY
                         (CFFI:FOREIGN-ALLOC '(:POINTER (:POINTER :VOID))
                                             :COUNT 0))
                        (MSG-PTR
                         (MEM-APTR MESSAGES '(:STRUCT CL-WL.FFI:WL_MESSAGE) 3)))
                    (WITH-FOREIGN-SLOTS
                     ((NAME SIGNATURE TYPES) MSG-PTR
                      (:STRUCT CL-WL.FFI:WL_MESSAGE))
                     (SETF NAME (FOREIGN-STRING-ALLOC "tranche_done")
                           SIGNATURE ""
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
                     (SETF NAME (FOREIGN-STRING-ALLOC "tranche_target_device")
                           SIGNATURE "a"
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
                     (SETF NAME (FOREIGN-STRING-ALLOC "tranche_formats")
                           SIGNATURE "a"
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
                     (SETF NAME (FOREIGN-STRING-ALLOC "tranche_flags")
                           SIGNATURE "u"
                           TYPES INTERFACE-ARRAY)))
                  MESSAGES)))
           (WITH-FOREIGN-SLOTS
            ((NAME VERSION METHOD_COUNT METHODS EVENT_COUNT EVENTS) *INTERFACE*
             (:STRUCT INTERFACE))
            (SETF NAME (FOREIGN-STRING-ALLOC "zwp_linux_dmabuf_feedback_v1")
                  VERSION 5
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
   (RESTART-CASE (ECASE OPCODE
                   (0
                    (CL-WL::DEBUG-LOG! "Dispatching ~a:~a~%"
                                       "zwp_linux_dmabuf_feedback_v1"
                                       "destroy")
                    (FUNCALL 'DESTROY RESOURCE)))
     (KILL-CLIENT NIL :REPORT "Kill the client causing errors"
      (CL-WL:DESTROY-CLIENT (CL-WL:CLIENT RESOURCE)) 0)))
 0)

(DEFVAR *DISPATCHER* (CALLBACK DISPATCHER-FFI))

(DEFMETHOD SHARED-INITIALIZE :AFTER ((INSTANCE DISPATCH) SLOT-NAMES &KEY ID)
  (DECLARE (IGNORE SLOT-NAMES))
  (UNLESS (ZWP_LINUX_DMABUF_FEEDBACK_V1-ID INSTANCE)
    (LET* ((RESOURCE
            (CL-WL::CREATE-RESOURCE (CL-WL:PTR (CL-WL:CLIENT INSTANCE))
                                    *INTERFACE* (VERSION INSTANCE)
                                    (OR ID 0))))
      (SETF (CL-WL::TRANSIENT-ID INSTANCE)
              (SETF (ZWP_LINUX_DMABUF_FEEDBACK_V1-ID INSTANCE)
                      (OR ID (CL-WL.FFI:RESOURCE-GET-ID RESOURCE))))
      (SETF (ZWP_LINUX_DMABUF_FEEDBACK_V1-PTR INSTANCE) RESOURCE)
      (SETF (GETHASH (POINTER-ADDRESS RESOURCE) CL-WL::*RESOURCE-TRACKER*)
              INSTANCE)
      (RESOURCE-SET-DISPATCHER RESOURCE *DISPATCHER* (NULL-POINTER)
       (NULL-POINTER) (NULL-POINTER)))))

(DEFMETHOD SEND-DONE ((DISPATCH DISPATCH))
  (CL-WL::DEBUG-LOG! "Event: ~a~%" "done")
  (LET ((ARG-LIST (FOREIGN-ALLOC '(:UNION WL_ARGUMENT) :COUNT 0)))
    (RESOURCE-POST-EVENT-ARRAY
     (ZWP_LINUX_DMABUF_FEEDBACK_V1-PTR DISPATCH) 0 ARG-LIST)))

(DEFMETHOD SEND-FORMAT-TABLE
           ((DISPATCH DISPATCH) FD SIZE)
  (CL-WL::DEBUG-LOG! "Event: ~a~%" "format_table")
  (LET ((ARG-LIST (FOREIGN-ALLOC '(:UNION WL_ARGUMENT) :COUNT 2)))
    (SETF (FOREIGN-SLOT-VALUE
           (MEM-AREF ARG-LIST '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 0)
           '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 'WL-FFI::H)
            FD)
    (SETF (FOREIGN-SLOT-VALUE
           (MEM-AREF ARG-LIST '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 1)
           '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 'WL-FFI::U)
            SIZE)
    (RESOURCE-POST-EVENT-ARRAY
     (ZWP_LINUX_DMABUF_FEEDBACK_V1-PTR DISPATCH) 1 ARG-LIST)))

(DEFMETHOD SEND-MAIN-DEVICE
           ((DISPATCH DISPATCH) DEVICE)
  (CL-WL::DEBUG-LOG! "Event: ~a~%" "main_device")
  (LET ((ARG-LIST (FOREIGN-ALLOC '(:UNION WL_ARGUMENT) :COUNT 1)))
    (SETF (FOREIGN-SLOT-VALUE
           (MEM-AREF ARG-LIST '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 0)
           '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 'WL-FFI::A)
            (LET* ((LENGTH (LENGTH DEVICE))
                   (STRUCT (FOREIGN-ALLOC '(:STRUCT WL_ARRAY)))
                   (DATA (FOREIGN-ALLOC :UINT64 :COUNT LENGTH)))
              (LOOP FOR INDEX BELOW LENGTH
                    DO (SETF (MEM-AREF DATA :UINT64 INDEX)
                               (NTH INDEX DEVICE)))
              (SETF (FOREIGN-SLOT-VALUE STRUCT '(:STRUCT WL_ARRAY)
                     'CL-WL.FFI::SIZE)
                      (* LENGTH (FOREIGN-TYPE-SIZE :UINT64))
                    (FOREIGN-SLOT-VALUE STRUCT '(:STRUCT WL_ARRAY)
                     'CL-WL.FFI::ALLOC)
                      LENGTH
                    (FOREIGN-SLOT-VALUE STRUCT '(:STRUCT WL_ARRAY)
                     'CL-WL.FFI::DATA)
                      DATA)
              STRUCT))
    (RESOURCE-POST-EVENT-ARRAY
     (ZWP_LINUX_DMABUF_FEEDBACK_V1-PTR DISPATCH) 2 ARG-LIST)))

(DEFMETHOD SEND-TRANCHE-DONE ((DISPATCH DISPATCH))
  (CL-WL::DEBUG-LOG! "Event: ~a~%" "tranche_done")
  (LET ((ARG-LIST (FOREIGN-ALLOC '(:UNION WL_ARGUMENT) :COUNT 0)))
    (RESOURCE-POST-EVENT-ARRAY
     (ZWP_LINUX_DMABUF_FEEDBACK_V1-PTR DISPATCH) 3 ARG-LIST)))

(DEFMETHOD SEND-TRANCHE-TARGET-DEVICE
           ((DISPATCH DISPATCH) DEVICE)
  (CL-WL::DEBUG-LOG! "Event: ~a~%" "tranche_target_device")
  (LET ((ARG-LIST (FOREIGN-ALLOC '(:UNION WL_ARGUMENT) :COUNT 1)))
    (SETF (FOREIGN-SLOT-VALUE
           (MEM-AREF ARG-LIST '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 0)
           '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 'WL-FFI::A)
            (LET* ((LENGTH (LENGTH DEVICE))
                   (STRUCT (FOREIGN-ALLOC '(:STRUCT WL_ARRAY)))
                   (DATA (FOREIGN-ALLOC :UINT64 :COUNT LENGTH)))
              (LOOP FOR INDEX BELOW LENGTH
                    DO (SETF (MEM-AREF DATA :UINT64 INDEX)
                               (NTH INDEX DEVICE)))
              (SETF (FOREIGN-SLOT-VALUE STRUCT '(:STRUCT WL_ARRAY)
                     'CL-WL.FFI::SIZE)
                      (* LENGTH (FOREIGN-TYPE-SIZE :UINT64))
                    (FOREIGN-SLOT-VALUE STRUCT '(:STRUCT WL_ARRAY)
                     'CL-WL.FFI::ALLOC)
                      LENGTH
                    (FOREIGN-SLOT-VALUE STRUCT '(:STRUCT WL_ARRAY)
                     'CL-WL.FFI::DATA)
                      DATA)
              STRUCT))
    (RESOURCE-POST-EVENT-ARRAY
     (ZWP_LINUX_DMABUF_FEEDBACK_V1-PTR DISPATCH) 4 ARG-LIST)))

(DEFMETHOD SEND-TRANCHE-FORMATS
           ((DISPATCH DISPATCH) INDICES)
  (CL-WL::DEBUG-LOG! "Event: ~a~%" "tranche_formats")
  (LET ((ARG-LIST (FOREIGN-ALLOC '(:UNION WL_ARGUMENT) :COUNT 1)))
    (SETF (FOREIGN-SLOT-VALUE
           (MEM-AREF ARG-LIST '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 0)
           '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 'WL-FFI::A)
            (LET* ((LENGTH (LENGTH INDICES))
                   (STRUCT (FOREIGN-ALLOC '(:STRUCT WL_ARRAY)))
                   (DATA (FOREIGN-ALLOC :UINT16 :COUNT LENGTH)))
              (LOOP FOR INDEX BELOW LENGTH
                    DO (SETF (MEM-AREF DATA :UINT16 INDEX)
                               (NTH INDEX INDICES)))
              (SETF (FOREIGN-SLOT-VALUE STRUCT '(:STRUCT WL_ARRAY)
                     'CL-WL.FFI::SIZE)
                      (* LENGTH (FOREIGN-TYPE-SIZE :UINT16))
                    (FOREIGN-SLOT-VALUE STRUCT '(:STRUCT WL_ARRAY)
                     'CL-WL.FFI::ALLOC)
                      LENGTH
                    (FOREIGN-SLOT-VALUE STRUCT '(:STRUCT WL_ARRAY)
                     'CL-WL.FFI::DATA)
                      DATA)
              STRUCT))
    (RESOURCE-POST-EVENT-ARRAY
     (ZWP_LINUX_DMABUF_FEEDBACK_V1-PTR DISPATCH) 5 ARG-LIST)))

(DEFMETHOD SEND-TRANCHE-FLAGS
           ((DISPATCH DISPATCH) FLAGS)
  (CL-WL::DEBUG-LOG! "Event: ~a~%" "tranche_flags")
  (LET ((ARG-LIST (FOREIGN-ALLOC '(:UNION WL_ARGUMENT) :COUNT 1)))
    (SETF (FOREIGN-SLOT-VALUE
           (MEM-AREF ARG-LIST '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 0)
           '(:UNION CL-WL.FFI:WL_ARGUMENT_OUTGOING) 'WL-FFI::U)
            (TRANCHE-FLAGS-TO-VALUE FLAGS))
    (RESOURCE-POST-EVENT-ARRAY
     (ZWP_LINUX_DMABUF_FEEDBACK_V1-PTR DISPATCH) 6 ARG-LIST)))

(DEFUN TRANCHE-FLAGS-FROM-VALUE (BITS)
  (LOOP FOR ENTRY IN '((1 :SCANOUT))
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

(DEFUN TRANCHE-FLAGS-TO-VALUE (KEYWORDS)
  (REDUCE #'+ KEYWORDS :KEY
          (LAMBDA (KEYWORD)
            (OR (CADR (ASSOC KEYWORD '((:SCANOUT 1)))) 0))))

(DEFCLASS GLOBAL (CL-WL:GLOBAL) NIL
          (:DEFAULT-INITARGS :VERSION 5 :DISPATCH-IMPL 'DISPATCH)
          (:DOCUMENTATION "dmabuf feedback

This object advertises dmabuf parameters feedback. This includes the
      preferred devices and the supported formats/modifiers.

      The parameters are sent once when this object is created and whenever they
      change. The done event is always sent once after all parameters have been
      sent. When a single parameter changes, all parameters are re-sent by the
      compositor.

      Compositors can re-send the parameters when the current client buffer
      allocations are sub-optimal. Compositors should not re-send the
      parameters if re-allocating the buffers would not result in a more optimal
      configuration. In particular, compositors should avoid sending the exact
      same parameters multiple times in a row.

      The tranche_target_device and tranche_formats events are grouped by
      tranches of preference. For each tranche, a tranche_target_device, one
      tranche_flags and one or more tranche_formats events are sent, followed
      by a tranche_done event finishing the list. The tranches are sent in
      descending order of preference. All formats and modifiers in the same
      tranche have the same preference.

      To send parameters, the compositor sends one main_device event, tranches
      (each consisting of one tranche_target_device event, one tranche_flags
      event, tranche_formats events and then a tranche_done event), then one
      done event.
"))

(DEFMETHOD DISPATCH-BIND ((GLOBAL GLOBAL) CLIENT DATA VERSION ID)
  "Default bind implementation for the zwp_linux_dmabuf_feedback_v1 global object.
This can be overriden by inheritance in case if custom behaviour is required."
  (CL-WL::DEBUG-LOG! "Binding ~a~%" "zwp_linux_dmabuf_feedback_v1")
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
                     "zwp_linux_dmabuf_feedback_v1")
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

