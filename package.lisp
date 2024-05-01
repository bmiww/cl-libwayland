(defpackage #:bm-cl-wayland
  (:use #:cl #:cffi :wl-ffi)
  (:nicknames :wl)
  (:export display-create create-client *global-tracker* resource-get-id object get-client iface
	   get-data pop-data display create-resource reserve-data global-create version data-ptr set-data
	   global-get-name wl_message display-add-socket-fd display-run display-get-event-loop event-loop-get-fd
	   event-loop-dispatch display-flush-clients ptr debug-log! resource-set-dispatcher dispatch-impl
	   wl_resource *resource-tracker* wl_argument id client mk-if resource-post-event-array
	   init-interface-definitions interface-exists-test
	   name version method_count methods event_count events
	   signature types destroy wl_array *client-tracker* objects))
