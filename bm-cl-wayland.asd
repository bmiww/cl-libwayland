
;; ██████╗ ███╗   ███╗       ██████╗██╗      ██╗    ██╗ █████╗ ██╗   ██╗██╗      █████╗ ███╗   ██╗██████╗
;; ██╔══██╗████╗ ████║      ██╔════╝██║      ██║    ██║██╔══██╗╚██╗ ██╔╝██║     ██╔══██╗████╗  ██║██╔══██╗
;; ██████╔╝██╔████╔██║█████╗██║     ██║█████╗██║ █╗ ██║███████║ ╚████╔╝ ██║     ███████║██╔██╗ ██║██║  ██║
;; ██╔══██╗██║╚██╔╝██║╚════╝██║     ██║╚════╝██║███╗██║██╔══██║  ╚██╔╝  ██║     ██╔══██║██║╚██╗██║██║  ██║
;; ██████╔╝██║ ╚═╝ ██║      ╚██████╗███████╗ ╚███╔███╔╝██║  ██║   ██║   ███████╗██║  ██║██║ ╚████║██████╔╝
;; ╚═════╝ ╚═╝     ╚═╝       ╚═════╝╚══════╝  ╚══╝╚══╝ ╚═╝  ╚═╝   ╚═╝   ╚══════╝╚═╝  ╚═╝╚═╝  ╚═══╝╚═════╝
(asdf:defsystem #:bm-cl-wayland
  :serial t
  :description "FFI and lispy interface to libwayland and wayland protocols"
  :author "bmiww <bmiww@bky.one>"
  :license "GPLv3"
  :version "0.0.1"
  :depends-on (#:str
	       #:cffi
	       ;; NOTE: Primarily for the define-c-callback macro
	       #:cl-async)
  :components ((:file "libwayland-ffi")
	       (:file "package")
	       (:file "bm-cl-wayland")))
