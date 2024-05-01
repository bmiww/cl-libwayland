
;;  ██████╗██╗      ██╗    ██╗██╗
;; ██╔════╝██║      ██║    ██║██║
;; ██║     ██║█████╗██║ █╗ ██║██║
;; ██║     ██║╚════╝██║███╗██║██║
;; ╚██████╗███████╗ ╚███╔███╔╝███████╗
;;  ╚═════╝╚══════╝  ╚══╝╚══╝ ╚══════╝
(asdf:defsystem #:cl-wl
  :serial t
  :description "Lispy interface to libwayland and wayland protocols"
  :author "bmiww <bmiww@bky.one>"
  :license "GPLv3"
  :version "0.0.1"
  :depends-on (#:str
	       #:cffi
	       #:alexandria
	       ;; NOTE: Primarily for the define-c-callback macro
	       #:cl-async
	       #:cl-wl.ffi)
  :components ((:file "cl-wl")))
