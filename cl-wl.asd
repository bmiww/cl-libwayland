
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
	       #:cl-wl.ffi)
  :components ((:file "cl-wl")
	       (:file "util")))
