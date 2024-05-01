
;;  ██████╗██╗      ██╗    ██╗██╗        ███████╗███████╗██╗
;; ██╔════╝██║      ██║    ██║██║        ██╔════╝██╔════╝██║
;; ██║     ██║█████╗██║ █╗ ██║██║        █████╗  █████╗  ██║
;; ██║     ██║╚════╝██║███╗██║██║        ██╔══╝  ██╔══╝  ██║
;; ╚██████╗███████╗ ╚███╔███╔╝███████╗██╗██║     ██║     ██║
;;  ╚═════╝╚══════╝  ╚══╝╚══╝ ╚══════╝╚═╝╚═╝     ╚═╝     ╚═╝
(asdf:defsystem #:cl-wl.ffi
  :serial t
  :description "FFI declarations towards libwayland"
  :author "bmiww <bmiww@bky.one>"
  :license "GPLv3"
  :version "0.0.1"
  :depends-on (#:cffi)
  :components ((:file "libwayland-ffi")))
