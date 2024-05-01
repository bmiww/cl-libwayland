
;; ██████╗ ██████╗  ██████╗ ████████╗ ██████╗  ██████╗ ██████╗ ██╗
;; ██╔══██╗██╔══██╗██╔═══██╗╚══██╔══╝██╔═══██╗██╔════╝██╔═══██╗██║
;; ██████╔╝██████╔╝██║   ██║   ██║   ██║   ██║██║     ██║   ██║██║
;; ██╔═══╝ ██╔══██╗██║   ██║   ██║   ██║   ██║██║     ██║   ██║██║
;; ██║     ██║  ██║╚██████╔╝   ██║   ╚██████╔╝╚██████╗╚██████╔╝███████╗
;; ╚═╝     ╚═╝  ╚═╝ ╚═════╝    ╚═╝    ╚═════╝  ╚═════╝ ╚═════╝ ╚══════╝
(asdf:defsystem #:cl-wl.protocol
  :serial t
  :description "Parser/Compiler for wayland xml files -> lisp"
  :author "bmiww <bmiww@bky.one>"
  :license "GPLv3"
  :version "0.0.1"
  :depends-on (#:xmls
	       #:str
	       #:split-sequence
	       #:alexandria
	       #:cl-wl
	       #:cl-wl.ffi)
  :components ((:file "parser")
	       (:file "compiler")))
