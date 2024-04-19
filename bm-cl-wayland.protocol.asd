
;; ██████╗ ██████╗  ██████╗ ████████╗ ██████╗  ██████╗ ██████╗ ██╗
;; ██╔══██╗██╔══██╗██╔═══██╗╚══██╔══╝██╔═══██╗██╔════╝██╔═══██╗██║
;; ██████╔╝██████╔╝██║   ██║   ██║   ██║   ██║██║     ██║   ██║██║
;; ██╔═══╝ ██╔══██╗██║   ██║   ██║   ██║   ██║██║     ██║   ██║██║
;; ██║     ██║  ██║╚██████╔╝   ██║   ╚██████╔╝╚██████╗╚██████╔╝███████╗
;; ╚═╝     ╚═╝  ╚═╝ ╚═════╝    ╚═╝    ╚═════╝  ╚═════╝ ╚═════╝ ╚══════╝
(asdf:defsystem #:bm-cl-wayland.protocol
  :serial t
  :description "Parser/Compiler for wayland xml files -> lisp"
  :author "bmiww <bmiww@bky.one>"
  :license "GPLv3"
  :version "0.0.1"
  :depends-on (#:xmls
	       #:str
	       #:split-sequence
	       #:bm-cl-wayland)
  :components ((:file "wl-protocol-parser")
	       (:file "wl-protocol-compiler")))
