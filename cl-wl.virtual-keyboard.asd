(ASDF/PARSE-DEFSYSTEM:DEFSYSTEM #:CL-WL.VIRTUAL-KEYBOARD
  :SERIAL
  T
  :LICENSE
  "GPLv3"
  :VERSION
  "0.0.1"
  :DEPENDS-ON
  (#:CFFI #:CL-WL #:CL-WL.FFI #:CL-WL.WAYLAND-CORE)
  :COMPONENTS
  ((:FILE "cl-wl.virtual-keyboard")))

