(ASDF/PARSE-DEFSYSTEM:DEFSYSTEM #:CL-WL.WAYLAND-CORE
  :SERIAL
  T
  :LICENSE
  "GPLv3"
  :VERSION
  "0.0.1"
  :DEPENDS-ON
  (#:CFFI #:CL-WL #:CL-WL.FFI)
  :COMPONENTS
  ((:FILE "cl-wl.wayland-core")))

