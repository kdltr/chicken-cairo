(module (cairo surface pdf)
  ()

(import scheme (chicken base) (chicken foreign))
(include "types.scm")
(foreign-declare "#include <cairo.h>")
(foreign-declare "#include <cairo-pdf.h>")

(defs
  (surface pdf-surface-create c-string double double))

)
