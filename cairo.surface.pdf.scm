(module (cairo surface pdf)
  ()

(import scheme (chicken base) (chicken foreign))
(include "types.scm")
(foreign-declare "#include <cairo.h>")

(cond-expand
 (CAIRO_HAS_PDF_SURFACE
  (foreign-declare "#include <cairo-pdf.h>"))
 (else))

(guarded-defs
 CAIRO_HAS_PDF_SURFACE
 (surface pdf-surface-create c-string double double))

)
