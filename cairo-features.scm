(import scheme)
(import (chicken base))
(import (chicken foreign))
(import feature-test)

#> #include "cairo.h" <#

(declaration-prefix "")
(registration-prefix "")

(print "(import scheme)")
(print "(import (chicken platform))")
(print)

(define-foreign-features
  CAIRO_HAS_PNG_FUNCTIONS
  CAIRO_HAS_PDF_SURFACE
  CAIRO_HAS_SVG_SURFACE
  CAIRO_HAS_XLIB_SURFACE)
