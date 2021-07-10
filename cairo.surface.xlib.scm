(module cairo.surface.xlib
 ()

(import scheme (chicken base) (chicken foreign))

(include "types.scm")
(foreign-declare "#include \"cairo.h\"")

(cond-expand
 (CAIRO_HAS_XLIB_SURFACE
  (foreign-declare "#include \"cairo-xlib.h\"")

  (define-foreign-type display (c-pointer "Display"))
  (define-foreign-type drawable unsigned-int32)
  (define-foreign-type visual (c-pointer "Visual")))
 (else))

(guarded-defs
 CAIRO_HAS_XLIB_SURFACE
 (surface xlib-surface-create display drawable visual int int)
 (void xlib-surface-set-size surface int int)
 (display xlib-surface-get-display surface))

)
