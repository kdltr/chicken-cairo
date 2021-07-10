(module cairo.surface.svg
 ()

 (import scheme (chicken base) (chicken foreign))

 (include "types.scm")
 (foreign-declare "#include \"cairo.h\"")

 (cond-expand
  (CAIRO_HAS_SVG_SURFACE
   (foreign-declare "#include \"cairo-svg.h\""))
  (else))

 (guarded-defs
  CAIRO_HAS_SVG_SURFACE
  (surface svg-surface-create c-string double double))
)
