(module cairo.svg
 ()

 (import scheme chicken foreign lolevel)
 (include "types.scm")
 (foreign-declare "#include \"cairo.h\"")
 (foreign-declare "#include \"cairo-svg.h\"")

(export svg-surface-create)
(define svg-surface-create
  (foreign-lambda surface "cairo_svg_surface_create"
                  c-string double double))

)