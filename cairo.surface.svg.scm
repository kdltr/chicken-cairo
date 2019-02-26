(module cairo.surface.svg
 ()

 (import scheme (chicken base) (chicken foreign))

 (include "types.scm")
 (foreign-declare "#include \"cairo.h\"")
 (foreign-declare "#include \"cairo-svg.h\"")

(defs
  (surface svg-surface-create c-string double double))
)